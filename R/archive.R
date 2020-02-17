##
## standard digest for each package + version + platform + bioc_version
##

#' @importFrom digest digest
.digest <-
    function(tbl, platform, bioc_version)
{
    key <- sprintf( # sprintf() handles 0-row tbl as desired, paste() does not
        "%s-%s-%s-%s",
        platform, bioc_version, tbl$Package, tbl$Version
    )
    value <- vapply(key, digest, character(1))
    unname(value)
}

##
## directories for archive
##
## .../pkgserver/library/<platform-version>/: platform and
##     version-specific installed libraries
##
## .../pkgserver/binary/<platform-version>/: (temporary) platform and
##     version-specific built binaries; moved to archive after
##     successful build
##
## .../pkgserver/source/: source tarballs
##
## .../pkgserver/archive/: built binaries
##
#' @importFrom rappdirs user_cache_dir

.directory_create <-
    function(path, directory)
{
    status <- dir.exists(path) || dir.create(path, recursive = TRUE)
    if (!status)
        stop("failed to create archive directory '", directory, "'")
    path
}

.directory_create_global <-
    function(directory)
{
    cache_dir <- user_cache_dir("pkgserver")
    path <- file.path(cache_dir, directory)
    .directory_create(path, directory)
}

.directory_create_version_specific <-
    function(directory, platform, bioc_version)
{
    cache_dir <- user_cache_dir("pkgserver")
    platform_dir <- digest(paste(platform, bioc_version, sep="-"))
    path <- file.path(cache_dir, directory, platform_dir)
    .directory_create(path, directory)
}

## .../pkgserver/library/<platform-version>
.archive_directory_library <-
    function(platform, bioc_version)
{
    .directory_create_version_specific("library", platform, bioc_version)
}

## .../pkgserver/binary/<platform-version>
.archive_directory_binary <-
    function(platform, bioc_version)
{
    .directory_create_version_specific("binary", platform, bioc_version)
}
## .../pkgserver/source
.archive_directory_source <-
    function()
{
    .directory_create_global("source")
}

## .../pkgserver/archive
.archive_directory_archive <-
    function()
{
    .directory_create_global("archive")
}

##
## tbl transformations
##

.archive_is_package_archived <-
    function(tbl, archive)
{
    tbl$digest %in% dir(archive)
}

.archive_move_binary_to_archive <-
    function(tbl, binary, archive)
{
    from <- dir(binary, full.names = TRUE)
    packages <- basename(from)

    ## map file to package & version
    prefix <- paste(tbl$Package, tbl$Version, sep="_")
    which <- vapply(prefix, function(prefix, packages) {
        which(startsWith(packages, prefix))
    }, integer(1), packages)
    from <- from[which]

    ## move to archive
    to <- file.path(archive, tbl$digest)
    status <- file.rename(from, to)
    stopifnot(
        "'archive_install()' failed to archive built packages" = all(status)
    )

    ## return
    tbl %>% mutate(is_archived = .archive_is_package_archived(., archive))
}

##
## client (not end-user) access
##

archive_platform <-
    function()
{
    R.version$platform
}


archive_statistics <-
    function(platform = archive_platform(), bioc_version = version())
{
    src <- .archive_directory_source()
    archive <- .archive_directory_archive()
    libraries <- .directory_create_global("library")
    archives <- dir(archive, full.names = TRUE)

    n_installed_packages <- tibble(
        path =  dir(libraries, full.names = TRUE)
    ) %>% mutate(
        platform = basename(.$path),
        n = lengths(lapply(.$path, dir))
    )
    archive_size <- sum(file.size(
        dir(.directory_create_global(""), recursive = TRUE, full.names = TRUE)
    ))

    structure(
        list(
            archive_size = structure(archive_size, class = "object_size"),
            n_installed_packages = n_installed_packages,
            n_src = length(dir(src)),
            n_archive = length(archives),
            archive_date_range = range(file.mtime(archives))
        ),
        class = "archive_statistics"
    )
}

#' @export
print.archive_statistics <-
    function(x, ...)
{
    cat(
        "class: ", class(x), "\n",
        format(x$archive_size, units="auto"), " disk space used\n",
        nrow(x$n_installed_packages), " platforms; installed packages:\n",
        sprintf(
            "  %s%4d\n",
            x$n_installed_packages$platform,
            x$n_installed_packages$n
        ),
        x$n_src, " source packages\n",
        x$n_archive, " archived packages\n",
        "  oldest: ", format(min(x$archive_date_range)), "\n",
        "  newest: ", format(max(x$archive_date_range)), "\n",
        sep = ""
    )
}

archive_need <-
    function(tbl, platform = archive_platform(), bioc_version = version())
{
    archive <- .archive_directory_archive()

    tbl %>%
        mutate(digest = .digest(., platform, bioc_version)) %>%
        mutate(is_archived = .archive_is_package_archived(., archive)) %>%
        filter(!.$is_archived)
}

#' @importFrom utils install.packages old.packages
#' @importFrom BiocManager repositories version
#' @importFrom dplyr pull
archive_install <-
    function(tbl, platform = archive_platform(), bioc_version = version(),
             repos = repositories())
{
    src <- .archive_directory_source()
    library <- .archive_directory_library(platform, bioc_version)
    binary <- .archive_directory_binary(platform, bioc_version)
    archive <- .archive_directory_archive()

    tbl <- tbl %>%
        mutate(digest = .digest(., platform, bioc_version)) %>%
        mutate(is_archived = .archive_is_package_archived(., archive))

    ## build necessary binaries
    owd <- setwd(binary)
    on.exit(setwd(owd))

    to_archive <- filter(tbl, !.$is_archived)
    package <- to_archive %>% pull("Package")
    message(length(package), " package(s) need archive installation")

    install.packages(
        package, library, repos, destdir = src, INSTALL_opts = "--build",
        verbose = FALSE, quiet = TRUE
    )
    to_archive <- .archive_move_binary_to_archive(to_archive, binary, archive)

    ## return
    mutate(tbl, archive = file.path(archive, digest))
}
