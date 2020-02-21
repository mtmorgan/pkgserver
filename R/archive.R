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
    vapply(key, digest, character(1), USE.NAMES = FALSE)
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

.directory_cache_dir <-
    function()
{
    cache_dir <- user_cache_dir("pkgserver")
    cache_dir <- getOption("PKGSERVER_CACHE_DIR", cache_dir)
    Sys.getenv("PKGSERVER_CACHE_DIR", cache_dir)
}

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
    cache_dir <- .directory_cache_dir()
    path <- file.path(cache_dir, directory)
    .directory_create(path, directory)
}

.directory_create_version_specific <-
    function(directory, platform, bioc_version)
{
    cache_dir <- .directory_cache_dir()
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
    function(tbl)
{
    archive <- .archive_directory_archive()
    tbl$digest %in% dir(archive)
}

#' @importFrom utils untar
.archive_package_version_from_binary <-
    function(source)
{
    fields <- c("Package", "Version")
    description_regex <- "^[^/]+/DESCRIPTION$"
    exdir <- tempfile()
    status <- dir.create(exdir)
    stopifnot(
        "failed to create temporary directory archiving binary package" = status
    )

    value <- vapply(source, function(source, exdir) {
        content <- untar(source, list = TRUE)
        description <- content[grepl(description_regex, content)]
        status <- untar(source, description, exdir = exdir)
        stopifnot("failed to untar binary source" = !status)
        read.dcf(file.path(exdir, description), fields)
    }, character(2), exdir, USE.NAMES = FALSE)

    tibble(source, Package = value[1,], Version = value[2,])
}

.archive_package_version_from_source <-
    function(source)
{
    fields <- c("Package", "Version")
    description <- file.path(source, "DESCRIPTION")
    stopifnot("unable to find DESCRIPTION files" = all(file.exists(description)))
    value <- vapply(description, function(description) {
        read.dcf(description, fields)
    }, character(2), USE.NAMES = FALSE)

    tibble(source, Package = value[1,], Version = value[2,])
}

.archive_move_binary_to_archive <-
    function(tbl, binary, archive)
{
    ## map file to package & version
    pkgversion <- .archive_package_version_from_binary(binary)
    tbl0 <- left_join(tbl, pkgversion, by = c("Package", "Version"))

    ## move to archive
    from <- tbl0$source
    to <- file.path(archive, tbl0$digest)
    !anyNA(from) ||
        .warning(sum(is.na(from)), " built binary packages cannot be archived")
    idx <- !is.na(from)
    status <- file.rename(from[idx], to[idx])
    all(status) ||
        .warning(sum(!status), " built binary packages not copied to archive")

    ## return
    tbl %>% mutate(is_archived = .archive_is_package_archived(.))
}

##
## client (not end-user) access
##

#' @rdname archive
#'
#' @title Manage package archives.
#'
#' @description `archive_platform()` provide a platform description
#'     for the session in which this package is running.
#'
#' @return `archive_platform()` returns character(1) description of
#'     the platform.
#'
#' @export
archive_platform <-
    function()
{
    R.version$platform
}

#' @rdname archive
#'
#' @description `archive_statistics()` queries the archive for
#'     information about disk usage, available platforms, and archived
#'     packages.
#'
#' @param platform `character(1)` description of the platform for
#'     which packages are being archived. The default is the platform
#'     on which the function is being evaluated.
#'
#' @param bioc_version Bioconductor version for which the archive
#'     applies. Default is from `BiocManager::version()`.
#'
#' @return `archive_statistics()` returns an S3 list-based object with
#'     package statistics, visuallized using `print()`.
#'
#' @importFrom BiocManager version
#'
#' @export
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
            "  %s%6d\n",
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

#' @rdname archive
#'
#' @description `archive_need()` identifies which packages have been
#'     archived.
#'
#' @param tbl a `tibble()` with character columns `Package` and
#'     `Version`.
#'
#' @return `archive_need()` returns a tibble with of Package, Version,
#'     and digest of all packages needing to be archived.
#'
#' @export
archive_need <-
    function(tbl, platform = archive_platform(), bioc_version = version())
{
    archive <- .archive_directory_archive()

    tbl %>%
        mutate(digest = .digest(., platform, bioc_version)) %>%
        mutate(is_archived = .archive_is_package_archived(.)) %>%
        filter(!.$is_archived) %>%
        select(-.$is_archived)
}

#' @rdname archive
#'
#' @description `archive_install()` builds, installs, and archives
#'     packages in `tbl` that are not already archived.
#'
#' @param repos `character()` of CRAN-style repositories where
#'     packages sources are to be retrieved from.
#'
#' @param verbose `logical(1)` request for additional progress
#'     indications, including from `install.packages()`.
#'
#' @return `archive_install()` returns a tbl containing Package,
#'     Version, digest, and archive path suitable for use in binary
#'     installation via `install.packages()`.
#'
#' @importFrom utils install.packages old.packages
#'
#' @importFrom BiocManager repositories
#'
#' @importFrom dplyr pull
#'
#' @export
archive_install <-
    function(tbl, platform = archive_platform(), bioc_version = version(),
             repos = repositories(), verbose = getOption("verbose"))
{
    src <- .archive_directory_source()
    library <- .archive_directory_library(platform, bioc_version)
    binary <- .archive_directory_binary(platform, bioc_version)
    archive <- .archive_directory_archive()

    verbose && .message(nrow(tbl), " candidate packages to achive")
    tbl <- tbl %>%
        mutate(digest = .digest(., platform, bioc_version)) %>%
        mutate(is_archived = .archive_is_package_archived(.))

    ## build necessary binaries
    owd <- setwd(binary)
    on.exit(setwd(owd))

    to_archive <- tbl %>% filter(!.$is_archived)
    verbose && .message(nrow(to_archive), " versioned packages not in archive")
    package <- to_archive %>% pull("Package")

    install.packages(
        package, library, repos, destdir = src, INSTALL_opts = "--build",
        verbose = verbose, quiet = TRUE
    )
    binaries <- dir(binary, full.names = TRUE)
    to_archive <- .archive_move_binary_to_archive(to_archive, binaries, archive)

    ## return
    tbl %>% mutate(archive = file.path(archive, digest))
}

#' @rdname archive
#'
#' @description `archive_update()` updates all archived packages to
#'     their current version.
#'
#' @return `archive_update()` returns a tibble of newly archived
#'     packages with the same structure as `archive_install()`.
#'
#' @export
archive_update <-
    function(platform = archive_platform(), bioc_version = version(),
             repos = repositories(), verbose = getOption("verbose"))
{
    library <- .archive_directory_library(platform, bioc_version)
    tbl <-
        .package_available(library, repos) %>%
        filter(.$installed, .$needs_update)

    archive_install(tbl, platform, bioc_version, repos, verbose = verbose) %>%
        select(.$Package, .$Version, .$digest) %>%
        mutate(is_archived = .archive_is_package_archived(.))
}

.archive_join_and_check_pkgversion <-
    function(tbl, pkgversion, by, force)
{
    archive <- .archive_directory_archive()

    tbl <-
        left_join(tbl, pkgversion, by = by) %>%
        mutate(archive_path = file.path(.$archive, .$digest)) %>%
        mutate(is_archived = .archive_is_package_archived(.))
    status <- tbl$is_archived
    (!force && any(status)) &&
        .message(sum(status), " sources already archived; see 'force ='")

    tbl
}

.archive_add_compressed <-
    function(tbl, platform, bioc_version, force)
{
    tbl <- tbl %>% filter(.$compressed)

    pkgversion <-
        .archive_package_version_from_binary(tbl$source) %>%
        mutate(digest = .digest(., platform, bioc_version))
    tbl <- .archive_join_and_check_pkgversion(tbl, pkgversion, "source", force)

    tbl_cp <- tbl %>% filter(tbl, .$force | !.$is_archived)
    message(nrow(tbl_cp), " to be added from compressed binaries")

    status <- file.copy(tbl_cp$source, tbl_cp$archive_path)
    all(status) ||
        .warning(sum(!status), " compressed binaries not copied to archive")

    tbl
}

#' @importFrom utils tar txtProgressBar setTxtProgressBar
.archive_add_directories <-
    function(tbl, platform, bioc_version, force)
{
    tbl <- tbl %>% filter(!.$compressed)

    pkgversion <-
        .archive_package_version_from_source(tbl$source) %>%
        mutate(digest = .digest(., platform, bioc_version))
    tbl <- .archive_join_and_check_pkgversion(tbl, pkgversion, "source", force)

    tbl_tar <- tbl %>% filter(.$force | !.$is_archived)
    message(nrow(tbl_tar), " to be added from directories")

    owd <- getwd()
    pb <- txtProgressBar(0, max(1L, nrow(tbl_tar)), style = 3L)
    i <- 0L
    on.exit({
        close(pb)
        setwd(owd)
    })
    status <- Map(
        function(archive_path, source, ...) {
            i <<- i + 1L
            setTxtProgressBar(pb, i)
            setwd(dirname(source)) # shorten file path
            tar(archive_path, basename(source), ...)
        },
        tbl_tar$archive_path, tbl_tar$source,
        MoreArgs = list(compression = "gzip", compression_level = 9L)
    )
    ## FIXME: not sure how status can be used for validation...

    tbl
}

#' @rdname archive
#'
#' @description `archive_add()` allows packages to be archived without
#'     installation, e.g., when the archiver is not running on the
#'     target platform.
#'
#' @param source `character()` path to local file system built
#'     tarballs of packages to be archived, or directories of built
#'     packages (e.g., the installation library path of the building
#'     platform).
#'
#' @param force `logical(1)` force update of identical archives.
#'
#' @return `archive_add()` returns a tibble with Package, Version,
#'     digest, and `is_archived` flag indicating success at archiving
#'     the package.
#'
#' @importFrom dplyr bind_rows
#'
#' @export
archive_add <-
    function(source, platform = archive_platform(), bioc_version = version(),
             force = FALSE)
{
    stopifnot(all(file.exists(source)))

    tbl <- tibble(source = source, compressed = !dir.exists(source))

    tbl0 <- .archive_add_directories(tbl, platform, bioc_version, force)
    tbl1 <- .archive_add_compressed(tbl, platform, bioc_version, force)

    left_join(tbl, bind_rows(tbl0, tbl1), by = "source") %>%
        select(.$Package, .$Version, .$digest, .$source, .$is_archived)
}
