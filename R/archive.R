.server <- new.env(parent = emptyenv())

#' @importFrom digest digest
.digest <-
    function(needed, platform, bioc_version)
{
    value <- Map(function(Package, Version) {
        value <- paste0(platform, bioc_version, Package, Version)
        unname(digest(value))
    }, needed$Package, needed$Version)
    as.character(unlist(value)) # including for 0-length arguments
}

#' @importFrom dplyr distinct
.archive_digest <-
    function(needed, platform, bioc_version)
{
    package_version <-
        select(needed, Package, Version) %>%
        distinct()
    mutate(
        package_version,
        digest = .digest(package_version, platform, bioc_version)
    )
}

#' @importFrom rappdirs user_cache_dir
.platform_cache_dir <-
    function(platform, bioc_version)
{
    cache_dir <- user_cache_dir("pkgserver")
    platform_dir <- digest(paste0(platform, bioc_version))
    file.path(cache_dir, platform_dir)
}

.cache_lib <-
    function(platform, bioc_version)
{
    lib <- file.path(.platform_cache_dir(platform, bioc_version), "lib")
    status <- dir.exists(lib) || dir.create(lib, recursive = TRUE)
    stopifnot("failed to create library cache" = status)
    lib
}

.cache_archive <-
    function(platform, bioc_version)
{
    archive <- file.path(.platform_cache_dir(platform, bioc_version), "download")
    status <- dir.exists(archive) || dir.create(archive, recursive = TRUE)
    stopifnot("failed to create package download directory" = status)
    archive
}

#' @importFrom dplyr anti_join
.archive_install_needed <-
    function(digest, lib)
{
    fields <- c("Package", "Version")
    installed <-
        installed.packages(lib)[, fields] %>%
        as_tibble()
    anti_join(digest, installed, by = c("Package", "Version"))
}

.archive_path <-
    function(digest, archive)
{
    paths <- dir(archive, full = TRUE)
    packages <- basename(paths)
    prefix <- paste(digest$Package, digest$Version, sep="_")
    which <- vapply(prefix, function(prefix, packages) {
        which(startsWith(packages, prefix))
    }, integer(1), packages)
    mutate(digest, archive = paths[which])
}

#' @importFrom BiocManager repositories version
archive_install <-
    function(digest, repos = repositories(), platform = archive_platform(),
             bioc_version = version())
{
    cache_lib <- .cache_lib(platform, bioc_version)
    digest <- .archive_digest(digest, platform, bioc_version)
    packages <- .archive_install_needed(digest, cache_lib)

    cache_archive <- .cache_archive(platform, bioc_version)
    owd <- setwd(cache_archive)
    on.exit(setwd(owd))

    install.packages(
        packages$Package, cache_lib, repos, INSTALL_opts = "--build"
    )

    .archive_path(digest, cache_archive)
}

archive_platform <-
    function()
{
    R.version$platform
}

archive_update <-
    function(repos = repositories(), platform = archive_platform(),
             bioc_version = version())
{
    cache_lib <- .cache_lib(platform, bioc_version)
    fields <- c("Package", "Version")
    packages <- old.packages(cache_lib, repos)[, fields]
    if (is.null(packages))
        packages <- matrix(
            character(), 0, 2, dimnames = list(NULL, c("Package", "Version"))
        )
    needed <- as_tibble(packages)
    digest <- .archive_digest(needed, platform, bioc_version)
    archive_install(digest, repos, platform, bioc_version)
}        
