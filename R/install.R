#' @importFrom utils available.packages installed.packages
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr filter select mutate left_join "%>%"
.packages <-
    function(lib_paths, repos)
{
    fields <- c("Package", "Version")
    available_packages <- available.packages(repos = repos)[, fields]
    installed_packages <- installed.packages(lib_paths)[, fields]

    tbl <-
        as_tibble(available_packages) %>%
        mutate(
            installed = Package %in% installed_packages[, "Package"],
            in_use = Package %in% loadedNamespaces()
        )

    out_of_date <-
        filter(tbl, installed) %>%
        mutate(
            out_of_date = Version != installed_packages[Package, "Version"]
        ) %>%
        select(Package, out_of_date)

    left_join(tbl, out_of_date, by = "Package") %>%
        mutate(out_of_date = ifelse(is.na(out_of_date), FALSE, out_of_date))
}

#' @importFrom tools package_dependencies
#' @importFrom dplyr bind_rows
.dependencies <-
    function(packages, repos)
{
    db <- available.packages(repos = repos)
    deps0 <- package_dependencies(packages, db, recursive = TRUE)
    all_pkgs <- unique(c(packages, unlist(deps0, use.names = FALSE)))
    db <- db[rownames(db) %in% all_pkgs, , drop = FALSE]
    deps <- package_dependencies(all_pkgs, db, recursive = TRUE)
    tbl <- tibble(
        Package = rep(names(deps), lengths(deps)),
        Needs = unlist(deps, use.names = FALSE)
    )
    ## all packages depend on themselves
    bind_rows(
        tbl,
        tibble(Package = all_pkgs, Needs = all_pkgs)
    )
}

#' @importFrom dplyr inner_join
.needed_packages <-
    function(packages, available, dependencies)
{
    updateable <-
        filter(
            available,
            !in_use & (!installed | out_of_date | Package %in% packages)
        ) %>%
        select(Package)

    inner_join(dependencies, updateable, by = c("Needs" = "Package")) %>%
        left_join(available, by = "Package") %>%
        select(Package, Version, Needs)
}

.install_packages <-
    function(needed, lib_paths, repos)
{
    archive <- archive_install(needed, repos = repos)
    packages <- archive$archive
    install.packages(packages, lib_paths[1], repos = NULL)
    tibble(Package = archive$Package, install_occurred = TRUE)
}

#' @importFrom BiocManager repositories
install_packages_for_builder <-
    function(packages, lib_paths = .libPaths(), repos = repositories())
{
    stop("not implemented")
}

#' @importFrom dplyr count arrange desc
install_packages_for_user <-
    function(packages, lib_paths = .libPaths(), repos = repositories())
{
    stopifnot(
        is.character(packages), !anyNA(packages),
        is.character(lib_paths), length(lib_paths) > 0, !anyNA(lib_paths),
        all(dir.exists(lib_paths)),
        is.character(repos), !anyNA(repos)
    )

    available <- .packages(lib_paths, repos)
    stopifnot(
        "not all 'packages' available in 'repos'" =
            all(packages %in% available[["Package"]])
    )

    dependencies <- .dependencies(packages, repos)

    unavailable <- inner_join(
        filter(available, in_use, out_of_date), dependencies, by = "Package"
    )
    if (nrow(unavailable))
        stop(
            "out-of-date dependencies are in use and cannot be updated: ",
            paste0(unique(unavailable$Package), collapse = " ")
        )

    needed <- .needed_packages(packages, available, dependencies)

    installed <- .install_packages(needed, lib_paths, repos)

    ## return value
    dependency_count <- count(dependencies, Package, name = "dependencies")
    dependency_count <- inner_join(
        dependency_count,
        select(available, Package, Version),
        by = "Package"
    )
    dependency_count %>%
        mutate(
            install_occurred = Package %in% installed$Package
        ) %>%
        select(Package, Version, dependencies, install_occurred) %>%
        arrange(desc(dependencies))
}

#' Install packages and their dependencies from CRAN-style repositories.
#'
#' @param packages character() vector of packages to install. Packages
#'     and all dependencies must be available in the repositories
#'     specified by `repos = `.
#'
#' @param lib_paths character() vector of (local) directory paths in
#'     which to install packages (the first position) and to find
#'     currently installed packages. `lib_paths` must have length at
#'     least 1.
#'
#' @param repos character() vector of paths (e.g., URLs) to
#'     'CRAN-style' repositories. The default includes CRAN and
#'     Bioconductor repositories relevant to the current R
#'     installation.
#'
#' @return a tibble of package, version, dependency count (including
#'     self), and logical value indicating whether installation
#'     occurred.
#'
#' @export
install <- install_packages_for_user
