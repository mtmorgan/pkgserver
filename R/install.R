#' @importFrom utils available.packages installed.packages
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr filter select mutate left_join "%>%"
.package_available <-
    function(lib_paths, repos)
{
    fields <- c("Package", "Version")
    available_packages <-
        available.packages(repos = repos)[, fields, drop = FALSE] %>%
        as_tibble()

    installed_packages <-
        installed.packages(lib_paths)[, fields, drop = FALSE] %>%
        as_tibble()

    idx <- match(available_packages$Package, installed_packages$Package)
    update_available <-
        available_packages$Version != installed_packages$Version[idx]

    available_packages %>%
        mutate(
            installed = .$Package %in% installed_packages$Package,
            in_use = .$Package %in% loadedNamespaces(),
            update_available = ifelse(
                is.na(update_available), TRUE, update_available
            )
        )
}

#' @importFrom tools package_dependencies
#' @importFrom dplyr distinct
.package_dependencies <-
    function(packages, repos, complete = FALSE)
{
    db <- available.packages(repos = repos)
    deps <- package_dependencies(packages, db, recursive = TRUE)
    if (complete) {
        all_pkgs <- unique(c(packages, unlist(deps, use.names = FALSE)))
        db <- db[rownames(db) %in% all_pkgs, , drop = FALSE]
        deps <- package_dependencies(all_pkgs, db, recursive = TRUE)
    }
    tibble(
        ## all packages depend on themselves
        Package = c(packages, rep(names(deps), lengths(deps))),
        Needs = c(packages, unlist(deps, use.names = FALSE))
    ) %>% distinct()
}

#' @importFrom dplyr inner_join
.package_need <-
    function(available, depends)
{
    inner_join(
        filter(.$available, .$update_available),
        select("depends", "Needs"),
        by = c(Package = "Needs")
    ) %>% distinct()
}

.install_packages <-
    function(need, lib_paths, repos)
{
    tbl <- need %>% select("Package", "Version")
    archive <- archive_install(tbl, repos = repos)
    packages <- archive$archive
    install.packages(packages, lib_paths[1], repos = NULL)

    mutate(need, install_occurred = .$Package %in% archive$Package)
}

#' @importFrom BiocManager repositories
install_packages_for_builder <-
    function(packages, lib_paths = .libPaths(), repos = repositories())
{
    stop("not implemented")
}

#' @importFrom dplyr count arrange desc
install_packages_for_user <-
    function(packages, lib_paths = .libPaths(), repos = repositories(),
             dry.run = FALSE)
{
    stopifnot(
        is.character(packages), !anyNA(packages),
        is.character(lib_paths), length(lib_paths) > 0, !anyNA(lib_paths),
        all(dir.exists(lib_paths)),
        is.character(repos), !anyNA(repos)
    )

    ## find packages for installation
    available <- .package_available(lib_paths, repos)
    stopifnot(
        "not all 'packages' available in 'repos'" =
            all(packages %in% available$Package)
    )
    message(
        nrow(available), " packages available from ",
        length(repos), " repositories"
    )

    depend <- .package_dependencies(packages, repos)
    message(length(packages), " packages have ", nrow(depend), " dependencies")

    need <- .package_need(available, depend)

    in_use <- filter(.$need, .$in_use, .$installed)
    if (nrow(in_use))
        warning(
            "out-of-date dependencies are in use and may fail to update:\n",
            "  ", paste0(unique(in_use$Package), collapse = " "),
            call. = FALSE, immediate. = TRUE
        )

    archive_need <- archive_need(need)
    message(
        nrow(archive_need), " dependencies need archive (full) installation\n",
        nrow(need), " dependencies need local (fast) installation"
    )

    ## install
    if (dry.run)
        return(need)

    .install_packages(need, lib_paths, repos)

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
#' @param dry.run logical(1) perform the installation (default,
#'     `TRUE`) or only the calculation of packages requiring update.
#'
#' @return a tibble of package, version, dependency count (including
#'     self), and logical value indicating whether installation
#'     occurred.
#'
#' @export
install <- install_packages_for_user
