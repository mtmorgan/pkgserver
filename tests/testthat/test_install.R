context("packages")

test_that(".package_available() works", {

    lib_paths <- repos <- character(1)
    
    available <- cbind(
        Package = c("A", "B", "C"),
        Version = c("1.1", "1.2", "1.3")
    ) %>% as_tibble()

    ## no installed packages
    installed <- cbind(Package = character(), Version = character())
    object <- with_mock(
        available.packages = function(...) available,
        installed.packages = function(...) installed,
        .package_available(lib_paths, repos)
    )
    expect_identical(select(object, colnames(available)), available)
    expect_identical(object$installed, logical(3))
    expect_identical(object$needs_update, !logical(3))

    ## installed package current
    installed <- cbind(Package = "B", Version = "1.2")
    object <- with_mock(
        available.packages = function(...) available,
        installed.packages = function(...) installed,
        .package_available(lib_paths, repos)
    )
    expect_identical(select(object, colnames(available)), available)
    expect_identical(object$installed, c(FALSE, TRUE, FALSE))
    expect_identical(object$needs_update, c(TRUE, FALSE, TRUE))

    ## installed package out-of-date
    installed <- cbind(Package = "B", Version = "1.0")
    object <- with_mock(
        available.packages = function(...) available,
        installed.packages = function(...) installed,
       .package_available(lib_paths, repos)
    )
    expect_identical(select(object, colnames(available)), available)
    expect_identical(object$installed, c(FALSE, TRUE, FALSE))
    expect_identical(object$needs_update, !logical(3))

})

test_that(".package_available() assigns in_use", {})

test_that(".package_dependencies() works", {})

test_that(".package_needed() works", {})
