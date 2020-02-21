context("digest")

test_that("'.digest()' vectorizes", {
    platform <- R.version$platform
    bioc_version <- package_version("3.11")

    ## 0-length
    tbl <- tibble(Package = character(), Version = character())
    object <- .digest(tbl, platform, bioc_version)
    expect_type(object, "character")
    expect_length(object, 0L)
    expect_null(names(object))

    ## n-length
    tbl <- tibble(
        Package = c("A", "B"),
        Version = c("1.0", "2.0")
    )
    object <- .digest(tbl, platform, bioc_version)
    expect_type(object, "character")
    expect_length(object, 2L)
    expect_null(names(object))

})

test_that("'.digest()' works with mutate()", {
    platform <- R.version$platform
    bioc_version <- package_version("3.11")

    ## 0-length
    tbl <- tibble(Package = character(), Version = character())
    object <- mutate(tbl, digest = .digest(tbl, platform, bioc_version))
    expect_s3_class(object, class(tbl))
    expect_identical(nrow(object), nrow(tbl))
    expect_identical(colnames(object), c("Package", "Version", "digest"))

    ## n-length
    tbl <- tibble(
        Package = c("A", "B"),
        Version = c("1.0", "2.0")
    )
    object <- mutate(tbl, digest = .digest(tbl, platform, bioc_version))
    expect_s3_class(object, class(tbl))
    expect_identical(nrow(object), nrow(tbl))
    expect_identical(colnames(object), c("Package", "Version", "digest"))

})

context("directory")

test_that("'.directory_*()' create appropriate directories", {
    platform <- R.version$platform
    bioc_version <- package_version("3.11")

    ##
    ## '.archive_directory_library()': version-specific installed
    ## package library
    ##

    ## single path
    object <- .archive_directory_library(platform, bioc_version)
    expect_type(object, "character")
    expect_length(object, 1L)
    expect_true(dir.exists(object))
    expect_true(endsWith(dirname(object), file.path("pkgserver", "library")))

    ## constant across invocations
    object1 <- .archive_directory_library(platform, bioc_version)
    object2 <- .archive_directory_library(platform, bioc_version)
    expect_identical(object1, object2)

    ## different with different versions / platforms
    object1 <- .archive_directory_library(platform, package_version("3.11"))
    object2 <- .archive_directory_library(platform, package_version("3.10"))
    expect_identical(dirname(object1), dirname(object2))
    expect_false(identical(basename(object1), basename(object2)))

    ## FIXME: test for failure

    ##
    ## '.archive_directory_binary()': (temporary) version-specific
    ## binary directory. Same behavior / implementation as
    ## .archive_directory_library

    object <- .archive_directory_binary(platform, bioc_version)
    expect_type(object, "character")
    expect_length(object, 1L)
    expect_true(dir.exists(object))
    expect_true(endsWith(dirname(object), file.path("pkgserver", "binary")))

    object1 <- .archive_directory_library(platform, bioc_version)
    object2 <- .archive_directory_binary(platform, bioc_version)
    expect_identical(basename(object1), basename(object2))

    ##
    ## '.archive_directory_source()': global archive of source tarballs
    ##

    object <- .archive_directory_source()
    expect_type(object, "character")
    expect_length(object, 1L)
    expect_true(dir.exists(object))
    expect_true(endsWith(object, file.path("pkgserver", "source")))

    ##
    ## '.archive_directory_archive()': global archive of binaries
    ##

    object <- .archive_directory_archive()
    expect_type(object, "character")
    expect_length(object, 1L)
    expect_true(dir.exists(object))
    expect_true(endsWith(object, file.path("pkgserver", "archive")))

})

context("archive")

test_that(".archive_is_package_archived() works", {

    ## 0, 2, or 2 w/ 1 version mismatch packages
    tbl0 <- tibble(Package = character(), Version = character())
    tbl2a <- tibble(Package = c("A", "C"), Version = c("1.1", "1.3"))
    tbl2b <- tibble(Package = c("A", "C"), Version = c("1.1", "1.4"))

    ## 0 or 3 packages installed
    lib0 <- cbind(Package = character(), Version = character())
    lib3 <- cbind(Package = c("A", "B", "C"), Version = c("1.1", "1.2", "1.3"))

    ## .archive_is_package_built
    platform <- R.version$platform
    bioc_version <- "3.11"

    ## 0 or 3 packages built
    dir0 <- .digest(as_tibble(lib0), platform, bioc_version)
    dir3 <- .digest(as_tibble(lib3), platform, bioc_version)

    dig0 <- tbl0 %>% mutate(digest = .digest(., platform, bioc_version))
    dig2a <- tbl2a %>% mutate(digest = .digest(., platform, bioc_version))
    dig2b <- tbl2b %>% mutate(digest = .digest(., platform, bioc_version))

    with_mock(dir = function(...) dir0, {
        object <- .archive_is_package_archived(dig0)
        expect_identical(object, logical())
        object <- .archive_is_package_archived(dig2a)
        expect_identical(object, logical(2))
        object <- .archive_is_package_archived(dig2b)
        expect_identical(object, logical(2))
    })

    with_mock(dir = function(...) dir3, {
        object <- .archive_is_package_archived(dig0)
        expect_identical(object, logical())
        object <- .archive_is_package_archived(dig2a)
        expect_identical(object, !logical(2))
        object <- .archive_is_package_archived(dig2b)
        expect_identical(object, c(TRUE, FALSE))
    })

})

test_that(".archive_move_binary_to_archive() works", {

    skip_if_not(interactive()) # can't get mock to find non-exported functions

    platform <- R.version$platform
    bioc_version <- "3.11"

    ## 0 packages
    tbl0 <-
        tibble(
            source = character(), Package = character(), Version = character()
        ) %>%
        mutate(digest = .digest(., platform, bioc_version))
    binary <- tempfile(); dir.create(binary)
    fls <- file.path(binary, sprintf("%s_%s.tar.gz", tbl0$Package, tbl0$Version))
    status <- file.create(fls)
    archive <- tempfile(); status <- dir.create(archive)
    with_mock(
        .archive_package_version_from_binary = function(...) {
            select(tbl0, Package, Version)
        },
        tbl <- .archive_move_binary_to_archive(tbl0, binary, archive)
    )
    expect_true(setequal(dir(archive), tbl0$digest))
    expect_true(all(tbl$is_archived))

    ## 2 packages
    binary <- tempfile(); dir.create(binary)
    source <- file.path(
        binary,
        sprintf("%s_%s.tar.gz", tbl2$Package, tbl2$Version)
    )
    status <- file.create(source)
    tbl2 <-
        tibble(
            source = source,
            Package = c("A", "C"), Version = c("1.1", "1.3")
        ) %>%
        mutate(digest = .digest(., platform, bioc_version))
    archive <- tempfile(); status <- dir.create(archive)
    with_mock(
        .archive_package_version_from_binary = function(...) {
            select(tbl2, Package, Version)
        },
        file.rename = function(from, ...) !logical(length(from)),
        .archive_is_package_archived = function(tbl, ...) !logical(nrow(tbl)),
        tbl <- .archive_move_binary_to_archive(tbl2, binary, archive)
    )
    expect_true(all(tbl$is_archived))

})

test_that("archive_install() works", {

    platform <- R.version$platform
    bioc_version <- "3.11"

    ## 0 packages
    tbl <-
        tibble(Package = character(), Version = character()) %>%
        mutate(digest = .digest(., platform, bioc_version))
    object <- archive_install(tbl)
    expect_s3_class(object, class(tbl))
    expect_identical(nrow(object), nrow(tbl))
    expect_identical(
        colnames(object),
        c("Package", "Version", "digest", "is_archived", "archive")
    )

    ## FIXME: non-trivial test

})
