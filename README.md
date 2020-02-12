# pkgserver

<!-- badges: start -->
<!-- badges: end -->

THIS IS A WORK-IN-PROGRESS

Use `install()` for fast binary package installation. This 'front-end'
must connect with an archive server capable of building and providing
binary package versions. When the archive server is first asked for a
versioned package, it must install and build the binary of the package
so the process is no faster than a standard installation. On second
and subsequent requests, the archive server provides a binary package
for fast installation. Currently, the only archive server is in fact
the package itself, so it is only capable of serving binary package
versions to itself.

## Installation

Install from github with

```{r, eval = FALSE}
BiocManager::install("mtmorgan/pkgserver")
```

## Example

Ask to install package(s)

```{r, eval = FALSE}
pkgs <- c("ShortRead", "Rsamtools")
pkgserver::install(pkgs)
```

This will determine the packages and dependencies needed for
installation, and forward the request to the archive server. The
archive server will either discover already-available binary versions
of the required packages, or will build the binary versions. The
binary versions will be returned to the requesting session for fast
installation.

The archive server is no faster when the package and version are first
requested. Subsequent requests are fast. For instance, I have

```
> .libPaths()
[1] "/Users/ma38727/Library/R/4.0/Bioc/3.11/library"
[2] "/Users/ma38727/bin/R-devel/library"
```

where the last element of `.libPaths()` contains only base packages. Thus

```{r}
pkgs <- c("ShortRead", "Rsamtools")
lib <- tempfile()
dir.create(lib)
pkgserver::install(pkgs, c(lib, tail(.libPaths(), 1)))
```

installs `pkgs` and their dependendcies in a library `lib`. The first
time is slow, but the second time

```{r}
lib <- tempfile()
dir.create(lib)
pkgserver::install(pkgs, c(lib, tail(.libPaths(), 1)))
```

is faster (some binary packages, `BH` in the example here, are still
large and a little slow to install).


