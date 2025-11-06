
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PITmodelR <img src="man/figures/pitmodelr_logo.png" align="right" width="110">

------------------------------------------------------------------------

**PITmodelR** is an R package for retrieving, organizing, and modeling
Passive Integrated Transponder (PIT) tag mark and observation data from
[PTAGIS](https://www.ptagis.org/). It provides tools to streamline
compiling tag data, building capture histories, and estimating survival
and migration timing metrics. Although designed with juvenile salmonids
in mind, many tools are broadly applicable to other PIT-tagged fish
populations.

***Note: `PITmodelR` is currently in development. Stay tuned for stable
releases.***

------------------------------------------------------------------------

## Features

- **Download PIT-tag data** directly from PTAGIS APIs.
- **Data wrangling helpers** to clean and structure detection histories.
- **Survival models** for juvenile migration through hydrosystem
  reaches.
- **Migration timing models** (arrival distributions, quantiles,
  cumulative passage).
- **Visualization tools** for survival curves, detection histories, and
  timing plots.
- Integration with **R modeling frameworks** (`PITcleanR`, `Rmark`,
  `mgcv`, etc.).

------------------------------------------------------------------------

## Installation

The `PITmodelR` package can be installed as an R package from GitHub
using the `remotes` package:

``` r
install.packages("remotes")
remotes::install_github("ryankinzer/PITmodelR")
```

The user may optionally add `build_vignettes = TRUE` within the
`install_github()` function to build available vignettes (i.e.,
tutorials) included with the package. Further instructions to use
`PITmodelR` can be found there, which can be accessed using e.g.,:

``` r
# see available vignettes
browseVignettes(package = "PITmodelR")

# to view the getting-started vignette
vignette("getting-started", package = "PITmodelR")
```

------------------------------------------------------------------------

## Authors

- Ryan Kinzer (Nez Perce Tribe - DFRM, Research Division)
- Mike Ackerman (Nez Perce Tribe - DFRM, Research Division)

------------------------------------------------------------------------

## Questions?

Please feel free to post an issue to this repository for requested
features, bug fixes, errors in documentation, etc.
