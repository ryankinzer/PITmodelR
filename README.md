# PITmodelR <img src="man/figures/logo.png" align="right" height="150"/>

**PITmodelR** is an R package for downloading and modeling Passive Integrated Transponder (PIT) tag data from [PTAGIS](https://www.ptagis.org/). It provides tools to streamline survival analysis, migration timing estimation, and visualization for juvenile salmonids and other tagged fish populations.

---

## Features

- **Download PIT-tag data** directly from PTAGIS APIs.
- **Data wrangling helpers** to clean and structure detection histories.
- **Survival models** for juvenile migration through hydrosystem reaches.
- **Migration timing models** (arrival distributions, quantiles, cumulative passage).
- **Visualization tools** for survival curves, detection histories, and timing plots.
- Integration with **R modeling frameworks** (`PITcleanR`, `Rmark`, `mgcv`, etc.).

---

## Installation

```r
# Install development version from GitHub\
remotes::install_github("ryankinzer/PITmodelR")
