[![rcmdcheck](https://github.com/cffdrs/cffdrs_r/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/cffdrs/cffdrs_r/actions/workflows/check-standard.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/cffdrs)](https://cran.r-project.org/package=cffdrs)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/cffdrs)](https://www.r-pkg.org/pkg/cffdrs)
[![r-downloads](https://flat.badgen.net/cran/v/cffdrs)](https://CRAN.R-project.org/package=cffdrs)
[![r-version](https://flat.badgen.net/cran/r/cffdrs)](https://CRAN.R-project.org/package=cffdrs)
[![r-dls](https://flat.badgen.net/cran/dt/cffdrs)](https://CRAN.R-project.org/package=cffdrs)


# cffdrs
This project provides a group of new functions to calculate the outputs of the two main components of the [Canadian Forest Fire Danger Rating System (CFFDRS) Van Wagner and Pickett (1985)](https://cfs.nrcan.gc.ca/publications?id=19973) at various time scales: the [Fire Weather Index (FWI) System Wan Wagner (1985)](https://cfs.nrcan.gc.ca/publications?id=19927) and the [Fire Behaviour Prediction (FBP) System Forestry Canada Fire Danger Group (1992)](https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/10068.pdf). Some functions have two versions, table and raster based.

## Installation

`cffdrs` is available from CRAN, so you can use `install.packages("cffdrs")` to get the current *released version*.

#### From Source on Windows

You are able to install directly from the remote.

```
remotes::install_github("cffdrs/cffdrs_r")
```
