# fdim

![R-CMD-check](https://github.com/pramitghosh/fdim/workflows/R-CMD-check/badge.svg) 
![pkgdown](https://github.com/pramitghosh/fdim/workflows/pkgdown/badge.svg) 
[![codecov](https://codecov.io/gh/pramitghosh/fdim/branch/master/graph/badge.svg)](https://codecov.io/gh/pramitghosh/fdim) 

Author: Pramit Ghosh

## Description

The package provides an implementation in R to calculate the box-counting dimension of a simple feature with geometry type POLYGON. The box-counting dimension, also known as the Minkowski-Bouligand dimension, estimates the fractal dimension of a set in Euclidean Space.

## Installation

### Dependencies

In order to install this package, the following packages are required as a pre-requisite.

- `sf`
- `graphics`
- `stats`

In addition, the following packages are also suggested. These are mostly required to knit the vignettes and run certain tests.

- `rnaturalearth`
- `rnaturalearthdata`
- `rgeos`

These can be installed by running the following command in R:

```r
install.packages(c("sf", "graphics", "stats", "rnaturalearth", "rnaturalearthdata", "rgeos"))
```
## Usage

The Box-Counting dimension can be calculated using `bcd()`. The following example illustrates the usage.

```r
library(rnaturalearth)
deutschland = ne_countries(scale = "medium", country = "Germany", returnclass = "sf")
bcd(deutschland, plot = TRUE)
```

A more detailed illustration along with the internal working can be found in the [vignettes](https://pramitghosh.github.io/fdim/articles/using_fdim.html).