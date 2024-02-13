# corplot
Fast correlation/corner/pairs plots for lots of data


## License

[![License: CC BY 4.0](https://img.shields.io/badge/License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

## Installation

This can be installed using:

```R
devtools::install_github('petedodd/corplot')
```

## Usage ##

Once installed, it provides once simple function:

```R
library(corplot)
corplot(matrix(rnorm(3e4),ncol=3),labels=c('x','y','z'),main='3D isotropic Gaussian')
```
