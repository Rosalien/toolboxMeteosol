# toolboxMeteosol

<!-- badges: start -->
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

R-package for processing raw files from biometeorological station of [SNO-Tourbières](https://www.sno-tourbieres.cnrs.fr/)

## Installation

You can install the released version of toolboxMeteosol with:

```r
devtools::install_github("Rosalien/toolboxMeteosol")
```

## Example

Raw files need to respect format describe in the documentation [here](https://sourcesup.renater.fr/www/si-snot/3.1_Sce_meteosol.html). Examples of raw files are also available in [extdata folder](https://github.com/Rosalien/toolboxMeteosol/tree/master/inst/extdata).

```r
toolboxMeteosol::workflowMeteosolData(repFile=system.file("extdata", package = "toolboxMeteosol"),
				repTraitement="~/",
				repOut="~/",
				yearsToIntegrate="20",
				Site="lgt",
				Station="bm1")
```
