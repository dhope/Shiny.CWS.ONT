
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Shiny.CWS.ONT

<!-- badges: start -->

<!-- badges: end -->

The goal of Shiny.CWS.ONT is to allow users to explore data collected by
CWS-Ontario and our partners

## Installation

You can install the development version of Shiny.CWS.ONT from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("dhope/Shiny.CWS.ONT")
```

## Example

Load the app using:

``` r
library(Shiny.CWS.ONT)
run_shiny_app()
```

Alternatively, download the entire package then open in Rstudio and run:

``` r
pkgload::load_all()
CWS_ON_app()
```

## Local install with data

If you have downloaded the data zip file you can set up the data and R
package as follows:

Before installation, first create a new RStudio Project where you’ll
store the data required for this tool. To do so, select File \> New
Project. When prompted, select New Directory and New Project. Under the
field Directory name, type in CWS_ONT_Shiny, then, click on the Browse
button and select a location you’ll remember (e.g.,
c:/users/user_name/work). Put the `forShiny.zip` file into this folder.

Open the Rstudio package and run the following code to install the
required packages:

``` r
source("https://raw.githubusercontent.com/dhope/Shiny.CWS.ONT/refs/heads/main/inst/extdata/install_shiny_app.R")
```

This should create a file named `run_shiny_app.R` that will give you
instructions on updating the data and running the app.
