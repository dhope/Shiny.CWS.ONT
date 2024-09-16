
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

To update the data in the app, you can use the `update_data()` function. For example:

```r
update_data(base_folder_path = "/path/to/files/",
                        spp_list_csv= "spp_list.csv",
                        spp_core_csv = "spp_core.csv",
                        project_status_rds = "project_status.rds",
                        all_events_rds = "all_events.rds",
                        all_counts_rds = "all_counts.rds",
                        locations_rds = "locations.rds")
run_shiny_app()

```


