.cws_env <- new.env(parent = emptyenv())
.cws_env$t2sr_range <- c(-30,120)
.cws_env$t2ss_range <- c(-30,120)
.cws_env$bird_codes <- c('A', 'AE', 'B', 'CF', 'D', 'DD', 'FS', 'FY', 'H',
                         'H*', 'M', 'N', 'NB', 'NE', 'None provided', 'NU',
                         'NY', 'P', 'S', 'S*', 'T', 'V', 'X')
  # c("PROB", "CONF", "POSS", "OBS ")
.cws_env$sf_data <- NULL

#' Set options for matching column headers in GPS text logs
#' @noRd
.onLoad <- function(libname, pkgname) {
  initData()
  }


initData <- function(){
.cws_env$spp_list <- spp_list |> tidyr::replace_na(list(TC_L = "None"))

# .cws_env$spp_core <- spp_core



.cws_env$project_status <- project_status

.cws_env$in_field <- dplyr::filter(.cws_env$project_status,
                                   project_status %in% c("ARUs in Field",
                                                         "Awaiting processing")) |>
  dplyr::pull(project)

.cws_env$comple_date <- lubridate::stamp("1 March 1999", orders = 'dmy', quiet = T)(compile_date)


.cws_env$all_events <- all_events_example
.cws_env$all_counts_core <- all_counts_core_example

.cws_env$t2sr_range <- range(.cws_env$all_events$t2se[.cws_env$all_events$t2se==.cws_env$all_events$t2sr], na.rm=T)
.cws_env$t2ss_range <- range(.cws_env$all_events$t2se[.cws_env$all_events$t2se==.cws_env$all_events$t2ss], na.rm=T)



# .cws_env$all_species <- tibble(species = (.cws_env$all_counts_core$species_name_clean |> factor() |>
#                                    forcats::fct_infreq() |> unique() |> sort()) ) |>
#   dplyr::mutate(english_name = as.character(species)) #|>
  # left_join(.cws_env$spp_core,by = join_by(english_name))
.cws_env$all_time_periods <- unique(.cws_env$all_events$Time_period)
.cws_env$all_time_periods <- .cws_env$all_time_periods[stringr::str_detect(.cws_env$all_time_periods,
                                                         "Missing", negate=T)]

.cws_env$locs_only <- .cws_env$locations[.cws_env$locations$project %in% .cws_env$in_field,]

# .cws_env$f <- .cws_env$spp_core |> dplyr::filter(english_name %in% .cws_env$all_species$species) |>
#   dplyr::distinct(TC, TC_L)
.cws_env$fl <- .cws_env$spp_list$TC_L |> unique()
# names(.cws_env$fl) <- .cws_env$f$TC_L
}
