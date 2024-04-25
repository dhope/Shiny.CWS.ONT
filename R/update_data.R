#' Update data in shiny app
#'
#' @param spp_list_csv
#' @param spp_core_csv
#' @param project_status_rds
#' @param all_events_rds
#' @param all_counts_rds
#'
#' @return
#' @export
#'
#' @examples
update_data <- function(spp_list_csv=NULL,
                        spp_core_csv = NULL,
                        project_status_rds = NULL,
                        all_events_rds = NULL,
                        all_counts_rds = NULL){

    if(!is.null(spp_list_csv)){
    .cws_env$spp_list <- spp_list_csv |>
      readr::read_csv(col_types = readr::cols()) |>
      janitor::clean_names()
    }

    if(!is.null(spp_core_csv)){
    .cws_env$spp_core <-
      spp_core_csv |>
      readr::read_csv(col_types = readr::cols()) |>
      dplyr::mutate(TC = stringr::str_sub(Technical_Committees, 1L, 2L)) |>
      dplyr::select(English_Name,TC, COSEWIC_Species, SARA_Species) |>
      dplyr::mutate(TC_L = case_when(
        TC == "LA"~ "Landbirds",
        TC == "SH"~ "Shorebirds",
        TC == "WB" ~ "Waterbirds",
        TC == "WB-InMa" ~ "Waterbirds - Inland",
        TC == "WB-Sea" ~ "Seabirds",
        TC == "WF" ~ "Waterfowl",
        TRUE ~ "Non-migratory"
      ) )
    }


    if(!is.null(project_status_rds)){
    .cws_env$project_status <- project_status_rds |>
      readr::read_rds()
    .cws_env$comple_date <- project_status_rds |>
      file.info() |> dplyr::pull(mtime) |> lubridate::as_date()
    }


    if(!is.null(all_events_rds)){
      # All locations and events
      .cws_env$all_events <- readr::read_rds(all_events_rds) |>
        # dplyr::bind_rows(list(
        #   data_boreal_2016$events,
        #   data_boreal_2012$events)) |>
        dplyr::filter(type!="Summary") |>
        dplyr::mutate(
          type = dplyr::case_when(
            type != "ARU recording"~type,
            project %in% .cws_env$project_status$project[.cws_env$project_status$data_processor=="CWS-ON-PS"]~"Visual scanning",
            TRUE ~ type
          ),
          loc_id = as.numeric(as.factor((paste0(location,source,
                                                type, project,
                                                longitude, latitude)))),
          Time_period =dplyr::case_when(
            t2ss >= -60 & t2ss <= 150~"Dusk",
            t2sr >= -70 & t2sr <= 220 ~"Dawn",
            t2ss <= -60 & t2sr>220 ~ "Day" ,
            t2ss > 150 | t2sr < -70 ~ "Night",
            # t2sr > -70 & t2sr < -60 ~ "Pre-dawn",
            is.na(t2sr) ~ "Missing time or location data",
            TRUE~"Unk") ,
          t2se = dplyr::case_when(Time_period=="Dusk"~t2ss,
                                  Time_period == "Dawn"~t2sr,
                                  TRUE ~ pmin(abs(t2ss),
                                              abs(t2sr))
          ) )
      rlang::inform("all_events updated:")
      glimpse(.cws_env$all_events)
    }
  if(!is.null(all_counts_rds)){
    .cws_env$all_counts_core <- readr::read_rds(all_counts_rds) |>
      dplyr::filter(event_id %in% .cws_env$all_events$event_id &
                      stringr::str_detect(species_name_clean, "Unidentified\\s", negate=T) &
                      species_name_clean %in% .cws_env$spp_list$english_name) |>  #TODO maybe allow these to be added later
      dplyr::left_join(.cws_env$spp_core, by = dplyr::join_by(species_name_clean == English_Name)) |>
      dplyr::left_join(naturecounts::meta_breeding_codes(),
                       by= dplyr::join_by(BreedingBirdAtlasCode==breeding_code )) |>
      dplyr::mutate( category =tidyr::replace_na(category,"None"))
    rlang::inform("all_counts_core updated:")
    glimpse(.cws_env$all_counts_core)
}

    ## Update calculated values

    .cws_env$t2sr_range <- range(.cws_env$all_events$t2se[.cws_env$all_events$t2se==.cws_env$all_events$t2sr], na.rm=T)
    .cws_env$t2ss_range <- range(.cws_env$all_events$t2se[.cws_env$all_events$t2se==.cws_env$all_events$t2ss], na.rm=T)



    .cws_env$all_species <- tibble(species = (.cws_env$all_counts_core$species_name_clean |> factor() |>
                                                forcats::fct_infreq() |> unique() |> sort()) ) |>
      dplyr::mutate(English_Name = as.character(species)) |>
      left_join(.cws_env$spp_core,by = join_by(English_Name))
    # print(.cws_env$all_species)
    .cws_env$all_time_periods <- unique(.cws_env$all_events$Time_period)
    .cws_env$all_time_periods <- .cws_env$all_time_periods[stringr::str_detect(.cws_env$all_time_periods,
                                                                               "Missing", negate=T)]



    .cws_env$f <- .cws_env$spp_core |> dplyr::filter(English_Name %in% .cws_env$all_species$species) |>
      dplyr::distinct(TC, TC_L)
    .cws_env$fl <- .cws_env$f$TC
    names(.cws_env$fl) <- .cws_env$f$TC_L
  }

