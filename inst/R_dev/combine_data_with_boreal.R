library(readr)
data_boreal_2016 <- readr::read_rds("data-large//Boreal2016_data_summary.rds")
data_boreal_2012 <- readr::read_rds("data-large//Boreal_Burns_ARU_2012_data_summary.rds")
all_events_with_boreal <-
  bind_rows(list(read_rds("data-large/all_events.rds"),
            data_boreal_2016$events,
            data_boreal_2012$events))


all_counts_with_boreal <-
  readr::read_rds("data-large//counts.rds") |>
  dplyr::bind_rows(list(data_boreal_2016$counts,
                        data_boreal_2012$counts) )

write_rds(all_events_with_boreal, "data-large/all_events_with_boreal.rds")
write_rds(all_counts_with_boreal, "data-large/all_counts_with_boreal.rds")



update_data(base_folder_path =
              "C:/Users/HopeD/OneDrive - EC-EC/Scratchpad/SHINY_DATA_SHARE",
               all_events_rds = "data-large/all_events.rds",
            all_counts_rds = "data-large/counts.rds", project_status_rds =
              "data-large/project_status.rds", locations_rds = "data-large/locations.rds")

run_shiny_app()
