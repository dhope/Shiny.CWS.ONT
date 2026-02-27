library(Shiny.CWS.ONT)
update_data(base_folder_path =
              "./forShiny/",
            all_events_rds = "all_events.rds",
            all_counts_rds = "counts.rds", project_status_rds =
              "project_status.rds",
            locations_rds = "locations.rds")
run_shiny_app()

