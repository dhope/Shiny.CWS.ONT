project_status <- glue::glue("{dat_loc}/project_status.rds") |>
  readr::read_rds()
compile_date <- glue::glue("{dat_loc}/project_status.rds") |>
  file.info() |> dplyr::pull(mtime) |> lubridate::as_date()

# spp_list <- readr::read_csv(glue::glue("{dat_loc2}/obba_significant_species_list.csv")) |>
#   janitor::clean_names()

# spp_core <- readr::read_csv(glue::glue("{dat_loc}/Avian_Core_20251124.csv"), col_types = readr::cols()) |>
#   janitor::clean_names() |>
#   dplyr::mutate(TC = stringr::str_sub(technical_committees, 1L, 2L),
#                 concept_id = glue::glue("avibase-{stringr::str_sub(avibase_id,1L, 8L)}")) |>
#   dplyr::select(english_name,TC, cosewic_status, scientific_name,sara_status,concept_id) |>
#   dplyr::mutate(TC_L = dplyr::case_when(
#     TC == "LA"~ "Landbirds",
#     TC == "SH"~ "Shorebirds",
#     TC == "WB" ~ "Waterbirds",
#     TC == "WB-InMa" ~ "Waterbirds - Inland",
#     TC == "WB-Sea" ~ "Seabirds",
#     TC == "WF" ~ "Waterfowl",
#     TRUE ~ "Non-migratory"
#
#   ) )

all_counts_core <- readr::read_rds(glue::glue("{dat_loc}/counts.rds"))
spp_list <- distinct(all_counts_core, english_name =species_name_clean, common_id, TC_L )
non_vocal_spp <- c("Wilson's Snipe",
                   "Yellow-bellied Sapsucker",
                   "Downy Woodpecker",
                   "Ruffed Grouse",
                   "Hairy Woodpecker",
                   "Pileated Woodpecker", "Red-bellied Woodpecker",
                   "American Woodcock",
                   "Black-backed Woodpecker",
                   "Red-headed Woodpecker",
                   "Sharp-tailed Grouse",
                   "American Three-toed Woodpecker",
                   "Spruce Grouse")

# All locations and events
all_events <- readr::read_rds(glue::glue("{dat_loc}/all_events.rds")) |>
  # dplyr::bind_rows(list(
  #   data_boreal_2016$events,
  #   data_boreal_2012$events)) |>
  dplyr::filter(type!="Summary") |>
  dplyr::mutate(
    type = dplyr::case_when(
      type != "ARU recording"~type,
      project %in% project_status$project[project_status$data_processor=="CWS-ON-PS"]~"Visual scanning",
      TRUE ~ type
    ),
    # loc_id = as.numeric(as.factor((paste0(location,source,
    #                                       type, project,
    #                                       longitude, latitude)))),
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


withr::with_seed(2025-12-17,{
  all_events_example <- dplyr::slice_sample(all_events, n= 5, by = c(project,year))})

 # usethis::use_data(all_events_example, overwrite = T, internal = T)
# readr::write_rds(all_events_example, "inst/extdata/data/all_events_example.rds")

library(naturecounts)

meta_breeding <- naturecounts::meta_breeding_codes()

# usethis::use_data(meta_breeding,
#                   overwrite = T, internal = T)

all_counts_core_example <- all_counts_core |>
  # dplyr::bind_rows(list(data_boreal_2016$counts,
  #                data_boreal_2012$counts) ) |>
  dplyr::filter(event_id %in% all_events_example$event_id &
                  stringr::str_detect(species_name_clean, "Unidentified\\s", negate=T) &
                  species_name_clean %in% spp_list$english_name) |>  #TODO maybe allow these to be added later
  # dplyr::left_join(spp_core, by = dplyr::join_by(species_name_clean==english_name)) |>
  dplyr::left_join(meta_breeding,
                   by= dplyr::join_by(breeding_rank==rank,BreedingBirdAtlasCode==breeding_code )) |>
  dplyr::mutate( category =tidyr::replace_na(category,"None"),
                 BreedingCode =
                   dplyr::case_when(
                     !is.na(BreedingBirdAtlasCode)~BreedingBirdAtlasCode,
                     (!is.na(collection) & stringr::str_detect(collection, "ONATLAS3"))~"None provided",
                     stringr::str_detect(vocalizations, "Song") ~ "S*",
                     stringr::str_detect(vocalizations, "S-C") ~ "S*",
                     (stringr::str_detect(vocalizations, "Non-vocal") & species_name_clean %in% non_vocal_spp )~"S*",
                     (stringr::str_detect(vocalizations, "NV-C")  & species_name_clean %in% non_vocal_spp )~"S*",
                     stringr::str_detect(vocalizations, "Non-vocal") ~"H*",
                     stringr::str_detect(vocalizations, "Call") %in% vocalizations ~"H*",
                     TRUE~"None provided"
                   ))


library(sf)

locations_example <- readr::read_rds(glue::glue("{dat_loc}/locations.rds") )|>
  dplyr::filter(location %in% all_events_example$location) |>
  distinct(move_type, site_id, geometry)


usethis::use_data(all_counts_core_example,all_events_example,
                  locations_example,meta_breeding, #spp_core,
                  project_status,compile_date,
                  spp_list,
                  non_vocal_spp,overwrite = T, internal = T)





