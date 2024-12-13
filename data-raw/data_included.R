

data_boreal_2016 <- readr::read_rds("data-large//Boreal2016_data_summary.rds")
data_boreal_2012 <- readr::read_rds("data-large//Boreal_Burns_ARU_2012_data_summary.rds")

project_status <- "inst/extdata/data/project_status.rds" |>
  readr::read_rds()
spp_list <- readr::read_csv("inst/extdata/data/obba_significant_species_list.csv") |>
  janitor::clean_names()

spp_core <- readr::read_csv("inst/extdata/data/ECCC_Avian_Core_20230518.csv", col_types = readr::cols()) |>
  dplyr::mutate(TC = stringr::str_sub(Technical_Committees, 1L, 2L)) |>
  dplyr::select(English_Name,TC, COSEWIC_Species, SARA_Species) |>
  dplyr::mutate(TC_L = dplyr::case_when(
    TC == "LA"~ "Landbirds",
    TC == "SH"~ "Shorebirds",
    TC == "WB" ~ "Waterbirds",
    TC == "WB-InMa" ~ "Waterbirds - Inland",
    TC == "WB-Sea" ~ "Seabirds",
    TC == "WF" ~ "Waterfowl",
    TRUE ~ "Non-migratory"

  ) )

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
all_events <- readr::read_rds("data-large/all_events.rds") |>
  dplyr::bind_rows(list(
    data_boreal_2016$events,
    data_boreal_2012$events)) |>
  dplyr::filter(type!="Summary") |>
  dplyr::mutate(
    type = dplyr::case_when(
      type != "ARU recording"~type,
      project %in% project_status$project[project_status$data_processor=="CWS-ON-PS"]~"Visual scanning",
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


withr::with_seed(42,{
  all_events_example <- dplyr::slice_sample(all_events, n= 5, by = c(project,year))})

# usethis::use_data(all_events_example, overwrite = T, internal = T)
# readr::write_rds(all_events_example, "inst/extdata/data/all_events_example.rds")


all_counts_core_example <- readr::read_rds("C:/Users/HopeD/OneDrive - EC-EC/Scratchpad/SHINY_DATA_SHARE/data-large//counts.rds") |>
  dplyr::bind_rows(list(data_boreal_2016$counts,
                 data_boreal_2012$counts) ) |>
  dplyr::filter(event_id %in% all_events_example$event_id &
                  stringr::str_detect(species_name_clean, "Unidentified\\s", negate=T) &
                  species_name_clean %in% spp_list$english_name) |>  #TODO maybe allow these to be added later
  dplyr::left_join(spp_core, by = dplyr::join_by(species_name_clean == English_Name)) |>
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

locations_example <- readr::read_rds("C:/Users/HopeD/OneDrive - EC-EC/Scratchpad/SHINY_DATA_SHARE/data-large/locations.rds") |>
  dplyr::filter(location %in% all_events_example$location)


usethis::use_data(all_counts_core_example,all_events_example,
                  locations_example,
                  overwrite = T, internal = T)



library(naturecounts)

meta_breeding <- naturecounts::meta_breeding_codes()

usethis::use_data(meta_breeding,
                  overwrite = T, internal = T)


