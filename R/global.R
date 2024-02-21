library(dplyr)
library(ggplot2)
library(sf)

spp_list <- readr::read_csv("data/obba_significant_species_list.csv") |>
  janitor::clean_names()

spp_core <- readr::read_csv("data/ECCC_Avian_Core_20230518.csv", col_types = readr::cols()) |>
  mutate(TC = stringr::str_sub(Technical_Committees, 1L, 2L)) |>
  dplyr::select(English_Name,TC, COSEWIC_Species, SARA_Species) |>
  mutate(TC_L = case_when(
    TC == "LA"~ "Landbirds",
    TC == "SH"~ "Shorebirds",
    TC == "WB" ~ "Waterbirds",
    TC == "WB-InMa" ~ "Waterbirds - Inland",
    TC == "WB-Sea" ~ "Seabirds",
    TC == "WF" ~ "Waterfowl",
    TRUE ~ "Non-migratory"

  ) )

project_status <- readr::read_rds( "data/project_status.rds")
comple_date <- file.info("data/project_status.rds")$ctime |> lubridate::as_date()



# All locations and events
all_events <- readr::read_rds("data/all_events.rds") |>
  filter(type!="Summary") |>
  mutate(
    type = case_when(
      type != "ARU recording"~type,
      project %in% project_status$project[project_status$data_processor=="CWS-ON-PS"]~"Visual scanning",
      TRUE ~ type
    ),
    loc_id = as.numeric(as.factor((paste0(location,source,
                                               type, project,
                                               longitude, latitude)))),
         Time_period =case_when(
  t2ss >= -60 & t2ss <= 150~"Dusk",
  t2sr >= -70 & t2sr <= 220 ~"Dawn",
  t2ss <= -60 & t2sr>220 ~ "Day" ,
  t2ss > 150 | t2sr < -70 ~ "Night",
  # t2sr > -70 & t2sr < -60 ~ "Pre-dawn",
  is.na(t2sr) ~ "Missing time data",
  TRUE~"Unk") ,
  t2se = case_when(Time_period=="Dusk"~t2ss,
                   Time_period == "Dawn"~t2sr,
                   TRUE ~ pmin(abs(t2ss),
                               abs(t2sr))
                   ) )


t2sr_range <- range(all_events$t2se[all_events$t2se==all_events$t2sr], na.rm=T)
t2ss_range <- range(all_events$t2se[all_events$t2se==all_events$t2ss], na.rm=T)


all_counts_core <- readr::read_rds("data/counts.rds") |>
  filter(event_id %in% all_events$event_id &
           stringr::str_detect(species_name_clean, "Unidentified\\s", negate=T) &
           species_name_clean %in% spp_list$english_name) |>  #TODO maybe allow these to be added later
  left_join(spp_core, by = join_by(species_name_clean == English_Name))





all_species <- tibble(species = (all_counts_core$species_name_clean |> factor() |>
  forcats::fct_infreq() |> unique() |> sort()) ) |>
  mutate(English_Name = as.character(species)) |>
  left_join(spp_core,by = join_by(English_Name))

f <- spp_core |> filter(English_Name %in% all_species$species) |>
  dplyr::distinct(TC, TC_L)
fl <- f$TC
names(fl) <- f$TC_L


all_time_periods <- unique(all_events$Time_period)
all_time_periods <- all_time_periods[stringr::str_detect(all_time_periods,
                                                         "Missing", negate=T)]


