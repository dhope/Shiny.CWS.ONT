library(terra)
library(leaflet)
library(leaflet.extras2)
library(leafsync)
g <- glue::glue
species <- "CONW"

rast_data_folders <- g("D:/SPATIAL/RoF_Models/INLA/{species}/")
rasters <- list.files(rast_data_folders, pattern = "tif")

r_pred <- rast(glue::glue("F:/RoF_Models/2025-01-10_prediction_rasters.nc"))


compare_rast <- "D:/DELIVERABLES/Spatialworks24_25/DataLayers/1_FNLC/fnlc_orig30m/"

r1 <- rast(g("{rast_data_folders}/{stringr::str_subset(rasters, 'p_obs')}"))
rc <- rast(compare_rast) |> crop(r1)


q1 <- global(r1,fun=quantile, probs= 0.99, na.rm=T)
r1[r1>q1$X99.] <- NA

r2 <- aggregate(r1, 5, na.rm=T)
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r2),
                    na.color = "transparent")
pal_c <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r_pred$`2025-01-10_prediction_rasters_1`),
                      na.color = "transparent")
# Create the map for rasters ---------
rmap <-
  leaflet() %>%
    # addProviderTiles("Esri.WorldImagery", group = 'Imagery') %>%
    # # addProviderTiles(providers$Esri.WorldPhysical, group = "Physical") |>
    # addProviderTiles(providers$OpenStreetMap, group = "Street") |>
    # # addProviderTiles(providers$OpenTopoMap, group = "Terrain") |>
    # setView(lng = -85.67, lat = 50.36, zoom = 6) |>
  addRasterImage(r2, colors = pal) %>%
  addLegend(title = stringr::str_remove(
    stringr::str_subset(rasters, 'p_obs'), ".tif"),
    values = values(r2),
    pal = pal)



map <- #renderLeaflet({
  leaflet() %>%
  # addProviderTiles("Esri.WorldImagery", group = 'Imagery') %>%
  # addProviderTiles(providers$Esri.WorldPhysical, group = "Physical") |>
  # addProviderTiles(providers$OpenStreetMap, group = "Street") |>
  # addProviderTiles(providers$OpenTopoMap, group = "Terrain") |>
  # setView(lng = -85.67, lat = 50.36, zoom = 6) |>
  addRasterImage(r_pred$`2025-01-10_prediction_rasters_1`, colors = pal_c) |>
  addLegend(title = "prediction_raster", values=values(r_pred$`2025-01-10_prediction_rasters_1`) ,
            pal = pal_c)

# })#


sync(map, rmap)
