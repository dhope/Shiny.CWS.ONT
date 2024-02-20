library(shiny)
library(shinyjs)
library(sf)
library(leaflet)
library(dplyr)






# all_events <- dplyr::slice_sample(all_events, n=1000)
# dplyr::select(-latitude, -longitude)








# Define server to create map with options
server <- function(input, output, session) {
  # Taken from https://github.com/rstudio/shiny-examples/blob/main/063-superzip-example/server.R#L14

  #https://stackoverflow.com/questions/42159804/how-to-collapse-sidebarpanel-in-shiny-app
  # observeEvent(input$showMenu, {
  #   shinyjs::show(id = "controls")
  # })
  # observeEvent(input$hideMenu, {
  #   shinyjs::hide(id = "controls")
  # })
  observe({
    toggle(id = "controls", condition = input$showMenu)
  })
  # observe({
  #   toggle(id = "exclude", condition = input$showExclude)
  # })

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      # addTiles() %>%
      addProviderTiles("Esri.WorldImagery", group = 'Imagery') %>%
      addProviderTiles(providers$Esri.WorldPhysical, group = "Physical") |>
      addProviderTiles(providers$OpenStreetMap, group = "Street") |>
      addProviderTiles(providers$OpenTopoMap, group = "Terrain") |>
      setView(lng = -85.67, lat = 50.36, zoom = 6)
  })

  observe({
    x <- input$spp_comm
    if(x == "All"){
      x <- all_species$species
    } else{
      x <- all_species |>
        filter(TC == input$spp_comm) |>
        pull(species)
    }
    # Can also set the label and select items
    updateSelectInput(session,"species",# "Select species to examine",
                      choices = x,
                      selected = head(x, 1)
    )


  })


  # A reactive expression that returns the set of locations that are
  # in bounds right now
  obsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(filtered_events()[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    filter(filtered_events(),
           !project %in% input$exclude &
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })


  doys <- reactive({
    lubridate::yday(input$daterange)
  })


  filtered_events <- reactive({
    all_events |>
      left_join(project_summary(),
                by = join_by(project, source)) |>
      filter(
        !is.na(project_name) &
        !project %in% input$exclude &
          year>=input$years[1] &
          year <= input$years[2] &
          source %in% input$source &
          type %in% input$type &
          doy>= doys()[1] & doy <= doys()[2]

      ) %>%{
      if(input$include_missing_times){
        filter(.,(is.na(t2se)) | ((Time_period %in% input$time_period) &
                                    (
               (
                 Time_period %in% c("Dusk", "Night") |
           (t2sr >= input$t2sr[1] & t2sr <= input$t2sr[2])
           ) &
          (!Time_period %in% c("Dusk", "Night") |
             (t2ss >= input$t2ss[1] & t2ss <= input$t2ss[2]) ) )
        )
        )
      }
    else{
      filter(.,
             ((Time_period %in% input$time_period)) & (
             (Time_period %in% c("Dusk", "Night") |
         (t2sr >= input$t2sr[1] & t2sr <= input$t2sr[2]) ) &
        (!Time_period %in% c("Dusk", "Night") |
           (t2ss >= input$t2ss[1] & t2ss <= input$t2ss[2]) ) )
      )

    }
      }
  })


  count_obs <- reactive({
    filtered_events() |>
      janitor::tabyl(Time_period)
      })
  output$n_events <- renderTable(count_obs())

  output$event_summary <- renderPlot({
    ggplot(filtered_events() |>
             filter(!is.na(t2se))) +
      stat_summary_hex(aes(lubridate::ymd("2020-01-01") +doy, t2se, z=1),
        fun = 'sum') +
      facet_grid(year~Time_period) +
      labs(x = "Date",
           y = "Time to sun event",
           fill = "Count") +
      rcartocolor::scale_fill_carto_c() +
      theme_minimal(base_size = 14,
                    base_family = "Roboto Condensed")
  })



  # Reactive to summarize by site based on settings
  sites_summarize <- reactive({
     filtered_events() |>
      dplyr::count(location, source,
                   type, project,
                   longitude,
                   latitude, loc_id)



  })

  species_summary <- reactive({
    active_events <- pull(filtered_events(), event_id)
    all_counts_core |>
      filter(event_id %in% active_events &
               species_name_clean == input$species) |>
      summarize(
        n_observations = n(),
        # p_obs = sum(!is.na(total_count))/n_observations,
        max_total_count = max(total_count, na.rm=T),
        sum_total_count = sum(total_count, na.rm=T),
        avg_total_count = mean(total_count, na.rm = T),
        .by = c(location, species_name_clean)
      ) |> left_join(sites_summarize(),
                     by = join_by(location))
  }
  )


  # Show a popup at the given location
  showPopup <- function(loc_id, lat, lng) {
    ll <- sites_summarize()
    selectedloc <- ll[ll$loc_id == loc_id,]
    content <- as.character(tagList(
      tags$h4("Location:", (paste(selectedloc$location, sep = ", ") )),
      tags$h6("Source: ", paste(selectedloc$source, sep = ", ") ),
      tags$h6("Project: ", paste(selectedloc$project, sep = ", ") ),
      tags$h6("Type: ", paste(selectedloc$type, sep = ", ") ) ,
      tags$h6("N: ", paste(selectedloc$n, sep = ", ") ) ) )
    #   tags$strong(HTML(sprintf("%s, %s %s",
    #                            selectedloc$city.x, selectedloc$state.x, selectedloc$zipcode
    #   ))), tags$br(),
    #   sprintf("Median household income: %s", dollar(selectedloc$income * 1000)), tags$br(),
    #   sprintf("Percent of adults with BA: %s%%", as.integer(selectedloc$college)), tags$br(),
    #   sprintf("Adult population: %s", selectedloc$adultpop)
    # ))
    # content
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = loc_id)
  }


  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    # colorBy <- input$color
    # sizeBy <- input$size

    # if (colorBy == "superzip") {
    #   # Color and palette are treated specially in the "superzip" case, because
    #   # the values are categorical instead of continuous.
    #   colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
    #   pal <- colorFactor("viridis", colorData)
    # } else {
    #   colorData <- zipdata[[colorBy]]
    #   pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    # }
    #
    # if (sizeBy == "superzip") {
    #   # Radius is treated specially in the "superzip" case.
    #   radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
    # } else {
    #   radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
    # }
    #
    content <- function(d){as.character(tagList(
      tags$h4("Location:", (paste(d$location, sep = ", ") )),
      tags$h6("Source: ", paste(d$source, sep = ", ") ),
      tags$h6("Project: ", paste(d$project, sep = ", ") ),
      tags$h6("Type: ", paste(d$type, sep = ", ") ) ,
      tags$h6("N: ", paste(d$n, sep = ", ") ) ) ) }
    radius <- sites_summarize()$n / max(sites_summarize()$n) * 30000

    colourData <- sites_summarize()$type
    pal <- colorFactor("viridis", colourData)
    add_clusters <- switch(isTRUE(input$cluster), markerClusterOptions(),NULL)
    leafletProxy("map") %>%
      clearShapes() |>
      clearMarkerClusters() |>
      clearMarkers() %>% {
      if(input$data_layer == "Species Observations") {
        {if(input$show_effort){
          addCircles(., ~longitude, ~latitude,
                   layerId=~loc_id,
                   data = obsInBounds(),color = 'grey',
                   weight = case_when(input$map_zoom <=4 ~1,
                                      input$map_zoom ==5 ~2,
                                      input$map_zoom ==6 ~3,
                                      input$map_zoom ==7 ~5,
                                      input$map_zoom ==8 ~7,
                                      input$map_zoom ==9 ~9,
                                      input$map_zoom >9 ~11),
                   opacity = 1, fill = TRUE, fillOpacity = 1 ) } else{.} }|>
       addMarkers( ~longitude, ~latitude,data = species_summary(),
                   clusterOptions = add_clusters,
                   # radius=~n*input$base_point_size,
                   layerId=~loc_id
                   # stroke=FALSE, #fillOpacity=0.4
                   # fillColor=pal(colourData)
        ) }
         else{.}
        } %>%
    {
    if(input$data_layer == "Surveyed Locations"){
      if(input$cluster){
        addCircleMarkers(., ~longitude, ~latitude,
                   layerId=~loc_id,
                   data =  sites_summarize(),
                   clusterOptions = add_clusters,
                   stroke=FALSE, fillOpacity=0.4,
                   fillColor=pal(colourData))
      } else{
      addCircles(., ~longitude, ~latitude,
                  radius=radius,
                  layerId=~loc_id,
                 data =  sites_summarize(),
                 stroke=FALSE, fillOpacity=0.4,
                 fillColor=pal(colourData))
      }
      } else{.}
      }  |>
      addLayersControl(position = 'bottomright',
        baseGroups = c( "Terrain",
                        "Imagery",
                        "Physical",
                        "Street" ),
                # overlayGroups = c("Species List",#"Interpreted locations",
                #                    "# Obs species"),
                options = layersControlOptions(collapsed = FALSE)
      ) |>
      addLegend("bottomleft", pal=pal, values=colourData, title="Data type",
                layerId="colourLegend")
  })


  project_summary <-
    reactive({
      project_status |>
        filter(project_status %in% input$project_status) %>%{
        if("All" %in% input$data_collector & (length(input$data_collector)==1)){
          .
        } else{
          filter(., data_collector %in% input$data_collector)
        }
      } %>%
        {
          if("All" %in% input$data_processor & (length(input$data_processor)==1)){
            .
          } else{
            filter(., data_processor %in% input$data_processor)
          }
        }

  })

  output$projects <- project_summary() |>
    dplyr::select(project_name,
                  data_collector,
                  data_processor, source, project_status) |>
    DT::datatable() |>
    DT::renderDataTable()

  counts_in_bounds <-
    reactive({
      active_events <- pull(filtered_events(), event_id)
      all_counts_core |>
        filter(event_id %in% active_events &
                 species_name_clean == input$species) |>
        left_join(x = obsInBounds(),
                  by = join_by(location, collection, event_id)) |>
        tidyr::replace_na(list(total_count= 0) )
    })

  output$p_obs <- renderPlot({
    counts_in_bounds() |>
      ggplot(aes(lubridate::ymd("2020-01-01") + doy-1, t2se, z= total_count)) +
      stat_summary_hex(fun = function(x){
        if(length(x)==0) return(0)
        (sum(x>0)/length(x))
      }) +
      facet_wrap(~Time_period,
                 scales = 'free', ncol = 1) +
      labs(x = "", y = "Time to sun event",
           fill = "Proportion with Obs") +
      rcartocolor::scale_fill_carto_c() +
      theme_minimal(base_size = 14,
                    base_family = "Roboto Condensed") +
      theme(legend.position = 'bottom')
  })


  output$hist <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(obsInBounds()) == 0)
      return(NULL)

    ggplot(obsInBounds() |>
           left_join(species_summary(),
                     by = join_by(project, location,
                                  source, type, longitude,
                                  latitude, loc_id)) |>
           tidyr::replace_na(list(max_total_count=0)),
           aes(max_total_count)) +
      geom_histogram(binwidth = 1) +
      theme_minimal()
     })

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showPopup(event$id, event$lat, event$lng)
    })
  })

  datasetInput <- reactive({
    switch(input$dataset,
           "locations" = sites_summarize(),
           "effort" = filtered_events(),
           "species_observations" = species_summary())

  })

  # Table of selected dataset ----
  output$table <- DT::renderDataTable({
    datasetInput() |>
    dplyr::slice_sample(n=input$rowtable) |>
    DT::datatable()
  })

  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      readr::write_csv(datasetInput(), file)
    }
  )



}
