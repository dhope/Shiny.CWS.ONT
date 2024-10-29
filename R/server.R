library(sf)
library(dplyr)


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
    shinyjs::toggle(id = "controls", condition = input$showMenu)
  })

  # Create the map ---------
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery", group = 'Imagery') %>%
      addProviderTiles(providers$Esri.WorldPhysical, group = "Physical") |>
      addProviderTiles(providers$OpenStreetMap, group = "Street") |>
      addProviderTiles(providers$OpenTopoMap, group = "Terrain") |>
      setView(lng = -85.67, lat = 50.36, zoom = 6)
  })

  ## Project summary --------------
  project_summary <-
    reactive({
      .cws_env$project_status |>
        dplyr::filter(project_status %in% input$project_status) %>%{
          if("All" %in% input$data_collector & (length(input$data_collector)==1)){
            .
          } else{
            dplyr::filter(., data_collector %in% input$data_collector)
          }
        } %>%
        {
          if("All" %in% input$data_processor & (length(input$data_processor)==1)){
            .
          } else{
            dplyr::filter(., data_processor %in% input$data_processor)
          }
        }

    })

  # Project table -----
  output$projects <- project_summary() |>
    dplyr::select(project_name,
                  data_collector,
                  data_processor, source, project_status) |>
    DT::renderDataTable(selection = list(target = 'row'))

  excluded_projects <- reactive({
    project_summary()$project_name[input$projects_rows_selected]
  })




  # Observation and Reactive functions ----------------------
  observe({
    x <- input$spp_comm
    if(x == "All"){
      x <- .cws_env$all_species$species
    } else{
      x <- .cws_env$all_species |>
        dplyr::filter(TC == input$spp_comm) |>
        pull(species)
    }
    # Can also set the label and select items
    updateSelectInput(session,"species",# "Select species to examine",
                      choices = x,
                      selected = head(x, 1)
    )
    updateSelectInput(session,"species_comm",# "Select species to examine",
                      choices = x
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

    dplyr::filter(filtered_events(),
           !project_name %in% excluded_projects() &
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })


  doys <- reactive({
    lubridate::yday(input$daterange)
  })

  ## Filter events data frame -----------------------------
  filtered_events <- reactive({
    .cws_env$all_events |>
      dplyr::left_join(project_summary(),
                by = dplyr::join_by(project, source)) %>% {
                  if(input$use_all_data){.} else{
      dplyr::filter(.,
        !is.na(project_name) &
        !project_name %in% excluded_projects()  &
          year>=input$years[1] &
          year <= input$years[2] &
          source %in% input$source &
          type %in% input$type &
          doy>= doys()[1] & doy <= doys()[2]
      ) %>%{
      if(input$include_missing_times){
        dplyr::filter(.,(is.na(t2se)) | ((Time_period %in% input$time_period) &
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
      dplyr::filter(.,
             ((Time_period %in% input$time_period)) & (
             (Time_period %in% c("Dusk", "Night") |
         (t2sr >= input$t2sr[1] & t2sr <= input$t2sr[2]) ) &
        (!Time_period %in% c("Dusk", "Night") |
           (t2ss >= input$t2ss[1] & t2ss <= input$t2ss[2]) ) )
      )

    }
      }
                  }
                }
    }
                  )


  ## Reactive to summarize by site based on settings ------------
  sites_summarize <- reactive({
    filtered_events() |>
      dplyr::count(loc_id,longitude,
                   latitude,
                   source,
                   type,
                   project,
                   location
      )



  })

  ## Species summary -----------------
  species_summary <- reactive({
    active_events <- pull(filtered_events(), event_id)
    .cws_env$all_counts_core %>% {

      if(input$data_layer=='Species Observations'){

      dplyr::filter(.,event_id %in% active_events &
                      species_name_clean == input$species) %>%
          {
            # if( ('Naturecounts' %in% input$source) & (length(input$source)==1)){
              dplyr::filter(., BreedingCode %in% input$breeding)
            # } else{.}
          } |>
          # left_join(active_events() |>
          #             dplyr::select(event_id, doy) |>
          #             .by = dplyr::join_by(event_id)
          #             )
      dplyr::summarize(
        n_observations = n(),
        max_total_count = max(total_count, na.rm=T),
        # range_dates = diff(range(doy)),
        sum_total_count = sum(total_count, na.rm=T),
        avg_total_count = mean(total_count, na.rm = T),
        .by = c(location, species_name_clean)
      )  %>%
      { if(input$limit_count){
        dplyr::filter(., max_total_count >= input$lower_count_limits &
                        max_total_count <= input$upper_count_limits)
      } else{.}}
      } else{# if(input$data_layer=='Community Builder') {
        dplyr::filter(.,event_id %in% active_events &
                 species_common_name %in% input$species_comm) |>
          dplyr::summarise(n_spp = n_distinct(species_common_name),
                    n_observations = n(),
                    species_name_clean = glue::glue_collapse(input$species_comm, sep = ", "),
                    max_total_count = max(total_count, na.rm=T),
                    sum_total_count = sum(total_count, na.rm=T),
                    avg_total_count = mean(total_count, na.rm = T),
                    .by = location) |>
          dplyr::filter(n_spp == length(input$species_comm))
    }
    } |> dplyr::left_join(sites_summarize(),
                          by = dplyr::join_by(location))
  }
  )






  ## Table by time period -----
  # Generate table count of observations by time period
  count_obs <- reactive({
    filtered_events() |>
      janitor::tabyl(Time_period)
      })
  output$n_events <- renderTable(count_obs())

  ## Counts in bounds -------------
  counts_in_bounds <-
    reactive({
      active_events <- pull(filtered_events(), event_id)
      .cws_env$all_counts_core |>
        dplyr::filter(event_id %in% active_events &
                        species_name_clean == input$species) |>
        dplyr::left_join(x = obsInBounds(),
                         by = dplyr::join_by(location,  event_id)
                         ) |>
        tidyr::replace_na(list(total_count= 0) )
    })


  # Plots ----------------

  ## Event summary plot -----------
  output$event_summary <- renderPlot({

    if (nrow(filtered_events()) == 0)
      return(NULL)

    ggplot(filtered_events() |>
             dplyr::filter(!is.na(t2se))) +
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

  output$p_obs <- renderPlot({
    # If no data are in view, don't plot
    if (nrow(counts_in_bounds()) == 0)
      return(NULL)

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
    # If no data are in view, don't plot
    if (nrow(obsInBounds()) == 0)
      return(NULL)

    ggplot(obsInBounds() |>
             dplyr::left_join(species_summary(),
                              by = dplyr::join_by(project, location,
                                                  source, type, longitude,
                                                  latitude, loc_id)) |>
             tidyr::replace_na(list(max_total_count=0)),
           aes(max_total_count)) +
      geom_histogram(binwidth = 1) +
      theme_minimal()
  })



# Populate the map ------------------

  ## Show a popup at the given location ---
  showPopup <- function(loc_id, lat, lng) {
    ll <- sites_summarize()
    selectedloc <- ll[ll$loc_id == loc_id,]
    content <- as.character(tagList(
      tags$h4("Location:", (paste(selectedloc$location, sep = ", ") )),
      tags$h6("Source: ", paste(selectedloc$source, sep = ", ") ),
      tags$h6("Project: ", paste(selectedloc$project, sep = ", ") ),
      tags$h6("Type: ", paste(selectedloc$type, sep = ", ") ) ,
      tags$h6("N: ", paste(selectedloc$n, sep = ", ") ) ) )
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = loc_id)
  }

  ## Add markers -------
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

    ### Create Label
    lab_counts <- sprintf(
      "<strong>Species:</strong> %s <br/>
      <strong>Max total count:</strong> %s <br/>
      <strong>Average total count:</strong> %s<br/>
      <strong>Sum of total counts:</strong> %s<br/>
      <strong>Number of observations:</strong> %s",
      species_summary()$species_name_clean,
      species_summary()$max_total_count,
      species_summary()$avg_total_count,
      species_summary()$sum_total_count,
      species_summary()$n_observations  ) %>%
      lapply(htmltools::HTML)

    radius <- sites_summarize()$n / max(sites_summarize()$n) * 30000

    colourData <- sites_summarize()$type
    colourDataspp <- species_summary()$type
    pal <- colorFactor("viridis", colourData)
    add_clusters <- switch(isTRUE(input$cluster), markerClusterOptions(
      # Leaving this here in case I want to customize cluster colours at some point
        #     iconCreateFunction=JS("function (cluster) {
  #   var childCount = cluster.getChildCount();
  #   var c = ' marker-cluster-';
  #   if (childCount < 100) {
  #     c += 'large';
  #   } else if (childCount < 1000) {
  #     c += 'medium';
  #   } else {
  #     c += 'small';
  #   }
  #   return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });
  #
  # }")

    ),NULL)

    leafletProxy("map") %>%
      clearShapes() |>
      clearMarkerClusters() |>
      clearMarkers() %>% {
      if(input$data_layer == "Species Observations" || input$data_layer == "Community Builder") {
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
          addCircleMarkers( ~longitude, ~latitude,
                           data = species_summary(),
                           clusterOptions = add_clusters,
                           layerId=~loc_id,popup= lab_counts,
                           # stroke=FALSE, fillOpacity=1,
                           color=pal(colourDataspp),
                           fill = F

          # addAwesomeMarkers( ~longitude, ~latitude,data = species_summary(),
          #                    clusterOptions = add_clusters,
          #                    layerId=~loc_id,popup= lab_counts,
          #                    icon = awesomeIcons(
          #                      icon = 'ios-close',
          #                      iconColor = 'black',
          #                      library = 'ion',
          #                      markerColor = pal(colourDataspp)
          #                    )
       # addMarkers( ~longitude, ~latitude,data = species_summary(),
       #             clusterOptions = add_clusters,
       #             layerId=~loc_id,popup= lab_counts,
       #             # stroke=FALSE, #fillOpacity=0.4
       #             color=pal(colourData)
        ) }
         else{.}
        } %>%
    {
    if(input$data_layer == "Surveyed Locations"){
      {
      if(input$includesurveyed){
        addCircles(., ~longitude, ~latitude,
                   # layerId=~loc_id,
                   data = .cws_env$locs_only,
                   color = 'darkgrey',
                   weight = case_when(input$map_zoom <=4 ~1,
                                      input$map_zoom ==5 ~2,
                                      input$map_zoom ==6 ~3,
                                      input$map_zoom ==7 ~5,
                                      input$map_zoom ==8 ~7,
                                      input$map_zoom ==9 ~9,
                                      input$map_zoom >9 ~11),
                   opacity = 1, fill = TRUE, fillOpacity = 1 )

      } else{.}} %>% {
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
      } } else{.}
      }  |>
      addScaleBar("bottomright",options = scaleBarOptions()) |>
      addLegend("bottomright", pal=pal, values=colourData, title="Data type",
                layerId="colourLegend")|>
      addLayersControl(position = 'bottomright',
        baseGroups = c( "Terrain",
                        "Imagery",
                        "Physical",
                        "Street" ),
                options = layersControlOptions(collapsed = FALSE)
      )
  })

  ## Map click ----------
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



  output$print_ex <- renderPrint({
    if(length(excluded_projects())==0) return("None")
    excluded_projects()
    }
    )






# Download datasets ---------------
  datasetInput <- reactive({
    switch(input$dataset,
           "locations" = sites_summarize(),
           "effort" = filtered_events(),
           "species_observations" = species_summary())

  })

  ## Table of selected dataset ----
  output$table <- DT::renderDataTable({
    dplyr::slice_sample(.data =  datasetInput(),
                        n=input$rowtable)
  })

  ## Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      readr::write_csv(datasetInput(), file)
    }
  )



}
