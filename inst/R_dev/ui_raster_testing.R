# library(leaflet)
# library(shinyjs)
get_env_ob <- function(object){
  .cws_env[[object]]
}

# Define UI for application that draws a histogram
ui_rast <- function(){

  tabPanel("Setup",
           sidebarPanel(
             inputPanel(
               selectInput("species_focal", "Select species to examine.
                                                                To type, click on selected species and hit <backspace>.", .cws_env$modelled_species, multiple = FALSE),

               selectInput("covariate_layer", "Select covariate layer", .cws_env$, multiple = FALSE),
               selectInput("prediction_layer", "Select prediction layer", .cws_env$all_species$species, multiple = FALSE),


               shinyWidgets::awesomeCheckboxGroup('source', "Data source",
                                                  choices = unique(get_env_ob("all_events")$source),
                                                  selected = unique(get_env_ob("all_events")$source),
                                                  inline = T
               ),
               shinyWidgets::awesomeCheckboxGroup('type', "Data type",
                                                  choices = unique(get_env_ob("all_events")$type),
                                                  selected = c("ARU recording",
                                                               "Checklist",
                                                               "Point Count"),inline = T
               ),
               sliderInput('years', "Years included", value = c(2021, 2023),
                           min = min(get_env_ob("all_events")$year, na.rm = T),
                           max =max(get_env_ob("all_events")$year, na.rm = T), sep = ""),
               dateRangeInput('daterange',"Range of dates (ignore year)",
                              start = "2024-05-01", end = "2024-07-10",
                              min = "2024-01-01", max = "2024-12-31",
                              format = "MM-dd"
               ) ,
               shinyWidgets::awesomeCheckboxGroup("time_period",
                                                  "Time period",
                                                  choices =get_env_ob("all_time_periods"),
                                                  selected = "Dawn", inline = T), # Leaving this out for now, but may want to limit later on
               # conditionalPanel()
               sliderInput("t2sr", "Time to sunrise (minutes)",
                           value = c(-30, 120),
                           min = ceiling(get_env_ob("t2sr_range")[[1]]),
                           max = ceiling(get_env_ob("t2sr_range")[[2]]),
                           step = 5),
               checkboxInput("include_missing_times",
                             "Include data with missing times?",
                             value = FALSE),
               conditionalPanel("input.time_period.includes('Dusk')||input.time_period.includes('Night')",
                                sliderInput("t2ss", "Time to sunset (minutes)",
                                            value = c(-30, 120),
                                            min = ceiling(get_env_ob("t2ss_range")[[1]]),
                                            max = ceiling(get_env_ob("t2ss_range")[[2]])
                                            ,
                                            step = 5)),
               checkboxInput("use_all_data",
                             markdown(
                               glue::glue("Load **ALL** data?

 {emo::ji('warning')} This may **significantly** slow the server {emo::ji('warning')}")),
                             value = FALSE)) ,
             tableOutput("n_events")
           ),
           mainPanel(
             plotOutput("event_summary", width = "100%", height = '800px')
           )

  ),
                                 tabPanel("Assess model output",
                                          titlePanel("Raster evaluation"),

                                          div(class="outer",
                                              tags$head(
                                                #   # Include our custom CSS
                                                includeCSS(system.file("extdata","styles.css",
                                                                       package = 'Shiny.CWS.ONT')),
                                                shinyjs::useShinyjs()
                                                #   includeScript("gomap.js")
                                              ),
                                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                                              leafletOutput("rmap", width="100%", height="100%"),


                                          )

                                 )


)
}




