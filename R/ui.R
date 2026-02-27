# library(leaflet)
# library(shinyjs)
get_env_ob <- function(object){
  .cws_env[[object]]
}

# Define UI for application that draws a histogram
ui_fun <- function(){ navbarPage("Data Explorer", id="nav",

# Intial app setup --------------------------------------------------------
          # print(search()),

           tabPanel("Setup",
                    sidebarPanel(
                      inputPanel(
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
                       sliderInput('years', "Years included", value = c(2021, 2025),
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
                                     "Load **ALL** data?

 ⚠️ This may **significantly** slow the server ⚠️ "),
                                     value = FALSE)) ,
                      tableOutput("n_events")
                      ),
                    mainPanel(
                      plotOutput("event_summary", width = "100%", height = '800px')
                    )

           ),

# Interactive map ---------------------------------------------------------


           tabPanel("Interactive map",
                    div(class="outer",
                              tags$head(
                              #   # Include our custom CSS
                                includeCSS(system.file("extdata","styles.css",
                                                       package = 'Shiny.CWS.ONT')),
                                shinyjs::useShinyjs()
                              #   includeScript("gomap.js")
                              ),
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("map", width="100%", height="100%"),

                              # # Shiny versions prior to 0.11 should use class = "modal" instead.
                              absolutePanel(id = "show-panel", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 120, left = 10, right = 'auto', bottom = "auto",
                                            width = 'auto', height = "auto",
                                            checkboxInput("showMenu", "Show menu?", TRUE),
                                            checkboxInput("cluster", "Cluster points?", TRUE),
                                            # actionButton("clearoverlays", "Clear overlays"),
                                            conditionalPanel(id="missing-locs",
                                                             condition = 'input.data_layer=="Surveyed Locations"',
                                              checkboxInput("includesurveyed", "Show unprocessed?", FALSE) ),
                                            conditionalPanel(id = "spp-plots",
                                                             condition = "input.data_layer=='Species Observations'",
                                                             checkboxInput("show_effort", "Show effort?", FALSE),
                                                             checkboxInput('limit_count', "Limit count?", FALSE ),
                                                             hr(),
                                                             p("Plots show data within map window",br(),
                                                             "or if polygon is drawn, shows points within polygon."),
                                                             em("If plots do not update after drawing/deleting polygon,",
                                                                br(),
                                                                "move the map window a bit.") ,

                                                             plotOutput("p_obs", height = 500),
                                                             plotOutput("hist", height = 200)
                                            )
                                            # checkboxInput("showExclude", "Adjust exclusions?", FALSE)
                                            ),


                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                        selectInput("base_point_size", "Adjust point size",choices = list("Small"=.5, "Medium"=1, "Large"=2 ),
                                                    selected = 1),#min = .1, max = 500, step =50, value = 100),
                                      radioButtons("data_layer", "Data layer", choices = c("None", "Surveyed Locations", "Species Observations", "Community Builder"),
                                                   selected = "None"),
                        conditionalPanel(id = "spp-control",condition = "input.data_layer=='Species Observations'||input.data_layer=='Community Builder'",
                                                       radioButtons("spp_comm", "Select species group", choices =c("All"="All", .cws_env$fl) ),
                                                       conditionalPanel(condition = "input.data_layer=='Community Builder'",
                                                                        selectInput("species_comm", "Select species to examine.
                                                                To type, click on selected species and hit <backspace>.", .cws_env$all_species$species, multiple = TRUE)),
                                                       conditionalPanel(condition = "input.data_layer=='Species Observations'",
                                                                        # conditionalPanel(condition = "input.source == 'Naturecounts'",
                                                                                         selectInput('breeding', "Select breeding category
                                                                                                     (*indicates derived from recording only)",
                                                                                                     choices = .cws_env$bird_codes,
                                                                                                     selected = .cws_env$bird_codes,
                                                                                                     multiple = T
                                                                                                     ),
                                                                                         # ),
                                                    selectInput("species", "Select species to examine.
                                                                To type, click on selected species and hit <backspace>.", .cws_env$all_species$species),
                                                    conditionalPanel(id="limit-count-panel",condition = "input.limit_count",
                                                                     "Enter limits for counts:",
                                                                     numericInput("lower_count_limits", "Lower limit", value = 1, min = 1, max = 1e6),
                                                                     numericInput("upper_count_limits", "Upper limit", value = 10, min = 1, max = 1e6)
                                                                                                                                          )
                                                       )
                                                    )
                              ),
                              tags$div(id="cite",
                                       'Data compiled from ', tags$em('WildTrax and NatureCounts')
                              )
                          )
                 ),

# Project selection -------------------------------------------------------


           tabPanel("Project Selection",
                    sidebarLayout(
                    sidebarPanel(
                      h1("Select groups here include"),
                      "Selecting only 'All' will include all groups, however if
                      any groups are selected, only those projects will be included",
                      shinyWidgets::awesomeCheckboxGroup('data_collector', "Data Collector",
                                                         choices = c("All",unique(.cws_env$project_status$data_collector)),
                                                         selected = "All" ,inline = F
                      ),
                      shinyWidgets::awesomeCheckboxGroup('data_processor', "Data Processor",
                                                         choices = c("All",unique(.cws_env$project_status$data_processor)),
                                                         selected = "All" ,inline = F
                      ),
                      checkboxGroupInput('project_status', glue::glue("Project Status (as of {.cws_env$comple_date})"),
                                         choices = unique(.cws_env$project_status$project_status),
                                         selected = unique(.cws_env$project_status$project_status))
                    ),
                    mainPanel(
                      h1("Included projects are shown below"),
                      h2("Select a row in the table to exclude"),
                      DT::dataTableOutput("projects"),
                      h2("Excluded projects:"),
                      verbatimTextOutput('print_ex')
                    )
           ) ),

# Download data -----------------------------------------------------------


           tabPanel("View and Download",
                    titlePanel("Download Data"),

                    ## Sidebar layout with input and output definitions ----
                    sidebarLayout(

                      ## Sidebar panel for inputs ----
                      sidebarPanel(
                        h2("Figure download"),
                        p("Select species and spatial subset in 'Interactive map' tab. ",
                        em("Figure is only the species selected current map window or falling under the drawn polygon.")),
                        hr(),
                        sliderInput('image_width', "Image width (cm)", min = 10,max = 30,
                                    step = 2.5, value = 17.5),

                        selectInput('image_res', "Image resolution",
                                    choices = c(72, 300, 600),
                                    multiple = F),

                        downloadButton('downloadImage', 'Download P Obs Plot') ),
                     mainPanel( plotOutput("p_obs_out"))
                      ) ,

                    sidebarLayout(
                    sidebarPanel(
                      h2("Download dataset"),
                      p("Depending on choice, data downloaded will",
                      "use data selected in other tabs"),
                      hr(),
                        ## Show table, logical ----
                        checkboxInput("showtable", "Show table?", FALSE),

                        conditionalPanel(
                          id = "showtable-control",condition = "input.showtable",
                          numericInput('rowtable', "Number of rows to display",
                                       value = 10)
                        ),

                        ## Input: Choose dataset ----
                        selectInput("dataset", "Choose a dataset:",
                                    choices = c("locations", "effort", "species_observations",
                                                "proportion_of_sites_obs")),

                        # Button
                        downloadButton("downloadData", "Download")

                      ) ,

                      ## Main panel for displaying outputs ----
                      mainPanel(
                        conditionalPanel(
                          id = "showtable-output",condition = "input.showtable",
                          DT::dataTableOutput("table") )

                      )

                    )
),
header =
tags$script(HTML(glue::glue("var header = $('.navbar> .container-fluid');
                       header.append('<div style=\"float:right\"><h3>Data compile date: {.cws_env$comple_date}</h3></div>');
                       console.log(header)")))
)
}




