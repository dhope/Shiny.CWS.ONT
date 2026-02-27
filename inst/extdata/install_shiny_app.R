# usage:
# source("https://raw.githubusercontent.com/LandSciTech/sdmEvaluationTool/refs/heads/main/setup.R")

message("=========================================")
message("   Installing the CWS Ontario Shiny App")
message("=========================================")
message("------ Downloading results --------------")
if(!file.exists("forShiny.zip")) stop("Download 'forShiny.zip' and place in working directory")
message("------ Unzipping contents ---------------")
unzip("./forShiny.zip")
message("------ Installing R packages ------------")
if (!requireNamespace("remotes")) {
  install.packages("remotes")
}
if (!requireNamespace("rstudioapi")) {
  install.packages("rstudioapi")
}
remotes::install_github(
  "dhope/Shiny.CWS.ONT",
  dependencies = TRUE
)
file.copy(system.file("extdata/run_shiny_app.R",package = 'Shiny.CWS.ONT'), ".")
if(Sys.getenv("RSTUDIO") == "1"){
  rstudioapi::navigateToFile("run_shiny_app.R")}
message("------ Done! ----------------------------")
message("Source the 'run_shiny_app.R' file to update and run the app")
