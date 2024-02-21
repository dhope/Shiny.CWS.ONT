#' Shiny app for exploring CWS ONT data
#'
#' @param ...
#'
#' @return Runs app
#' @export
#'
CWS_ON_app <- function(...){
  shinyApp(ui, server, ...)
}

#' Run the shiny app
#'
#' @param ...
#'
#' @return
#' @export
run_shiny_app <- function(...){
  runApp(CWS_ON_app(),...)
}
