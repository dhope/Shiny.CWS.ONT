#' Shiny app for exploring CWS ONT data
#'
#' @param ...
#'
#' @return Retuns shiny app
#' @export
#'
CWS_ON_app <- function(...){
  shinyApp(ui_fun(), server, ...)
}

#' Run the shiny app
#'
#' @param ...
#'
#' @return Runs app
#' @export
run_shiny_app <- function(...){
  runApp(CWS_ON_app(),...)
}
