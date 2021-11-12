#' Run Survival Analysis App
#'
#' @return shiny application object
#'
#' @export
survivalApp <- function(){
  shiny::shinyApp(ui, server)
}
