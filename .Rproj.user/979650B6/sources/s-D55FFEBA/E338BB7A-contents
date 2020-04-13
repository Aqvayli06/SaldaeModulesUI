
#' Saldae Dashboard Module UI: value boxes
#' @description Saldae Dashboard module UI :  value boxes  in a row containing
#' @author Farid Azouaou
#' @param id  server module ID
#' @return n  value boxes objects
#' @export

SA_Value_box_UI <- function(id){
  ns <- NS(id)
  uiOutput(ns("valuebox_output"))
}

#' Saldae Dashboard Module Server
#' @description Saldae Dashboard module SERVER : render and generate 3 value boxes output
#' @param input  input shinydashboard elements containing information to use for output generation
#' @param output output shinydashboard element
#' @param session shiny session
#' @param tisefka reactive object containing  value data
#' @return output objects to be displayed in corresponding UI module
#' @export
SA_Value_box_server <- function(input, output, session,tisefka) {
output$valuebox_output <- renderUI({
  fluidRow(
    flexdashboard::valueBox(value ="Farid",caption = "Saldae Value Box 1",icon="fa-thumbs-down"),
    flexdashboard::valueBox(value = "Farid",caption = "Saldae Value Box 2",icon="fa-thumbs-down"),
    flexdashboard::valueBox(value = "tisefka()[3]",caption = "Saldae Value Box 3",icon="fa-thumbs-down")
  )
})
}
