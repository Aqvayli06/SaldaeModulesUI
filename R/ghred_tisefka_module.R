
#' Saldae Dashboard Module UI
#' @description Saldae Dashboard module UI : data upload
#' @author Farid Azouaou
#' @param id  server module ID
ghred_tisefka_UI <- function(id){
  ns <- NS(id)
fluidPage(
  fluidRow(
    column(width = 3,
           fileInput(inputId = ns("tisefka_file"), label = "Choose CSV File",
                     multiple = FALSE,
                     accept = c("csv")
           )),
    column(width= 3,
           shinyWidgets::radioGroupButtons(
             inputId = ns("tisefka_tala"),
             label = "Data Source :",
             choices = c(
               `<i class="fas fa-table"></i>` = "CSV", `<i class="fas fa-database"></i>` = "Database",
               `<i class="fas fa-file-excel"></i>` = "EXCEL"
             ),
             status = "success",
             justified = TRUE,
             selected = "CSV"
           )
    )
  ),
  #-----------date related settings
  fluidRow(column(width = 3,
                  uiOutput(ns("date_variable"))),
           column(width = 3,
                  uiOutput(ns("SA_date_format")))
           ),
  reactable::reactableOutput(ns("tisefka_view"))
)

}

#' Saldae dashboard module: upload data
#' @description upload rwa data and prepare it to be used for exploration and analysis
#' @author Farid Azouaou
#' @param input  input shinydashboard elements containing information to use data upload
#' @param output output shinydashboard element
#' @param session shiny session
#' @return output objects to be displayed in corresponding UI module
#' @export

ghred_tisefka_mod <-function(input, output, session){

  tisefka <- reactive({
    req(input$tisefka_file)
    SaldaeDataExplorer::ghred_tisefka_aqerru(input_file = input$tisefka_file, tala = input$tisefka_tala, tawriqt = NULL)
  })

  #------- select date variable
  output$date_variable <- renderUI({
    req(tisefka())
    dates_yellan <- colnames(tisefka())
    # dates_yellan <- text_similarity_f(target_text = dates_yellan, text_benchmark = "DATE")
    shinyWidgets::pickerInput(
      inputId = session$ns("date_variable"),
      label = "Choose Date variable:",
      choices = dates_yellan,
      options = list(
        style = "btn-primary"
      )
    )
  })
  output$SA_date_format <- renderUI({
    req(input$date_variable)
    shinyWidgets::pickerInput(
      inputId = session$ns("SA_date_format"),
      label = "Choose Date format:",
      choices = SA_date_format_yellan(),
      options = list(
        style = "btn-primary"
      )
    )
  })

  tisefka_tizegzawin <- reactive({
    req(input$SA_date_format)
    SaldaeDataExplorer::sbed_tisefka(tisefka = tisefka(), date_variable = input$date_variable, SA_date_format = input$SA_date_format, spread_key = input$tisefka_spread, spread_value = input$tisefka_spread_var)
  })

#--------- display raw data
  output$tisefka_view <- reactable::renderReactable({
    req(tisefka_tizegzawin())
    return(reactable::reactable(data = tisefka_tizegzawin(),pagination = FALSE, highlight = TRUE, height = 400))
  })
#--------- Data summary
  # data_summary <- reactive({
  #   req(tisefka())
  #   diag_output <- data_diagnosis_f(tisefka = tisefka(), categoricals_ukud = tisefka_tizegzawin_d_ukud()$ukud_units)
  #   return(diag_output)
  # })

#-------
}
