
#' Saldae Dashboard Module UI
#' @description Saldae Dashboard module UI : data upload
#' @author Farid Azouaou
#' @param id  server module ID
#' @export
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
  #---- Help text
  uiOutput(ns("date_variable_help")),

  #---- Data Overview
  div(class = "col-xs-12 col-sm-12 col-md-12",
      shinydashboard::tabBox(width = 12, title = "Data Diagnosis",
                             tabPanel(title = "Overview",icon = icon("eye"),
                                      rhandsontable::rHandsontableOutput(ns("tisefka_view"))
                             ),
                             tabPanel(title = "Description",icon = icon("table"),
                                      rhandsontable::rHandsontableOutput(ns("tisefka_description"))
                             ),
                             tabPanel(title = "Outliers",icon = icon("table"),
                                      rhandsontable::rHandsontableOutput(ns("tisefka_outliers"))
                             )
      )
  )
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
  output$date_variable_help <- renderUI({
    req(tisefka())
    my_help_text <- h3("Once data is uploaded, you need to specify the variable to use for time (Date)", style = "color:navy")
    helpText(my_help_text)
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
      choices = SaldaeDataExplorer::SA_date_format_yellan(),
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
data_summary <- reactive({
  req(tisefka_tizegzawin())
    diag_output <- SaldaeDataExplorer::data_diagnosis_f(tisefka = tisefka_tizegzawin(), categoricals_ukud = NULL)
    return(diag_output)
})

numeric_variables <- reactive({
    req(data_summary())
    dat_diag <- data_summary()$diagnosis
    numericals <- dat_diag[grepl("numeric", dat_diag$types), "variables", drop = T]
    multi_integers <- dat_diag[grepl("integer", dat_diag$types), c("unique_count", "variables"), drop = T]
    if (nrow(multi_integers)) {
      multi_integers <- multi_integers[multi_integers["unique_count"] > 10, "variables", drop = T]
    } else {
      multi_integers <- NULL
    }
    return(c(numericals, multi_integers))
})

tisefka_overview <- reactive({
  req(data_summary())
  SaldaeDataExplorer::Handson_exploration(tisefka = tisefka_tizegzawin(), tisefka_report = data_summary(),numeric_variables = numeric_variables())
})
#-------------------------
output$tisefka_description <- rhandsontable::renderRHandsontable({
  req(data_summary())
  return(rhandsontable::rhandsontable(data_summary()$beschreibung, rowHeaders = NULL, width = 1000, height = 300))
})
output$tisefka_outliers <- rhandsontable::renderRHandsontable({
  req(tisefka_overview())
  return(rhandsontable::rhandsontable(data_summary()$outliers, rowHeaders = NULL, width = 1000, height = 300))
})
output$tisefka_view <- rhandsontable::renderRHandsontable({
  req(tisefka_overview())
  return(tisefka_overview())
})
ts_time_units <- reactive({
  return(SaldaeDataExplorer::possible_units_for_summary(time_vect = rownames(tisefka_tizegzawin())))
})

explore_output <- reactive({
  req(tisefka_overview())
  output <- list()
  output$tisefka_tizegzawin <- dplyr::tbl_df(tisefka_tizegzawin())
  output$tisefka_overview <- tisefka_overview()
  output$numeric_variables <- numeric_variables()
  output$ts_time_units <- ts_time_units()
  return(output)
})

}
