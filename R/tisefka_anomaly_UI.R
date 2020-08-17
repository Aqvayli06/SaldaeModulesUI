#' Saldae Dashboard Module Anomaly Detection Advanced
#' @description
#' @author Farid Azouaou
#' @param id t.b.d
#' @export
SA_anomaly_UI <- function(id,mod_title = NULL ,div_width = "col-xs-12 col-sm-12 col-md-12") {
  ns <- NS(id)
    div(class = div_width,
        shinydashboard::tabBox(width = 12, title = mod_title,
                               tabPanel(icon("table"),

                                        DT::dataTableOutput(ns("tisefka_table"))
                               ),
                               tabPanel(icon("bar-chart"),
                                        uiOutput(ns("select_element")),
                                        dygraphs::dygraphOutput(ns("tisefka_plot"))
                               )

        )
    )


}

#' Saldae Dashboard Module Server
#' @description Saldae Dashboard module SERVER : render and generate object to be displayed data(chart/table)
#' @author Farid Azouaou
#' @param input  input shinydashboard elements containing information to use for output generation
#' @param output output shinydashboard element
#' @param session shiny session
#' @param tisefka reactive object containing data
#' @return output objects to be displayed in corresponding UI module
#' @export

SA_anomaly_mod <- function(input, output, session,tisefka) {
  tisefka_choices <- reactive({
    tisefka()$numeric_variables
  })
  tisefka_anomaly <- reactive({
     SaldaeDataExplorer::anomaly_detection_nnegh(tisefka()$tisefka_tizegzawin,target_ts = tisefka()$numeric_variables,anomaly_mode = "anomalize")
  })
  output$select_element <- renderUI({
    req(tisefka_choices())
    shinyWidgets::pickerInput(inputId = session$ns("variable_picker"),
                              label = "Select target element:",
                              multiple = FALSE,
                              choices = tisefka_choices(),
                              selected = tisefka_choices()[1]
                              )
    })
  #----------------main chart
  output$tisefka_table <- DT::renderDataTable({
    SaldaeDataExplorer::anomaly_to_DT_insight(tisefka_anomaly())
  })
  output$tisefka_plot <- dygraphs::renderDygraph({
    req(input$variable_picker)
    tisefka_anomaly()[[input$variable_picker]]%>%
      SaldaeDataExplorer::SA_anomaly_charter(target_variable = input$variable_picker)
  })
  #---------------
}
