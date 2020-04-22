#------------------------ multiple-select, multiple output
#' Saldae Dashboard Module UI (analytics)
#' @description Saldae Dashboard module UI : forecasting
#' @author Farid Azouaou
#' @param id  server module ID
#' @param div_width dimension information about the framework(html object)
#' @param mod_title module title (default NULL)
#' @return UI module
#' @export

SA_tisefka_forecast_UI <- function(id,mod_title = NULL ,div_width = "col-xs-12 col-sm-6 col-md-8") {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(width = 4,uiOutput(ns("select_element")))
    ),
    fluidRow(
      column(width = 3,uiOutput(ns("submit")))
    ),
    uiOutput(ns("graphs_ui"))
  )
}



#' Saldae Dashboard Module Server Analytics
#' @description Saldae Dashboard module SERVER : render and generate multiple output objects for analytics
#' @author Farid Azouaou
#' @param input  input shinydashboard elements containing information to use for output generation
#' @param output output shinydashboard element
#' @param session shiny session
#' @param tisefka reactive object containing data
#' @param div_width dimension information about the framework(html object)
#' @return output objects to be displayed in corresponding UI module
#' @export

SA_tisefka_forecast_mod <- function(input, output, session,tisefka,div_width = "col-xs-6 col-sm-12 col-md-6") {
  tisefka_choices <- reactive({
    tisefka()$numeric_variables
  })
  tisefka_tizegzawin <- reactive({
    tisefka()$tisefka_tizegzawin
  })
  ts_time_units <- reactive({
    tisefka()$ts_time_units
  })

  output$submit <- renderUI({
    shinyWidgets::actionBttn(
      inputId = session$ns("submit"),
      style = "stretch",
      color = "primary",
      label = "Update output")
  })
  #----------- select variable
  output$select_element <- renderUI({
    req(tisefka_choices())
    shinyWidgets::pickerInput(inputId = session$ns("variable_picker"),
                              label = "Select target element:",
                              multiple = TRUE,
                              choices = tisefka_choices()
    )
  })


  output$gemmu_rate_yellan <- renderUI({
    req(ts_time_units())
    gemmu_yellan <- SaldaeDataExplorer::gemmu_yellan_f(base_unit = ts_time_units()[1])
    shinyWidgets::radioGroupButtons(
      inputId = session$ns("gemmu_rate_yellan"),
      label = "Possible rates",
      choices = gemmu_yellan,
      justified = FALSE,
      status = "success",
      checkIcon = list(
        yes = shiny::icon("ok",
                          lib = "glyphicon"
        )
      )
    )
  })

  #----------------
  tisefka_forecast_aqerru <- reactive({
    req(input$variable_picker)
    a
    SaldaeForecasting::Saldae_Forecaster(tisefka = tisefka_tizegzawin(),target_variables = input$variable_picker, anomaly_detection = TRUE, Saldae_model = "saldae_prophet")
  })

  tisefka_forecast <- reactive({
    req(tisefka_forecast_aqerru())
    purrr::map(.x= tisefka_forecast_aqerru(),  ~SaldaeForecasting::sbed_forecast_aqerru(.x , asurif_arzdat = NULL))
  })


  tisefka_tables <- reactive({
    req(tisefka_forecast())
    return(purrr::map(.x =tisefka_forecast(),~reactable::reactable(.x,pagination = FALSE, highlight = TRUE, height = 250))%>%
             stats::setNames(names(tisefka_forecast())))
  })

  tisefka_plots <- reactive({
    req(tisefka_forecast())
    purrr::map(.x =names(tisefka_forecast()),~SaldaeForecasting::sekned_forecast_aqeru(fcast_df =  tisefka_forecast()[[.x]],target_variable = .x))%>%
      stats::setNames(names(tisefka_forecast()))
  })
  #---------------------
  output$graphs_ui <- renderUI({
    req(tisefka_plots())
    plots_list <- purrr::imap(tisefka_plots(), ~{
      tagList(
        div(class = div_width,
            shinydashboard::tabBox(width = 12, title = .y,
                                   tabPanel(icon("bar-chart"),
                                            plotly::plotlyOutput(session$ns(paste0("tisefka_plot_",.y)), height = "250px")
                                   ),
                                   tabPanel(icon("table"),
                                            reactable::reactableOutput(session$ns(paste0("tisefka_table_",.y)))
                                   )
            )
        ),
        br()
      )
    })
    tagList(plots_list)
  })
  observeEvent(input$submit, {
    req(tisefka_plots())
    purrr::map(names(tisefka_plots()), ~{
      output_name_plot <- paste0("tisefka_plot_", .x)
      output_name_table <- paste0("tisefka_table_", .x)
      output[[output_name_plot]] <- plotly::renderPlotly(tisefka_plots()[[.x]])
      output[[output_name_table]] <- reactable::renderReactable(tisefka_tables()[[.x]])
    })
  })
}
