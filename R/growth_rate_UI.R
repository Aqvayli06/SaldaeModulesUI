#------------------------ multiple-select, multiple output
#' Saldae Dashboard Module UI (growth rate)
#' @description Saldae Dashboard module UI : growth rate calculator
#' @author Farid Azouaou
#' @param id  server module ID
#' @param div_width dimension information about the framework(html object)
#' @param mod_title module title (default NULL)
#' @return UI module
#' @export

SA_tisefka_gemmu_UI <- function(id,mod_title = NULL ,div_width = "col-xs-12 col-sm-6 col-md-8") {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(width = 4,uiOutput(ns("select_element")))    ,
      column(width = 3,uiOutput(ns("gemmu_rate_yellan")))
    ),
    fluidRow(
      column(width = 3,uiOutput(ns("submit")))
    ),
    uiOutput(ns("graphs_ui"))
  )
}



#' Saldae Dashboard Module Server Growth Rate
#' @description Saldae Dashboard module SERVER : render and generate multiple output objects for growth rates
#' @author Farid Azouaou
#' @param input  input shinydashboard elements containing information to use for output generation
#' @param output output shinydashboard element
#' @param session shiny session
#' @param tisefka reactive object containing data
#' @param div_width dimension information about the framework(html object)
#' @return output objects to be displayed in corresponding UI module
#' @export

SA_tisefka_gemmu_mod <- function(input, output, session,tisefka,div_width = "col-xs-6 col-sm-12 col-md-6") {
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
    gemmu_yellan <- gemmu_yellan_f(base_unit = ts_time_units()[1])
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
  tisefka_gemmu <- reactive({
    req(input$variable_picker)
    SaldaeDataExplorer::Saldae_rate_n_gemmu_f(tisefka = tisefka_tizegzawin(),target_ts = input$variable_picker,gemmu_iswi = input$gemmu_rate_yellan, base_unit = ts_time_units()[1])
  })

  tisefka_tables <- reactive({
    req(tisefka_gemmu())
    return(purrr::map(.x =tisefka_gemmu(),~reactable::reactable(.x,pagination = FALSE, highlight = TRUE, height = 250))%>%
             stats::setNames(names(tisefka_gemmu())))
  })

  tisefka_yiwen_plots <- reactive({
    req(tisefka_gemmu())
    purrr::map(.x = tisefka_gemmu(),~SaldaeDataExplorer::sekned_gemmu_f(gemu_tisefka = .x))
  })
  #---------------------
  output$graphs_ui <- renderUI({
    req(tisefka_yiwen_plots())
    plots_list <- purrr::imap(tisefka_yiwen_plots(), ~{
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
    req(tisefka_yiwen_plots())
    purrr::map(names(tisefka_yiwen_plots()), ~{
      output_name_plot <- paste0("tisefka_plot_", .x)
      output_name_table <- paste0("tisefka_table_", .x)
      output[[output_name_plot]] <- plotly::renderPlotly(tisefka_yiwen_plots()[[.x]])
      output[[output_name_table]] <- reactable::renderReactable(tisefka_tables()[[.x]])
    })
  })
}
