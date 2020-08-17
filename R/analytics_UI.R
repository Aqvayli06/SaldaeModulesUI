key_value_calculator <- function(tisefka = NULL,key_value =NULL){
  tisefka <- na.omit(tisefka)
  if(key_value == "Sum"){
    my_value <- sum(tisefka,na.rm = TRUE)
  }
  if(key_value == "Average"){
    my_value <- colMeans(tisefka,na.rm = TRUE)
  }
  if(key_value == "Maximum"){
    my_value <- max(tisefka,na.rm = TRUE)
  }
  if(key_value == "Minimum"){
    my_value <- min(tisefka,na.rm = TRUE)
  }
  if(key_value == "First Value"){
    my_value <- head(tisefka,1)
  }
  if(key_value == "Last Value"){
    my_value<- tail(tisefka,1)
  }
  return(round(my_value,3))
}

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
      column(width = 4,uiOutput(ns("select_element"))),
      column(width = 4,uiOutput(ns("SA_outliers"))),
      column(width = 3,uiOutput(ns("submit")))
    ),
    fluidRow(
      column(width = 4,uiOutput(ns("SA_key_figure_select")))
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

  output$SA_outliers <- renderUI({
    shinyWidgets::prettySwitch(
      inputId = session$ns("SA_outliers"),
      label = "Outliers detection",
      status = "success",
      fill = TRUE)
  })
  output$submit <- renderUI({
    req(tisefka_choices())
    shinyWidgets::actionBttn(
      inputId = session$ns("submit"),
      style = "material-flat",
      color = "primary",
      label = "Start Predictions")%>%shinyhelper::helper(type = "markdown",buttonLabel="Got it",
                                                         # icon= shiny::icon("fa-lightbulb"),
                                                         colour = "green",
                                                         content = "sald_forecast")
  })
  #----------- select variable
  output$select_element <- renderUI({
    req(tisefka_choices())
    shinyWidgets::pickerInput(inputId = session$ns("select_element"),
                              label = "Select target element:",
                              multiple = TRUE,
                              choices = tisefka_choices()
    )
  })


  #----------------
  tisefka_forecast_aqerru <- eventReactive(input$submit,{
    req(tisefka_tizegzawin())
      tisefka_forecast_aqerru <- SaldaeForecasting::Saldae_Forecaster(tisefka = tisefka_tizegzawin(),target_variables = input$select_element, anomaly_detection = input$SA_outliers, Saldae_model = "saldae_prophet")
  })

  tisefka_forecast <- reactive({
    req(tisefka_forecast_aqerru())
    purrr::map(.x= tisefka_forecast_aqerru(),  ~SaldaeForecasting::sbed_forecast_aqerru(.x , asurif_arzdat = NULL))
  })


  tisefka_plots <- reactive({
    purrr::map(.x =names(tisefka_forecast()),~SaldaeForecasting::sekned_forecast_aqeru(fcast_df =  tisefka_forecast()[[.x]],target_variable = .x))%>%
      stats::setNames(names(tisefka_forecast()))
  })

  tisefka_tables <- reactive({
    req(tisefka_forecast())
    return(purrr::map(.x =tisefka_forecast(),~DT::datatable(.x,extensions = 'Scroller', options = list(deferRender = TRUE, scrollY = 200, scroller = TRUE)) )%>%
             stats::setNames(names(tisefka_forecast())))
  })
  output$SA_key_figure_select <- renderUI({
    req(tisefka_forecast())
    key_figures_choices <- c("Average","Sum","Maximum","Minimum","First Value","Last Value")
    shinyWidgets::pickerInput(
      inputId = session$ns("SA_key_figure_select"),
      label = "Saldae Key Numbers:",
      choices = key_figures_choices,
      multiple = FALSE
    )
  })
  #---------------------

  observeEvent(eventExpr=tisefka_tables(),handlerExpr= {
    purrr::map(names(tisefka_plots()), ~{
      output_name_plot <- paste0("tisefka_plot_", .x)
      output_name_table <- paste0("tisefka_table_", .x)
      output_name_figures <- paste0("tisefka_key_figures_", .x)
      output[[output_name_table]] <- DT::renderDataTable(tisefka_tables()[[.x]])
      output[[output_name_plot]] <- plotly::renderPlotly(tisefka_plots()[[.x]])

      output[[output_name_figures]] <- shinydashboard::renderInfoBox({
        my_title <- paste(.x,":",input$SA_key_figure_select)
        shinydashboard::infoBox(title = my_title,
                                value = my_analytics_key_values()[[.x]],color = "maroon",
                                width = 6,
                                shiny::icon("bar-chart")
        )
      })
      #
    })
  })

  output$graphs_ui <- renderUI({
    req(tisefka_plots())
    plots_list <- purrr::imap(tisefka_plots(), ~{
      tagList(
        div(class = div_width,
            shinydashboard::tabBox(width = 12, title = .y,
                                   tabPanel(icon("bar-chart"),
                                            plotly::plotlyOutput(session$ns(paste0("tisefka_plot_",.y)), height = "250px")
                                   ),tabPanel(icon("table"),
                                              DT::dataTableOutput(session$ns(paste0("tisefka_table_",.y)))
                                   ),tabPanel(icon("align-left"),
                                              shiny::textAreaInput(inputId = session$ns(paste0("tisefka_awal_",.y)),label = "Comments",value = "insert your comments here",width = "100%",height = "50%")
                                   ),tabPanel(icon("percentage"),
                                              fluidRow(
                                                column(width = 6,
                                                       shinydashboard::infoBoxOutput(session$ns(paste0("tisefka_key_figures_",.y)))
                                                       )
                                              )
                                   )

            )
        ),
        br()
      )
    })
    tagList(plots_list)
  })
  my_analytics_key_values <- reactive({
    my_value <- purrr::map(names(tisefka_forecast()),~key_value_calculator(tisefka = tisefka_forecast()[[.x]][,"forecast"],key_value = input$SA_key_figure_select))%>%
              stats::setNames(names(tisefka_forecast()))
  })
  analytics_output <- reactive({
    req(tisefka_plots())
    output <- list()
    output$analytics_plots    <- tisefka_plots()
    output$analytics_tisefka  <- tisefka_forecast()
    output$analytics_awal <- purrr::map(names(tisefka_plots()),~ input[[paste0("tisefka_awal_",.x)]])%>%stats::setNames(names(tisefka_plots()))
    output$analytics_key_figures <- list(key_metric = input$SA_key_figure_select,
                                         key_figures =  my_analytics_key_values())

    output$analytics_settings <- "ulac"
    return(output)
  })
}
