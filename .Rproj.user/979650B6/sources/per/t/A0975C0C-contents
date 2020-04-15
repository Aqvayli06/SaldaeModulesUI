#' Saldae Dashboard Module plotly rawdata single
#' @description Plot single variable
#' @author Farid Azouaou
#' @param tisefka raw data
#' @param variable_inu variable to plot
#' @param graph_type  plot type  "bar" "scatter" "line" "hist",..
#' @return plotly interactive object
mod_sekned_yiwet_tisefka <- function(tisefka = NULL,variable_inu=NULL,graph_type="scatter"){
  y <- list(title = variable_inu)
  tisefka%>%plotly::plot_ly(x = ~date,y = ~base::get(variable_inu),name =variable_inu ,type = graph_type) %>%
    plotly::layout(yaxis = y)%>%plotly::config(displaylogo = F)
}
#' Saldae Dashboard Module plotly rawdata
#' @description multiple plots : data x-axis date and y-axis numerical variable
#' @author Farid Azouaou
#' @param tisefka raw data
#' @param target_variables variables to plot
#' @param graph_type plot type  "bar" "scatter" "line" "hist",..
#' @return plotly interactive object
#' @export

mod_sekned_tisefka_iceqfan <- function(tisefka = NULL,target_variables= NULL,graph_type = NULL){
  plotlist_inu <- target_variables%>%purrr::map(function(x)mod_sekned_yiwet_tisefka(tisefka =tisefka ,variable_inu = x,graph_type = graph_type))
  sub_rows <- switch (length(plotlist_inu),
    "1" = 0,"2" = 1,"3" = 2,"4" = 2,"5" = 2,"6" = 2,"7" = 3, "8" = 3, "9" = 3,"10" = 4,"11" = 4, "12" = 4
  )
  if(sub_rows >0){
    plotlist_inu <- plotly::subplot(titleX = TRUE,titleY = TRUE,
      plotlist_inu,nrows = sub_rows,margin = 0.04
    )
  }else{
    plotlist_inu<- plotlist_inu[[1]]
  }
  plotlist_inu <- plotlist_inu%>%plotly::layout(legend = list(orientation = "h", x = 0.35, y = 100))%>%
    plotly::config(displaylogo = F)
  return(plotlist_inu)
}

#' Saldae Dashboard Module UI
#' @description Saldae Dashboard module UI : display data(chart/table)
#' @author Farid Azouaou
#' @param id  server module ID
#' @param div_width dimension information about the framework(html object)
#' @param mod_title module title (default NULL)
#' @return UI module
#' @export

SA_tisefka_UI <- function(id,mod_title = NULL ,div_width = "col-xs-12 col-sm-12 col-md-12") {
  ns <- NS(id)
    fluidPage(fluidRow(
             column(width = 5 ,uiOutput(ns("select_element"))),
             column(width = 3 ,uiOutput(ns("graph_type")))
    ),
    div(class = div_width,
        shinydashboard::tabBox(width = 12, title = mod_title,
                               tabPanel(icon("bar-chart"),
                                        plotly::plotlyOutput(ns("tisefka_plot"))
                               ),
                               tabPanel(icon("table"),
                                        reactable::reactableOutput(ns("tisefka_table"))
                               )
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

SA_tisefka_mod <- function(input, output, session,tisefka) {
  tisefka_choices <- reactive({
    tisefka()$numeric_variables
  })
  tisefka_tizegzawin <- reactive({
    tisefka()$tisefka_tizegzawin
  })
  output$select_element <- renderUI({
    req(tisefka_tizegzawin())
    shinyWidgets::pickerInput(inputId = session$ns("variable_picker"),
                              label = "Select target element:",
                              multiple = TRUE,
                              choices = tisefka_choices(),
                              selected = tisefka_choices()[1]
                              )
    })
  #--------------- chart type
  output$graph_type <- renderUI({
    req(tisefka_tizegzawin())
    plot_choices <- c(
      `<i class='fa fa-line-chart'></i>` = "scatter", `<i class='fas fa-circle'></i>` = "bar", `<i class='fa fa-line-chart'></i>` = "Lines+Markers",
      `<i class='fas fa-chart-area'></i>` = "Filled", `<i class='fa fa-bar-chart'></i>` = "Bar", `<i class='fas fa-bell'></i>` = "Density"
    )

    shinyWidgets::radioGroupButtons(
      inputId = session$ns("graph_type"),
      label = "Select Chart Type:",
      choices = plot_choices,
      justified = FALSE,
      status = "success",
      selected = plot_choices[1]
    )
  })
  #----------------main chart
  output$tisefka_table <- reactable::renderReactable({
    req(input$variable_picker)
    return(reactable::reactable(tisefka_tizegzawin()%>%dplyr::select(!!input$variable_picker), pagination = FALSE, highlight = TRUE, height = 250))
  })
  output$tisefka_plot <- plotly::renderPlotly({
    req(input$variable_picker)
      return(mod_sekned_tisefka_iceqfan(tisefka = tisefka_tizegzawin(),target_variables = input$variable_picker,graph_type = input$graph_type))
  })
  #---------------
}

#------------------------ multiple-select, multiple output
#' Saldae Dashboard Module UI
#' @description Saldae Dashboard module UI : display data(chart/table) multipleoutputs
#' @author Farid Azouaou
#' @param id  server module ID
#' @param div_width dimension information about the framework(html object)
#' @param mod_title module title (default NULL)
#' @return UI module
#' @export

SA_tisefka_multiple_UI <- function(id,mod_title = NULL ,div_width = "col-xs-12 col-sm-6 col-md-8") {
  ns <- NS(id)
  fluidPage(fluidRow(
    column(width = 4,uiOutput(ns("select_element")))    ,
    column(width = 3,uiOutput(ns("graph_type")))    ,
    column(width = 3,uiOutput(ns("submit")))
  ),
  uiOutput(ns("graphs_ui"))
  )
}

#' Saldae Dashboard Module Server
#' @description Saldae Dashboard module SERVER : render and generate multiple output objects (chart/table)
#' @author Farid Azouaou
#' @param input  input shinydashboard elements containing information to use for output generation
#' @param output output shinydashboard element
#' @param session shiny session
#' @param tisefka reactive object containing data
#' @param div_width dimension information about the framework(html object)
#' @return output objects to be displayed in corresponding UI module
#' @export

SA_tisefka_multiple_mod <- function(input, output, session,tisefka,div_width = "col-xs-6 col-sm-12 col-md-4") {
  tisefka_choices <- reactive({
    tisefka()$numeric_variables
  })
  tisefka_tizegzawin <- reactive({
    tisefka()$tisefka_tizegzawin
  })

  output$submit <- renderUI({
    shinyWidgets::actionBttn(
      inputId = session$ns("submit"),
      style = "stretch",
      color = "primary",
      label = "Update output")
  })
  output$select_element <- renderUI({
    shinyWidgets::pickerInput(inputId = session$ns("variable_picker"),
                              label = "Select target element:",
                              multiple = TRUE,
                              choices = tisefka_choices()
    )
  })
  #--------------- chart type
  output$graph_type <- renderUI({
    plot_choices <- c(
      `<i class='fa fa-line-chart'></i>` = "scatter", `<i class='fas fa-circle'></i>` = "bar", `<i class='fa fa-line-chart'></i>` = "Lines+Markers",
      `<i class='fas fa-chart-area'></i>` = "Filled", `<i class='fa fa-bar-chart'></i>` = "Bar", `<i class='fas fa-bell'></i>` = "Density"
    )
    shinyWidgets::radioGroupButtons(
      inputId = session$ns("graph_type"),
      choices = plot_choices,
      label = "Select Chart Type:",
      justified = FALSE,
      status = "success",
      selected = plot_choices[1]
    )
  })
  #----------------
tisefka_tables <- reactive({
    req(tisefka_tizegzawin())
    a <- purrr::map(input$variable_picker,~tisefka_tizegzawin()%>%dplyr::select(!!.x))
    a <- purrr::map(a,~reactable::reactable(.x,pagination = FALSE, highlight = TRUE, height = 250))%>%
      stats::setNames(input$variable_picker)
    return(a)
  })

tisefka_yiwen_plots <- reactive({
    req(tisefka_tizegzawin())
    purrr::imap(input$variable_picker,~mod_sekned_yiwet_tisefka(tisefka = tisefka_tizegzawin(),variable_inu = .x,graph_type = input$graph_type))%>%
      stats::setNames(input$variable_picker)
})


#---------------------
output$graphs_ui <- renderUI({
  req(tisefka_yiwen_plots())
  # div_width = "col-xs-12 col-sm-12 col-md-4"
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
