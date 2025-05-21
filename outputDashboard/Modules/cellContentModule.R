wellPanelModuleUI <- function(id, panelConfig, count) {
  
  ns <- NS(id)
  
  i <- as.numeric(gsub("panel", "", id))
  
  # Initialize defaults -----------------------------------------------------
  panelConfig <- panelConfig %||% list(
    plotType = plotTypes[1],
    title = "",       
    caption = ""     
  )
  
  # Define content panels ---------------------------------------------------
  wellPanel(
    
    ## Top line ----------------------------------------------------------------
    div(
      style = "display: flex; align-items: center; justify-content: space-between; padding: 5px;",
      
      ### Settings ----------------------------------------------------------------
      dropdown(
        h4("Plot Options", style = "color: #F7931D;"),
        hr(),
        pickerInput(
          inputId = ns("plotType"),
          label = 'Plot Type',
          choices = plotTypes,
          selected = panelConfig$plotType,
          options = list(`style` = "btn-default")
        ),
        style = "material-flat",
        icon = icon("bars"),
        status = "warning",
        width = "850%"
      ),
      
      ### Title -------------------------------------------------------------------
      div(
        textInput(ns("title"), label = NULL, value = panelConfig$title %||% NULL, placeholder = "Add figure title", width = "80%"),
        style = "flex-grow: 1; text-align: center; font-weight: 700;"
      ),
      
      ### Remove button -----------------------------------------------------------
      if (count > 1) {
        actionBttn(paste0("removeBtn_", i), icon("trash"), color = "royal", style = "material-flat")
      },
    ),
    
    ## Plot --------------------------------------------------------------------
    plotOutput(ns("plot"), width = "100%", height = "75%"),
    
    ## Caption -----------------------------------------------------------------
    div(
    textAreaInput(ns("caption"), label = NULL, value = panelConfig$caption %||% NULL, placeholder = "Add figure caption...", resize = "none"),
    style = "font-style: italic;"
    ),
    
    class = "contentWell"
    
  )

  # UI End ------------------------------------------------------------------
}

wellPanelModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    plot_labels <- setNames(
      unlist(lapply(plotTypes, names)),
      unlist(plotTypes)
    )
    
    panelConfig <- reactive({
      if (!is.null(input$plotType) && !is.null(input$title) && !is.null(input$caption)) {
        return(list(
          plotType = input$"plotType",
          title = if (!is.null(input$"title") && input$"title" != "") input$"title" else plot_labels[[input$plotType]],
          caption = input$"caption"
        ))
      }
    })
    
    # Render plot -------------------------------------------------------------
    output$plot <- renderPlot({
      req(input$plotType)
      plot_func <- get(input$plotType)
      plot_func()
    })
    
    return(panelConfig())

    # Server End --------------------------------------------------------------
  })
}