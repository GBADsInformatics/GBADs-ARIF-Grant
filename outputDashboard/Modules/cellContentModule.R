wellPanelModuleUI <- function(id, panelConfig, count, plot_list) {
  
  ns <- NS(id)
  i <- as.numeric(gsub("panel", "", id))
  
  # Initialize defaults -----------------------------------------------------
  panelConfig <- panelConfig %||% list(
    plotType = plot_list[1],
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
          choices = plot_list,
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
    uiOutput(ns("plot_ui"), height = "100%"),
    
    ## Caption -----------------------------------------------------------------
    div(
      style = "display: flex; align-items: center; gap: 10px; font-style: italic; padding-right: 10px;",
      
      textAreaInput(
        ns("caption"),
        label = NULL,
        value = panelConfig$caption %||% NULL,
        placeholder = "Add figure caption...",
        resize = "none",
        width = "90%"
      ),
      downloadButton(
        ns("download_plot"),
        label = NULL,
        icon = icon("download"),
        class = "btn btn-outline-secondary btn-sm"
      )
    ),
    
    class = "contentWell"
    
  )

  # UI End ------------------------------------------------------------------
}

wellPanelModuleServer <- function(id, plot_list, data, db_type) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    plot_labels <- setNames(
      unlist(lapply(plot_list, names)),
      unlist(plot_list)
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
    output$plot_ui <- renderUI({
      req(input$plotType)
      req(data())
      
      plot_func <- get(input$plotType)
      
      if (db_type() == "ahle") {
        req(is.list(data()))
        req(!is.null(data()$data_full), !is.null(data()$summary), !is.null(data()$results))
        plot_output <- plot_func(data())
      } else {
        req(is.data.frame(data()))
        plot_output <- plot_func(data())
      }
      
      if (inherits(plot_output, "plotly")) {
        div(
          style = "width: 100%; height: 100%;",
          plotlyOutput(ns("plotly_obj"), width = "100%", height = "380px")
        )
      } else if (inherits(plot_output, "gg") || inherits(plot_output, "ggplot")) {
        div(
          style = "width: 100%; height: 100%;",
          plotOutput(ns("ggplot_obj"), width = "100%", height = "380px")
        )
      } else if (inherits(plot_output, "flextable")) {
        div(
          style = "overflow-x: auto; overflow-y: auto; padding: 5px; height: 380px;",
          htmlOutput(ns("flextable_obj"), inline = TRUE)
        )
      } else {
        verbatimTextOutput(ns("plot_fallback"))
      }
    })
    
    output$ggplot_obj <- renderPlot({
      req(input$plotType, data())
      plot_func <- get(input$plotType)
      if (db_type() == "ahle") {
        plot_func(data())
      } else {
        plot_func(data())
      }
    })
    
    output$plotly_obj <- renderPlotly({
      req(input$plotType, data())
      plot_func <- get(input$plotType)
      if (db_type() == "ahle") {
        plot_func(data())
      } else {
        plot_func(data())
      }
    })
    
    output$flextable_obj <- renderUI({
      req(input$plotType, data())
      plot_func <- get(input$plotType)
      if (db_type() == "ahle") {
        ft <- plot_func(data())
      } else {
        ft <- plot_func(data())
      }
      flextable::htmltools_value(ft)
    })
    
    
    # Save plots
    plot_output <- reactive({
      plot_func <- get(input$plotType)
      plot_func(data())
    })
    
    output$download_plot <- downloadHandler(
      filename = function() {
        p <- plot_output()
        if (inherits(p, "gg") || inherits(p, "ggplot")) {
          paste0("plot_", Sys.Date(), ".png")
        } else if (inherits(p, "flextable")) {
          paste0("table_", Sys.Date(), ".html")
        } else {
          paste0("output_", Sys.Date(), ".txt")
        }
      },
      content = function(file) {
        p <- plot_output()
        if (inherits(p, "gg") || inherits(p, "ggplot")) {
          ggsave(filename = file, plot = p, width = 8, height = 6, dpi = 300)
        } else if (inherits(p, "flextable")) {
          flextable::save_as_html(p, path = file)
        }
      }
    )
    
    return(panelConfig())

    # Server End --------------------------------------------------------------
  })
}