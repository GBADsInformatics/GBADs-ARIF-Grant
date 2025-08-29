flatten_plot_list <- function(nested_list) {
  flat <- unlist(nested_list, recursive = FALSE)
  return(flat)
}

wellPanelModuleUI <- function(id, panelConfig, count, plot_list) {
  ns <- NS(id)
  i <- as.numeric(gsub("panel", "", id))
  
  # Flatten plot_list for UI
  flattened_list <- flatten_plot_list(plot_list)
  
  # Initialize defaults
  panelConfig <- panelConfig %||% list(
    plotType = names(flattened_list)[1],
    title = "",
    caption = ""
  )
  
  wellPanel(
    div(
      style = "display: flex; align-items: center; justify-content: space-between; padding: 5px;",
      
      # Dropdown
      dropdown(
        h4("Plot Options", style = "color: #F7931D;"),
        hr(),
        pickerInput(
          inputId = ns("plotType"),
          label = "Plot Type",
          choices = names(flattened_list),
          selected = panelConfig$plotType,
          options = list(
            container = "body",
            dropdownAutoWidth = TRUE
          )
        ),
        style = "material-flat",
        icon = icon("bars"),
        status = "warning",
        width = "850%"
      ),
      
      # Title
      div(
        textInput(ns("title"), label = NULL, value = panelConfig$title %||% NULL, placeholder = "Add figure title", width = "80%"),
        style = "flex-grow: 1; text-align: center; font-weight: 700;"
      ),
      
      # Remove button
      if (count > 1) {
        actionBttn(ns("removeBtn"), icon("trash"), color = "royal", style = "material-flat")
      }
    ),
    
    # Plot output
    uiOutput(ns("plot_ui"), height = "100%"),
    
    # Caption and download
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
}


wellPanelModuleServer <- function(id, plot_list, data, db_type) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Flatten once at the beginning
    flattened_list <- flatten_plot_list(plot_list)

    # Reactive config
    panelConfig <- reactive({
      if (!is.null(input$plotType) && !is.null(input$title) && !is.null(input$caption)) {
        list(
          plotType = input$plotType,
          title = if (!is.null(input$title) && input$title != "") input$title else input$plotType,
          caption = input$caption
        )
      }
    })

    # Render UI wrapper for plots
    output$plot_ui <- renderUI({
      req(input$plotType, data())
      plot_func <- flattened_list[[input$plotType]]
      plot_output <- plot_func(data())

      if (inherits(plot_output, "plotly")) {
        div(style = "width: 100%; height: 100%;", plotlyOutput(ns("plotly_obj"), width = "100%", height = "380px"))
      } else if (inherits(plot_output, "gg") || inherits(plot_output, "ggplot")) {
        div(style = "width: 100%; height: 100%;", plotOutput(ns("ggplot_obj"), width = "100%", height = "380px"))
      } else if (inherits(plot_output, "flextable")) {
        div(style = "overflow-x: auto; overflow-y: auto; padding: 5px; height: 380px;", htmlOutput(ns("flextable_obj"), inline = TRUE))
      } else {
        verbatimTextOutput(ns("plot_fallback"))
      }
    })

    output$ggplot_obj <- renderPlot({
      req(input$plotType, data())
      plot_func <- flattened_list[[input$plotType]]
      plot_func(data())
    })

    output$plotly_obj <- renderPlotly({
      req(input$plotType, data())
      plot_func <- flattened_list[[input$plotType]]
      plot_func(data())
    })

    output$flextable_obj <- renderUI({
      req(input$plotType, data())
      plot_func <- flattened_list[[input$plotType]]
      ft <- plot_func(data())
      flextable::htmltools_value(ft)
    })

    # Download handler
    plot_output <- reactive({
      req(input$plotType, data())
      plot_func <- flattened_list[[input$plotType]]
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
        } else {
          writeLines(capture.output(print(p)), con = file)
        }
      }
    )
    
    remove_trigger <- reactive(input$removeBtn)
    
    return(list(
      config = panelConfig,
      remove = remove_trigger
    ))
  })
}
