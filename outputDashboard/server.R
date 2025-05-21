server <- function(input, output, session) {
  
  
  # Initiate reactive values ------------------------------------------------
  data <- loadData("./Data/2025-02-12-1111-Ethiopia_Cattle.json")
  dashboardInfo <- data$Info
  panelConfigs <- reactiveVal(data$Panels)
  panelCount <- reactiveVal(length(data$Panels))
  
  output$dashboardName <- renderText(paste("Viewing outputs from:", dashboardInfo$dashboardName))
  
  # Render UI ---------------------------------------------------------------
  output$dynamicUI <- renderUI({
    
    panelList <- panelConfigs()
    count <- panelCount()
    
    divs <- lapply(seq_len(panelCount()), function(i) {
      panel_id <- as.character(i)
      panelConfig <- panelList[[panel_id]] %||% list()
      
      tags$div(
        class = "cell col-lg-4 col-md-6 col-sm-12",
        wellPanelModuleUI(paste0("panel", i), panelConfig = panelConfig, count = count)
      )
    })
    
    if (count < 6) {
      divs <- append(divs, list(
        tags$div(
          class = "cell col-lg-4 col-md-6 col-sm-12",
          actionButton("addCell", "+", class = "addBtn")
        )
      ))
    }
    
    tags$div(class = "container-fluid",
             tags$div(class = "row", divs)
    )
  })

# Save configurations function --------------------------------------------

  saveConfigurations <- function() {
    lapply(1:panelCount(), function(i) {
      panelConfig <- wellPanelModuleServer(id = paste0("panel", i))
      currentConfig <- panelConfigs()
      currentConfig[[i]] <- panelConfig
      panelConfigs(currentConfig)
    })
  }  
  
  # "Add" button logic ------------------------------------------------------
  observeEvent(input$addCell, {
    saveConfigurations()
    count <- panelCount()
    if (count < 6) {
      panelCount(count + 1)
      
      newPanelConfig <- list(
        plotType = "Bar",
        title = "",
        caption = ""
      )
      
      panelList <- panelConfigs()
      panelList[[as.character(count + 1)]] <- newPanelConfig
      
      panelConfigs(panelList)
    }
  })
  
  # "Remove" button logic ---------------------------------------------------
  removePanel <- function(index) {
    observeEvent(input[[paste0("removeBtn_", index)]], {
      count <- panelCount()
      
      saveConfigurations()
      
      panelList <- panelConfigs()
      panelList <- panelList[-index]
      names(panelList) <- seq(1: length(panelList))
      
      panelCount(count - 1)
      
      panelConfigs(panelList)
      
    })
  }
  
  lapply(1:6, removePanel)
  
  # Run cell content module server ------------------------------------------
observe({
    lapply(1:panelCount(), function(i) {
      wellPanelModuleServer(id = paste0("panel", i))
    })
  })
  

# Save JSON ---------------------------------------------------------------
  
  output$download <- downloadHandler(
    filename = function() {
      paste( Sys.Date(), "-", dashboardInfo$userID, "-", dashboardInfo$modelID, ".json", sep = "")
    },
    content = function(file) {
      saveConfigurations()
      current_configs <- panelConfigs()
      
      outputFile <- list(
        "Info" = dashboardInfo,
        "Panels" = current_configs
      )
      
      json_data <- toJSON(outputFile, pretty = TRUE)
      
      writeLines(json_data, file)
    }
  )
  
  
  
}