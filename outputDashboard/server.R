server <- function(input, output, session) {
  
  # Initiate reactive values ------------------------------------------------
  default_config_single <- loadData("./Data/default_config_single.json")
  default_config_ahle   <- loadData("./Data/default_config_ahle.json")
  
  panelConfigs <- reactiveVal(default_config_single$Panels)
  panelCount   <- reactiveVal(length(default_config_single$Panels))
  dashboardInfo <- reactiveVal(default_config_single$Info)
  
  
  db_type <- reactiveVal("single")
  plot_list <- reactiveVal(plot_list_single_run)

  single_model_name <- reactiveVal("20240601T121530_user123_CattleScenario1_current_20240615T094400_full.csv") #default for now
  
  current_model_name <- reactiveVal(NULL)
  ideal_model_name <- reactiveVal(NULL)
  zeromort_model_name <- reactiveVal(NULL)
  zeromorb_model_name <- reactiveVal(NULL)
  
  data <- reactiveVal(NULL)
  
  
  observe({
    if (db_type() == "single") {
      plot_list(plot_list_single_run)
    } else {
      plot_list(plot_list_ahle)
    }
  })
  
  observeEvent(db_type(), {
    if (db_type() == "ahle") {
      panelConfigs(default_config_ahle$Panels)
      panelCount(length(default_config_ahle$Panels))
      dashboardInfo(default_config_ahle$Info)
    } else {
      panelConfigs(default_config_single$Panels)
      panelCount(length(default_config_single$Panels))
      dashboardInfo(default_config_single$Info)
    }
  })
  
  observe({
    if (db_type() == "single" && !is.null(single_model_name())) {
      # --- SINGLE MODEL RUN ---
      file_path <- file.path("data", single_model_name())
      
      if (file.exists(file_path)) {
        df <- read.csv(file_path, stringsAsFactors = FALSE)
        df <- summarize_data(df)  # assumes this prepares it for plotting
        data(df)
        
      } else {
        
        data(NULL)
        
      }
      
    } else if (db_type() == "ahle") {
      
      session$sendCustomMessage("showLoading", NULL)
      
      file_list <- list(
        Current   = file.path("data", current_model_name()),
        Ideal     = file.path("data", ideal_model_name()),
        ZeroMort  = file.path("data", zeromort_model_name()),
        ZeroMorb  = file.path("data", zeromorb_model_name())
      )
      
      valid_items <- c(
        "Feed Cost", "Gross Margin", "Health Cost", "Labour Cost", "Population",
        "Value of Herd Increase", "Value of Manure", "Value of Milk", "Value of Offtake"
      )
      
      num_cores <- parallel::detectCores(logical = FALSE) - 1
      cl <- parallel::makeCluster(num_cores)
      doParallel::registerDoParallel(cl)
      
      all_data <- foreach(scenario = names(file_list), .combine = bind_rows, .packages = c("dplyr", "stringr", "tidyr")) %dopar% {
        read.csv(file_list[[scenario]]) %>%
          pivot_longer(cols = -c(X), names_to = "Iteration", values_to = "Value") %>%
          mutate(
            Scenario = scenario,
            Iteration = str_remove(Iteration, "^V"),
            X = str_replace_all(X, c("_SubAF" = "_SF", "_SubAM" = "_SM")),
            Group_tag = str_extract(X, "[^_]+$"),
            Group = if_else(Group_tag %in% c("JF", "JM", "SF", "SM", "AF", "AM"), Group_tag, "Overall"),
            X = str_remove_all(X, "_(JF|JM|SF|SM|AF|AM)"),
            X = str_replace(X, "Num_N", "Num"),
            Item = recode(X,
                          "Feed_cost" = "Feed Cost",
                          "Gross_margin" = "Gross Margin",
                          "Health_cost" = "Health Cost",
                          "Labour_cost" = "Labour Cost",
                          "Num" = "Population",
                          "Value_herd_increase" = "Value of Herd Increase",
                          "Value_manure" = "Value of Manure",
                          "Value_milk" = "Value of Milk",
                          "Value_offtake" = "Value of Offtake")
          ) %>%
          filter(Item %in% valid_items) %>%
          select(-X, -Group_tag)
      }
      
      stopCluster(cl)
      
      df_full    <- preprocessing_data_full(all_data)
      df_summary <- preprocessing_data_summary(df_full)
      df_results <- preprocessing_ahle_results(df_full)
      
      data(list(
        data_full = df_full,
        summary   = df_summary,
        results   = df_results
      ))
      
    } else {
      data(NULL)
    }
  })
  
  
# User Name ---------------------------------------------------------------
  userName <- "Kurtis Sobkowich"
  output$user_name <- renderText(userName)
  
  
# Dashboard Title ---------------------------------------------------------
  titleText <- reactiveVal("CattleScenario1")
  
  # Output the dashboard title
  output$dashboardTitle <- renderText({
    titleText()
  })
  
  # Show modal on click
  observeEvent(input$editTitle, {
    showModal(modalDialog(
      title = "Edit Model Name",
      textInput("newModelName", "Enter model name:", value = titleText()),
      uiOutput("modelNameWarning"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("saveTitle", "Save", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })
  
  # Disable Save button if name is invalid
  observe({
    req(input$newModelName)
    is_invalid <- grepl("[^a-zA-Z0-9_-]", input$newModelName) || input$newModelName == ""
    
    if (is_invalid) {
      disable("saveTitle")
      addClass("newModelName", "invalid-input")
    } else {
      enable("saveTitle")
      removeClass("newModelName", "invalid-input")
    }
  })
  
  # Show warning message
  output$modelNameWarning <- renderUI({
    req(input$newModelName)
    if (grepl("[^a-zA-Z0-9_.-]", input$newModelName)) {
      div("Model name can only contain letters, numbers, underscores (_), or dashes (-).",
          class = "warning-text")
    }
  })
  
  # Save new title
  observeEvent(input$saveTitle, {
    req(input$newModelName)
    titleText(input$newModelName)
    removeModal()
  })
  
  # Render UI ---------------------------------------------------------------
  output$dynamicUI <- renderUI({
    current_plot_list <- plot_list()
    
    panelList <- panelConfigs()
    count <- panelCount()
    
    divs <- lapply(seq_len(panelCount()), function(i) {
      panel_id <- as.character(i)
      panelConfig <- panelList[[panel_id]] %||% list()
      
      tags$div(
        class = "cell col-lg-4 col-md-6 col-sm-12",
        wellPanelModuleUI(paste0("panel", i), panelConfig = panelConfig, count = count, plot_list = current_plot_list)
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
    current_plot_list <- plot_list()
    
    lapply(1:panelCount(), function(i) {
      panelConfig <- wellPanelModuleServer(
        id = paste0("panel", i),
        plot_list = current_plot_list,
        data = data
      )
      
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
  

# "Configure" button logic ------------------------------------------------

  observeEvent(input$configure_db, {
    showModal(
      modalDialog(
        title = tags$h4("Configure Output", style = "margin-bottom: 0;"),
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save_db_config", "Save", class = "btn-primary")
        ),
        
        # Run type selection
        tags$div(
          style = "margin-bottom: 1rem;",
          radioButtons(
            inputId = "run_type",
            label = tags$strong("Select run type:"),
            choices = c("Single run" = "single", "AHLE (Current vs. Ideal)" = "ahle"),
            selected = db_type(),
            inline = TRUE
          )
        ),
        
        # Descriptions for each run type
        conditionalPanel(
          condition = "input.run_type == 'single'",
          tags$p("Display outputs based on a single model run (i.e., 'Ideal' OR 'Current').")
        ),
        conditionalPanel(
          condition = "input.run_type == 'ahle'",
          tags$p("Display outputs comparing the results of a 'Current' and an 'Ideal' model run. Select this option to calculate metrics based on the Animal Health Loss Envelope (AHLE).")
        ),
        
        # Single run selection UI
        conditionalPanel(
          condition = "input.run_type == 'single'",
          tags$div(
            tags$label("Choose a model:"),
            selectInput(
              inputId = "single_model_choice",
              label = NULL,
              choices = single_model_labels
            )
          )
        ),
        
        # AHLE run selection UI
        conditionalPanel(
          condition = "input.run_type == 'ahle'",
          tags$div(
            tags$label("Choose a model:"),
            selectInput(
              inputId = "ahle_model_choice",
              label = NULL,
              choices = unique(group_model_labels)
            )
          )
        )
      )
    )
  })
  
  observeEvent(input$save_db_config, {
    removeModal()
    db_type(input$run_type)
    
    if (input$run_type == "single") {
      selected_label <- input$single_model_choice
      selected_file <- unname(named_choices[selected_label])
      
      single_model_name(selected_file)

      current_model_name(NULL)
      ideal_model_name(NULL)
      zeromort_model_name(NULL)
      zeromorb_model_name(NULL)
      
    } else if (input$run_type == "ahle") {
      selected_group <- input$ahle_model_choice
      
      model_df <- tibble(
        file = file_names,
        group = parse_model_group(file_names)
      ) %>%
        filter(!is.na(group), group == selected_group)
      
      matching_files <- model_df$file
      
      # Set each scenario-specific file name
      current_model_name(matching_files[grepl("_current_", matching_files)])
      ideal_model_name(matching_files[grepl("_ideal_", matching_files)])
      zeromort_model_name(matching_files[grepl("_zeromort_", matching_files)])
      zeromorb_model_name(matching_files[grepl("_zeromorb_", matching_files)])
      
      # Clear single model selection
      single_model_name(NULL)
    }
  })
  
  # Run cell content module server ------------------------------------------
  observe({
    req(plot_list())
    req(panelCount() > 0)
    
    current_plot_list <- isolate(plot_list())
    
    current_data <- data
    
    lapply(seq_len(panelCount()), function(i) {
      wellPanelModuleServer(
        id = paste0("panel", i),
        plot_list = current_plot_list,
        data = current_data,
        db_type = db_type
      )
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