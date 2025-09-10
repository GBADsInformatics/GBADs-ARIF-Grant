# ==============================================================================
# Output Dashboard Module
# ==============================================================================

# ----------------------------- Helpers (module-scope) -------------------------
# Pretty label from filename root
clean_model_label <- function(model_names) {
  if (is.null(model_names)) return("")
  stringr::str_replace_all(
    model_names,
    c(
      "(_ideal)$"          = " (Ideal)",
      "(_current)$"        = " (Current)",
      "(_zeroMorbidity)$"  = " (Zero Morbidity)",
      "(_zeroMortality)$"  = " (Zero Mortality)"
    )
  )
}

parse_model_name <- function(filenames) { str_match(
  filenames, 
  "^(.*_(?:ideal|current|zeroMorbidity|zeroMortality))_full\\.csv$" )[, 2] 
}

# Extract group from filenames: everything before variant tag
# Accept: current|ideal|zeroMort|zeroMortality|zeroMorb|zeroMorbidity
parse_model_group <- function(filenames) {
  m <- stringr::str_match(
    filenames,
    "^(.*)_(?:current|ideal|zeroMort(?:ality)?|zeroMorb(?:idity)?)_full\\.csv$"
  )
  m[, 2]
}

# Utility: robust detection of the ID/metric column for wide AHLE CSVs
.detect_id_col <- function(df) {
  preferred <- c("X","Item","Variable","Metric","Name","Label","Key")
  id <- intersect(preferred, names(df))
  if (length(id)) return(id[1])
  
  # choose a character/factor column with highest distinct ratio
  char_cols <- names(df)[vapply(df, function(col) is.character(col) || is.factor(col), logical(1))]
  char_cols <- setdiff(char_cols, ".__scenario__.")
  if (length(char_cols)) {
    distinct_ratio <- vapply(char_cols, function(nm) dplyr::n_distinct(df[[nm]])/max(1, nrow(df)), numeric(1))
    return(char_cols[which.max(distinct_ratio)])
  }
  
  # fallback: first non-iteration, non-scenario column
  looks_iter <- function(nm) grepl("^V\\d+$", nm) || grepl("^\\d+$", nm) || grepl("^(Iter|Iteration)\\d+$", nm, ignore.case=TRUE)
  non_scen <- setdiff(names(df), ".__scenario__.")
  cand <- non_scen[!looks_iter(non_scen)]
  if (length(cand)) return(cand[1])
  non_scen[1]
}

.unify_ahle_long <- function(raw_list, debug = FALSE) {
  stopifnot(length(raw_list) > 0)
  raw <- dplyr::bind_rows(raw_list)
  
  if (debug) {
    cat("\n[DBG] AHLE CSV STRUCTURES\n")
    for (i in seq_along(raw_list)) {
      nm <- raw_list[[i]]$.__scenario__.[1]
      df <- raw_list[[i]]
      cat(sprintf("  [%s] nrow=%d ncol=%d\n", nm, nrow(df), ncol(df)))
      cat("    names: ", paste(head(names(df), 20), collapse=", "), if (ncol(df) > 20) ", ..." else "", "\n", sep="")
      cat("    types: ", paste(vapply(df, function(x) class(x)[1], character(1))[1:min(20, ncol(df))], collapse=", "), "\n", sep="")
    }
    cat("  [BIND] nrow=", nrow(raw), " ncol=", ncol(raw), " | names: ",
        paste(head(names(raw), 30), collapse=", "), if (ncol(raw) > 30) ", ..." else "", "\n", sep="")
  }
  
  # ---- If already long-ish, standardize ----
  tolower_names <- tolower(names(raw))
  has_value_col <- "value" %in% tolower_names
  has_item_col  <- any(tolower_names %in% c("item","variable","metric","name","x"))
  has_scen_col  <- ".__scenario__." %in% names(raw)
  
  if (has_scen_col && has_item_col && has_value_col) {
    if (debug) cat("[DBG] Detected long format; standardizing.\n")
    nm_map <- setNames(names(raw), tolower(names(raw)))
    pick_first <- function(keys) {
      k <- keys[keys %in% tolower(names(raw))]
      if (length(k)) nm_map[[k[1]]] else NULL
    }
    
    scen_col <- ".__scenario__."
    val_col  <- pick_first(c("value"))
    iter_col <- pick_first(c("iteration","iter"))
    item_col <- pick_first(c("item","variable","metric","name","x"))
    group_col<- pick_first(c("group","group_tag","grp"))
    
    if (is.null(item_col) || is.null(val_col)) stop("Long-like data missing 'item' or 'value'.")
    
    out <- raw
    if (is.null(iter_col)) { out[["Iteration"]] <- "1"; iter_col <- "Iteration" }
    
    out[[iter_col]] <- as.character(out[[iter_col]])
    out[[iter_col]] <- sub("^V","", out[[iter_col]])
    out[[item_col]] <- stringr::str_replace_all(out[[item_col]], c("_SubAF"="_SF","_SubAM"="_SM"))
    
    out <- out |>
      dplyr::mutate(
        Scenario  = .data[[scen_col]],
        Iteration = .data[[iter_col]],
        X         = .data[[item_col]],
        Group_tag = dplyr::coalesce(.data[[group_col]], "Overall"),
        Group     = dplyr::if_else(Group_tag %in% c("JF","JM","SF","SM","AF","AM"), Group_tag, "Overall"),
        X_key     = stringr::str_remove_all(.data[[item_col]], "_(JF|JM|SF|SM|AF|AM)"),
        X_key     = stringr::str_replace(X_key, "Num_N", "Num"),
        Item      = dplyr::recode(
          X_key,
          "Feed_cost"           = "Feed Cost",
          "Gross_margin"        = "Gross Margin",
          "Health_cost"         = "Health Cost",
          "Labour_cost"         = "Labour Cost",
          "Num"                 = "Population",
          "Value_herd_increase" = "Value of Herd Increase",
          "Value_manure"        = "Value of Manure",
          "Value_milk"          = "Value of Milk",
          "Value_offtake"       = "Value of Offtake",
          .default              = NA_character_
        ),
        Value = suppressWarnings(as.numeric(.data[[val_col]]))
      ) |>
      dplyr::filter(!is.na(Item)) |>
      dplyr::select(Scenario, Iteration, X, Group, Item, Value)
    
    if (debug) cat("[DBG] Long path → rows=", nrow(out), " cols=", ncol(out), "\n", sep="")
    return(out)
  }
  
  .looks_iter <- function(nm) grepl("^V\\d+$", nm) || grepl("^\\d+$", nm) || grepl("^(Iter|Iteration)\\d+$", nm, ignore.case=TRUE)
  
  id_col <- {
    preferred <- c("X","Item","Variable","Metric","Name","Label","Key")
    id <- intersect(preferred, names(raw))
    if (length(id)) id[1] else {
      char_cols <- names(raw)[vapply(raw, function(col) is.character(col) || is.factor(col), logical(1))]
      char_cols <- setdiff(char_cols, ".__scenario__.")
      if (length(char_cols)) {
        distinct_ratio <- vapply(char_cols, function(nm) dplyr::n_distinct(raw[[nm]])/max(1, nrow(raw)), numeric(1))
        char_cols[which.max(distinct_ratio)]
      } else {
        non_scen <- setdiff(names(raw), ".__scenario__.")
        cand <- non_scen[!vapply(non_scen, .looks_iter, logical(1))]
        if (length(cand)) cand[1] else non_scen[1]
      }
    }
  }
  
  non_id <- setdiff(names(raw), c(id_col, ".__scenario__."))
  iter_like   <- non_id[vapply(non_id, .looks_iter, logical(1))]
  numeric_like<- non_id[vapply(raw[non_id], function(col) is.numeric(col) || is.integer(col), logical(1))]
  
  value_cols <- unique(c(iter_like, numeric_like))

  if (!length(value_cols)) {
    value_cols <- non_id
    if (debug) cat("[DBG] Fallback: using ALL non-id/non-scenario columns as value cols.\n")
  }
  
  if (debug) {
    cat(sprintf("[DBG] Wide path using id_col='%s'; value_cols[1..10]=%s\n",
                id_col, paste(head(value_cols, 10), collapse=", ")))
  }
  
  out <- tidyr::pivot_longer(
    raw,
    cols = dplyr::all_of(value_cols),
    names_to = "Iteration",
    values_to = "Value"
  ) |>
    dplyr::mutate(
      Scenario  = .data[[".__scenario__."]],
      Iteration = as.character(Iteration),
      Iteration = dplyr::if_else(grepl("^V\\d+$", Iteration), sub("^V","",Iteration), Iteration),
      "{id_col}" := stringr::str_replace_all(.data[[id_col]], c("_SubAF"="_SF","_SubAM"="_SM")),
      Group_tag = stringr::str_extract(.data[[id_col]], "[^_]+$"),
      Group     = dplyr::if_else(Group_tag %in% c("JF","JM","SF","SM","AF","AM"), Group_tag, "Overall"),
      X_key     = stringr::str_remove_all(.data[[id_col]], "_(JF|JM|SF|SM|AF|AM)"),
      X_key     = stringr::str_replace(X_key, "Num_N", "Num"),
      Item      = dplyr::recode(
        X_key,
        "Feed_cost"           = "Feed Cost",
        "Gross_margin"        = "Gross Margin",
        "Health_cost"         = "Health Cost",
        "Labour_cost"         = "Labour Cost",
        "Num"                 = "Population",
        "Value_herd_increase" = "Value of Herd Increase",
        "Value_manure"        = "Value of Manure",
        "Value_milk"          = "Value of Milk",
        "Value_offtake"       = "Value of Offtake",
        .default              = NA_character_
      )
    ) |>
    dplyr::filter(!is.na(Item)) |>
    dplyr::select(-Group_tag, -.__scenario__.)
  
  suppressWarnings(out$Value <- as.numeric(out$Value))
  if (id_col != "X") out <- dplyr::rename(out, X = dplyr::all_of(id_col))
  
  if (debug) cat("[DBG] Wide path → rows=", nrow(out), " cols=", ncol(out), "\n", sep="")
  out
}

# Validator used by UI
.has_valid_data <- function(obj, db_type) {
  if (is.null(obj)) return(FALSE)
  if (db_type == "single") {
    is.data.frame(obj) && nrow(obj) > 0
  } else {
    is.list(obj) &&
      all(c("data_full","summary","results") %in% names(obj)) &&
      is.data.frame(obj$data_full) && nrow(obj$data_full) > 0
  }
}

# ----------------------------- UI --------------------------------------------
outputDashboardUI <- function(id) {
  ns <- NS(id)
  
  fixedPage(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('download-plotly', function(plotOutputId) {
        var el = document.getElementById(plotOutputId);
        if (el) {
          var plot = el.children[0];
          if (plot && typeof Plotly !== 'undefined') {
            Plotly.downloadImage(plot, {
              format: 'png',
              filename: 'plotly_export',
              height: 600,
              width: 800,
              scale: 2
            });
          }
        }
      });
    ")),
    
    # Header
    fluidRow(
      column(
        width = 4, style = "padding-top: 40px;",
        div(
          style = "display:flex; flex-direction:column; align-items:flex-start;",
          div(
            style = "margin-top:-10px; margin-left:20px; display:flex; align-items:center; gap:8px;",
            h3(textOutput(ns("dashboardTitle")), style = "margin:0; font-weight:bold;"),
            actionButton(ns("editTitle"), label = NULL, icon = icon("pencil-alt"), class = "edit-icon")
          ),
          div(
            style = "margin-left:20px;",
            p(tags$span(style="font-weight:bold;","Created by: "),
              textOutput(ns("user_name"), inline = TRUE))
          )
        )
      ),
      column(
        width = 8, style = "padding-top: 20px;",
        div(
          style = "display:flex; flex-direction:column; align-items:flex-end;",
          h4("Dashboard Details"),
          uiOutput(ns("dashboardMeta")),
          div(
            actionButton(ns("configure_db"), "Configure",
                         style = "font-family: raleway; font-weight:bold; background-color:#FFF; color:#F7931D; border:1px solid #F7931D;"),
            actionButton(ns("download"), "Save Dashboard",
                         style = "font-family: raleway; font-weight:bold; background-color:#F7931D; color:#fff")
          )
        )
      )
    ),
    
    hr(), br(style = "height: 30px;"),
    
    withSpinner(
      uiOutput(ns("dynamicUI")),
      type = 6, color = "#F7931D"
    )
  )
}

# ----------------------------- Server ----------------------------------------
outputDashboardServer <- function(id, user_data, DEBUG = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---------------- Empty-state UI helper ----------------
    empty_state_ui <- function(title = "No valid model runs found", msg = NULL) {
      if (is.null(msg)) {
        msg <- HTML("You do not have any valid model runs at this moment.<br>
                     If you have not run a model yet, please do so in the <b>Models</b> tab.
                     If you have, please wait a moment and then click <b>Refresh</b>.")
      }
      tags$div(
        style = "display:flex; align-items:center; justify-content:center; min-height:260px;",
        tags$div(
          style = "max-width:720px; width:100%; border:1px solid #f0f0f0; border-radius:16px; padding:28px; box-shadow:0 2px 8px rgba(0,0,0,0.04); background:#fff;",
          tags$div(
            style = "display:flex; gap:16px; align-items:flex-start;",
            tags$div(
              style = "flex:0 0 auto; width:56px; height:56px; border-radius:999px; display:flex; align-items:center; justify-content:center; background:#FFF6EA; color:#F7931D; font-size:26px;",
              shiny::icon("triangle-exclamation")
            ),
            tags$div(
              style = "flex:1 1 auto;",
              tags$h4(title, style = "margin:0 0 6px 0; font-weight:700;"),
              tags$p(msg, style = "margin:0; color:#444; line-height:1.35;"),
              tags$div(style="margin-top:14px;",
                       actionButton(ns("refreshOutputsNow"), "Refresh", class = "lightBtn"))
            )
          )
        )
      )
    }
    
    # ---------------- Files from Bucket (outputs) ----------------
    outputs_files <- reactiveVal(character(0))
    
    refresh_outputs_list <- function() {
      uid <- user_data()[["user_id"]]
      if (is.null(uid) || is.na(uid)) return(invisible())
      files <- list_outputs_files_from_api(bucket = "outputs", user_id = uid)
      files <- files[grepl("_full\\.csv$", files, ignore.case = TRUE)]
      outputs_files(files)
    }
    
    observe({ req(user_data()); refresh_outputs_list() })
    observeEvent(input$refreshOutputsNow, {
      refresh_outputs_list()
      showNotification("Checking for new model outputs…", type = "message", duration = 2)
    })
    
    outputs_have_any <- reactive({ length(outputs_files()) > 0 })
    
    # ---------------- Initial reactive values ----------------
    default_config_single <- loadData("./Data/default_config_single.json")
    default_config_ahle   <- loadData("./Data/default_config_ahle.json")
    
    panelConfigs  <- reactiveVal(default_config_single$Panels)
    panelCount    <- reactiveVal(length(default_config_single$Panels))
    dashboardInfo <- reactiveVal(default_config_single$Info)
    
    db_type   <- reactiveVal("single")
    plot_list <- reactiveVal(plot_list_single_run)
    
    single_model_name   <- reactiveVal(NULL)
    current_model_name  <- reactiveVal(NULL)
    ideal_model_name    <- reactiveVal(NULL)
    zeromort_model_name <- reactiveVal(NULL)
    zeromorb_model_name <- reactiveVal(NULL)
    
    data <- reactiveVal(NULL)
    child_handles <- reactiveVal(list())
    
    observe({
      if (db_type() == "single") plot_list(plot_list_single_run) else plot_list(plot_list_ahle)
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
    
    # ---------------- Data loader (Outputs) ----------------
    has_valid_data <- reactive({
      .has_valid_data(data(), db_type())
    })
    
    observe({
      uid <- user_data()[["user_id"]]
      req(db_type(), uid)
      
      # ---- SINGLE ----
      if (db_type() == "single" && nzchar(single_model_name() %||% "")) {
        df <- download_csv_from_api(bucket = "outputs", user_id = uid, file_name = single_model_name())
        if (is.null(df)) { data(NULL); return() }
        df <- try(summarize_data(df), silent = TRUE)
        if (inherits(df, "try-error") || is.null(df)) { data(NULL); return() }
        data(df)
        return()
      }
      
      # ---- AHLE ----
      if (db_type() == "ahle") {
        flist <- list(
          Current  = current_model_name(),
          Ideal    = ideal_model_name(),
          ZeroMort = zeromort_model_name(),
          ZeroMorb = zeromorb_model_name()
        )
        if (any(vapply(flist, function(x) is.null(x) || !nzchar(x), logical(1)))) {
          if (DEBUG) cat("[ERR] AHLE: missing one or more scenario filenames\n")
          data(NULL); return()
        }
        
        raw_list <- lapply(names(flist), function(scn) {
          df <- download_csv_from_api(bucket = "outputs", user_id = uid, file_name = flist[[scn]])
          if (is.null(df)) {
            if (DEBUG) cat("[ERR] AHLE: download returned NULL for", scn, "->", flist[[scn]], "\n")
            return(NULL)
          }
          df$.__scenario__. <- scn
          df
        })
        if (any(vapply(raw_list, is.null, logical(1)))) { data(NULL); return() }
        
        all_data <- try(.unify_ahle_long(raw_list, debug = DEBUG), silent = TRUE)
        if (inherits(all_data, "try-error") || is.null(all_data)) {
          showNotification("Failed to reshape AHLE data. Check CSV structure.", type="error", duration=6)
          if (DEBUG) print(all_data)
          data(NULL); return()
        }
        if (DEBUG) cat("[DBG] unique(Scenario) in all_data: ", paste(unique(all_data$Scenario), collapse=", "), "\n")
        
        df_full    <- try(preprocessing_data_full(all_data),   silent = TRUE)
        df_summary <- try(preprocessing_data_summary(df_full), silent = TRUE)
        df_results <- try(preprocessing_ahle_results(df_full), silent = TRUE)
        
        if (any(vapply(list(df_full, df_summary, df_results),
                       function(x) inherits(x, "try-error") || is.null(x), logical(1)))) {
          showNotification("One of the AHLE preprocessors failed.", type="error", duration=6)
          if (DEBUG) {
            if (inherits(df_full, "try-error") || is.null(df_full))     cat("[ERR] preprocessing_data_full failed\n")
            if (inherits(df_summary, "try-error") || is.null(df_summary)) cat("[ERR] preprocessing_data_summary failed\n")
            if (inherits(df_results, "try-error") || is.null(df_results)) cat("[ERR] preprocessing_ahle_results failed\n")
          }
          data(NULL); return()
        }
        
        data(list(data_full = df_full, summary = df_summary, results = df_results))
        if (DEBUG) .log_has_valid_ahle(data())
        return()
      }
      
      data(NULL)
    })
    
    # ---------------- User name ----------------
    ud <- reactive({ req(user_data()); user_data() })
    output$user_name <- renderText({
      paste(ud()[["firstname"]], ud()[["lastname"]])
    })
    
    # ---------------- Dashboard Title ----------------
    titleText <- reactiveVal("Untitled_Dashboard")
    output$dashboardTitle <- renderText(titleText())
    
    observeEvent(input$editTitle, {
      showModal(modalDialog(
        title = "Edit Title",
        textInput(ns("newDBName"), "Name your dashboard:", value = titleText()),
        uiOutput(ns("dbNameWarning")),
        footer = tagList(modalButton("Cancel"), actionButton(ns("saveTitle"), "Save", class = "btn-primary")),
        easyClose = TRUE
      ))
    })
    
    observe({
      req(input$newDBName)
      is_invalid <- grepl("[^a-zA-Z0-9_-]", input$newDBName) || input$newDBName == ""
      if (is_invalid) {
        if (requireNamespace("shinyjs", quietly = TRUE)) shinyjs::disable("saveTitle")
        addClass("newDBName", "invalid-input")
      } else {
        if (requireNamespace("shinyjs", quietly = TRUE)) shinyjs::enable("saveTitle")
        removeClass("newDBName", "invalid-input")
      }
    })
    
    output$dbNameWarning <- renderUI({
      req(input$newDBName)
      if (grepl("[^a-zA-Z0-9_.-]", input$newDBName)) {
        div("Model name can only contain letters, numbers, underscores (_), or dashes (-).", class = "warning-text")
      }
    })
    
    observeEvent(input$saveTitle, {
      req(input$newDBName); titleText(input$newDBName); removeModal()
    })
    
    # ---------------- Dashboard Details ----------------
    output$dashboardMeta <- renderUI({
      req(db_type())
      tagList(
        p(tags$span(style="font-weight:bold;","Run type: "),
          if (db_type() == "single") "Single scenario" else "AHLE"),
        if (db_type() == "single") {
          tags$div(
            style = "text-align:left; line-height:1.2; max-width:300px; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;",
            p(
              tags$span(style="font-weight:bold;", "Scenario: "),
              tags$span(style="font-weight:normal;", clean_model_label(single_model_name())),
              style="margin:0;"
            )
          )
        } else {
          tags$div(
            style = "text-align:left; line-height:1.2; max-width:300px; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;",
            p(
              tags$span(style="font-weight:bold;", "Model: "),
              tags$span(style="font-weight:normal;", clean_model_label(current_model_name())),
              style="margin:0;"
            )
          )
        }
      )
    })
    
    # ---------------- Main UI (panels & waiting states) ----------------
    output$dynamicUI <- renderUI({
      if (!outputs_have_any()) {
        return(empty_state_ui())
      }
      if (!has_valid_data()) {
        return(tags$div(
          style = "display:flex; align-items:center; justify-content:center; min-height:200px; color:#666;",
          tags$div(
            style = "text-align:center;",
            tags$div(style="font-size:20px; font-weight:600; margin-bottom:6px;", "Waiting for data…"),
            tags$div("Choose a model in ", tags$b("Configure"), " to populate the dashboard.")
          )
        ))
      }
      
      current_plot_list <- plot_list()
      panelList <- panelConfigs()
      count <- panelCount()
      
      divs <- lapply(seq_len(count), function(i) {
        panel_id <- as.character(i)
        thisConfig <- panelList[[panel_id]] %||% list()
        tags$div(
          class = "cell col-lg-4 col-md-6 col-sm-12",
          wellPanelModuleUI(
            ns(paste0("panel", i)),
            panelConfig = thisConfig,
            count = count,
            plot_list = current_plot_list
          )
        )
      })
      
      if (count < 6) {
        divs <- append(divs, list(
          tags$div(class = "cell col-lg-4 col-md-6 col-sm-12",
                   actionButton(ns("addCell"), "+", class = "addBtn"))
        ))
      }
      
      tags$div(class = "container-fluid", tags$div(class = "row", divs))
    })
    
    # ---------------- Panel config save/restore ----------------
    saveConfigurations <- function() {
      handles <- child_handles()
      if (!length(handles)) return(invisible())
      current <- panelConfigs()
      for (i in seq_along(handles)) current[[as.character(i)]] <- handles[[i]]$config()
      panelConfigs(current)
    }
    
    observeEvent(input$addCell, {
      saveConfigurations()
      count <- panelCount()
      if (count < 6) {
        panelCount(count + 1)
        newPanelConfig <- list(plotType = "Bar", title = "", caption = "")
        panelList <- panelConfigs()
        panelList[[as.character(count + 1)]] <- newPanelConfig
        panelConfigs(panelList)
      }
    })
    
    # ---------------- Configure modal ----------------
    modal_open <- reactiveVal(FALSE)
    
    observeEvent(input$configure_db, {
      # Always re-pull the latest output file list when opening the modal
      refresh_outputs_list()
      modal_open(TRUE)
      
      showModal(
        modalDialog(
          title = tags$h4("Configure Output", style = "margin-bottom: 0;"),
          size = "m",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("save_db_config"), "Save", class = "btn-primary")
          ),
          
          # Run type
          tags$div(
            style = "margin-bottom: 1rem;",
            radioButtons(
              inputId = ns("run_type"),
              label = tags$strong("Select run type:"),
              choices = c("Single run" = "single", "AHLE (Current vs. Ideal)" = "ahle"),
              selected = db_type(),
              inline = TRUE
            )
          ),
          
          # Descriptions
          conditionalPanel(
            condition = sprintf("input['%s'] == 'single'", ns("run_type")),
            tags$p("Display outputs based on a single model run (i.e., 'Ideal' OR 'Current').")
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'ahle'", ns("run_type")),
            tags$p("Display outputs comparing the results of a 'Current' and an 'Ideal' model run. Select this to calculate Animal Health Loss Envelope (AHLE) metrics.")
          ),
          
          # SINGLE selection
          conditionalPanel(
            condition = sprintf("input['%s'] == 'single'", ns("run_type")),
            tags$div(
              tags$label("Choose a scenario:"),
              selectInput(ns("single_model_choice"), label = NULL, choices = NULL, selectize = TRUE),
              uiOutput(ns("single_empty_msg"))
            )
          ),
          
          # AHLE selection (group)
          conditionalPanel(
            condition = sprintf("input['%s'] == 'ahle'", ns("run_type")),
            tags$div(
              tags$label("Choose a model group:"),
              selectInput(ns("ahle_model_choice"), label = NULL, choices = NULL, selectize = TRUE),
              uiOutput(ns("ahle_empty_msg"))
            )
          )
        )
      )
      
      # Populate selects using the freshly updated list
      files <- outputs_files() %||% character(0)
      
      # Single-run choices
      if (length(files)) {
        updateSelectInput(
          session, "single_model_choice",
          choices  = files,
          selected = if (db_type() == "single" && nzchar(single_model_name() %||% "")) single_model_name() else files[1]
        )
      } else {
        updateSelectInput(session, "single_model_choice", choices = character(0), selected = NULL)
      }
      
      # AHLE group choices (derived from filenames)
      if (length(files)) {
        groups <- unique(stats::na.omit(parse_model_group(files)))
        updateSelectInput(
          session, "ahle_model_choice",
          choices  = groups,
          selected = if (db_type() == "ahle" && !is.null(input$ahle_model_choice)) input$ahle_model_choice else groups[1]
        )
      } else {
        updateSelectInput(session, "ahle_model_choice", choices = character(0), selected = NULL)
      }
      
      # Enable/disable Save based on selections
      validate_modal <- function() {
        rt <- input$run_type %||% "single"
        ok <- if (rt == "single") {
          length(files) > 0 && nzchar(input$single_model_choice %||% "")
        } else {
          length(files) > 0 && nzchar(input$ahle_model_choice %||% "")
        }
        if (requireNamespace("shinyjs", quietly = TRUE)) {
          if (ok) shinyjs::enable(ns("save_db_config")) else shinyjs::disable(ns("save_db_config"))
        }
      }
      if (requireNamespace("shinyjs", quietly = TRUE)) validate_modal()
      observeEvent(list(input$run_type, input$single_model_choice, input$ahle_model_choice), {
        if (requireNamespace("shinyjs", quietly = TRUE)) validate_modal()
      }, ignoreInit = TRUE)
      
      output$single_empty_msg <- renderUI({
        if (!length(files)) tags$div(class="text-muted", "No output files found yet.")
      })
      output$ahle_empty_msg <- renderUI({
        if (!length(files)) tags$div(class="text-muted", "No output files found yet.")
      })
    })
    
    # Save selection
    observeEvent(input$save_db_config, {
      if (!outputs_have_any()) {
        showNotification("No outputs available yet. Please run a model in the Models tab.", type = "error", duration = 4)
        return()
      }
      
      modal_open(FALSE)  # stop polling as we close the modal
      removeModal()
      
      db_type(input$run_type)
      
      if (input$run_type == "single") {
        single_model_name(input$single_model_choice)
        current_model_name(NULL); ideal_model_name(NULL); zeromort_model_name(NULL); zeromorb_model_name(NULL)
      } else {
        files  <- outputs_files() %||% character(0)
        groups <- parse_model_group(files)
        selected_group <- input$ahle_model_choice
        matching_files <- files[groups == selected_group]
        
        current_model_name(   matching_files[grepl("(?i)_current_full\\.csv$",               matching_files)][1] )
        ideal_model_name(     matching_files[grepl("(?i)_ideal_full\\.csv$",                 matching_files)][1] )
        zeromort_model_name(  matching_files[grepl("(?i)_(zeroMort|zeroMortality)_full\\.csv$", matching_files)][1] )
        zeromorb_model_name(  matching_files[grepl("(?i)_(zeroMorb|zeroMorbidity)_full\\.csv$", matching_files)][1] )
        single_model_name(NULL)
      }
      
      showNotification("Dashboard configuration saved.", type = "message", duration = 2)
    })
    
    #Refetch models every 4s when modal is open
    observe({
      req(modal_open())
      invalidateLater(4000, session)
      
      # Re-pull; if changed, repopulate the selects
      before <- outputs_files()
      refresh_outputs_list()
      after <- outputs_files()
      
      if (!identical(before, after)) {
        files  <- after %||% character(0)
        groups <- unique(stats::na.omit(parse_model_group(files)))
        
        updateSelectInput(
          session, "single_model_choice",
          choices  = files,
          selected = isolate(input$single_model_choice) %||% if (length(files)) files[1] else NULL
        )
        
        updateSelectInput(
          session, "ahle_model_choice",
          choices  = groups,
          selected = isolate(input$ahle_model_choice) %||% if (length(groups)) groups[1] else NULL
        )
      }
    })
    
    
    # ---------------- Child panel servers ----------------
    observe({
      req(plot_list()); req(panelCount() > 0)
      current_plot_list <- isolate(plot_list())
      current_data <- data
      
      handles <- lapply(seq_len(panelCount()), function(i) {
        wellPanelModuleServer(
          id = paste0("panel", i),
          plot_list = current_plot_list,
          data = current_data,
          db_type = db_type
        )
      })
      child_handles(handles)
      
      lapply(seq_len(panelCount()), function(i) {
        observeEvent(handles[[i]]$remove(), {
          saveConfigurations()
          plist <- panelConfigs()
          if (length(plist) >= i) plist <- plist[-i]
          names(plist) <- as.character(seq_along(plist))
          panelConfigs(plist)
          panelCount(length(plist))
        }, ignoreInit = TRUE, once = TRUE)
      })
    })
    
    # ---------------- Save JSON (download) ----------------
    # COMING SOON
    # output$download <- downloadHandler(
    #   filename = function() {
    #     info <- dashboardInfo()
    #     paste(Sys.Date(), "-", info$userID, "-", info$modelID, ".json", sep = "")
    #   },
    #   content = function(file) {
    #     saveConfigurations()
    #     outputFile <- list("Info" = dashboardInfo(), "Panels" = panelConfigs())
    #     jsonlite::write_json(outputFile, file, pretty = TRUE, auto_unbox = TRUE)
    #   }
    # )
    
    observeEvent(input$download, {
      showModal(modalDialog(
        title = "Coming Soon",
        "Support for saving dashboard configurations will be added in a later version.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })
    
  })
}