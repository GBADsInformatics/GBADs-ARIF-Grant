function(input, output, session) {
  
  # ============================================================================
  # Utilities
  # ============================================================================
  or_default <- function(x, default = "") if (is.null(x)) default else x
  .rx_esc    <- function(x) gsub("([][{}()+*^$|\\.?\\\\-])", "\\\\\\1", x)
  
  find_model_output_basenames <- function(all_paths, model_base) {
    if (!length(all_paths) || !nzchar(model_base)) return(character(0))
    bnames <- basename(as.character(all_paths))
    bnames <- bnames[!is.na(bnames) & nzchar(bnames)]
    tokens <- c("current","ideal","zero[Mm]ortality","zero[Mm]orbidity","zeromort","zeromorb")
    pat_strict <- paste0("^", .rx_esc(model_base), "_(", paste(tokens, collapse="|"), ")(?:_[^/]*)?\\.csv$")
    strict <- bnames[grepl(pat_strict, bnames, perl=TRUE)]
    pat_loose <- paste0("(^|_)", .rx_esc(model_base), "_(", paste(tokens, collapse="|"), ")_")
    loose <- bnames[grepl(pat_loose, bnames, perl=TRUE, ignore.case=TRUE) & grepl("\\.csv$", bnames, ignore.case=TRUE)]
    unique(c(strict, loose))
  }
  
  # ============================================================================
  # Navigation State
  # ============================================================================
  currentPage <- reactiveVal("main")
  userData    <- reactiveVal("")
  user_id     <- reactiveVal(NULL)
  
  observeEvent(input$loginBtn,    { currentPage("login")    })
  observeEvent(input$registerBtn, { currentPage("register") })
  observeEvent(input$backBtn,     { currentPage("main")     })
  observeEvent(input$logoutBtn,   { currentPage("main")     })
  
  # ============================================================================
  # Dynamic Top-Level UI
  # ============================================================================
  output$dynamicContent <- renderUI({
    req(currentPage())
    tagList(
      if (currentPage() != "account") {
        tags$div(
          class = "titlePanel",
          div(
            h1(
              tags$span("GBAD", style = "font-weight: 900; font-size: clamp(60px, 10vw, 120px);"),
              tags$span("s",    style = "font-weight: 900; font-size: clamp(60px, 10vw, 120px); color: #f55c2c;")
            )
          )
        )
      } else tags$div(),
      switch(
        currentPage(),
        
        # Landing
        "main" = tagList(
          div(style = "max-width: 600px; margin: 0 auto;",
              div(
                h3("Access Your Custom Models and Dashboards.",
                   style = "color: #f55c2c; line-height: 1.5; width: 100%; font-size: clamp(12px, 4vw, 30px); text-align: center; font-weight: bold; padding: 0 20%;"
                ),
                h5("Easily view and manage your model outputs, and create custom dashboard-style reports.",
                   style = "padding: 5% 20%; line-height: 1.5; text-align: center;"
                )
              ),
              div(style = "padding-top: 10%; text-align: center;",
                  actionButton("loginBtn",    "Login",    class = "darkBtn"),
                  actionButton("registerBtn", "Register", class = "lightBtn")
              )
          )
        ),
        
        # Login
        "login" = tagList(
          div(style = "maxwidth: 550px; margin: 0 auto;",
              h3("Login", style = "color: #f55c2c; text-align: center; font-weight: bold;"),
              div(style = "text-align: center;", h5("Please enter your email and code.", style = "text-align: center;")),
              div(style = "width: 100%; display: flex; justify-content: center;", textInput("userEmail", label = NULL, placeholder = "Email")),
              div(style = "width: 100%; display: flex; justify-content: center;", passwordInput("userCode", label = NULL, placeholder = "Code")),
              div(style = "text-align: center;", actionButton("submitCodeBtn", "Submit", class = "darkBtn")),
              div(style = "text-align: center;", actionButton("backBtn", "Back", class = "subtleBtn"))
          )
        ),
        
        # Register
        "register" = tagList(
          div(style = "max-width: 550px; margin: 0 auto;",
              h3("Register", style = "color: #f55c2c; line-height: 1.5; text-align: center; font-weight: bold; padding: 0 20%;"),
              h5("Please fill out the registration form to get started.", style = "padding: 5% 20%; line-height: 1.5; text-align: center;"),
              div(
                style = "text-align: center;",
                div(
                  style = "display: flex; justify-content: space-between; gap: 10px; width: 100%;",
                  textInput("firstName",  label = NULL, placeholder = "First Name",            width = "48%"),
                  textInput("lastName",   label = NULL, placeholder = "Last Name (Optional)",  width = "48%")
                ),
                textInput("email",       label = NULL, placeholder = "Email Address",           width = "100%"),
                textInput("currentRole", label = NULL, placeholder = "Current Role (Optional)", width = "100%"),
                selectInput("country",  label = NULL, choices = c("Country" = "", countries),   selected = "", width = "100%"),
                selectInput("language", label = NULL, choices = c("Preferred Language" = "", languages), selected = "", width = "100%"),
                div(
                  style = "text-align: left; margin: 10px auto; width: 100%;",
                  checkboxInput("infoConsent",  "I consent to my current role, country of residence, and preferred language being used by GBADs for the purpose of grant proposals and user demographic tracking."),
                  checkboxInput("cloudConsent", "I understand that GBADs uses cloud computing to enhance data processing speeds, and certify that the use of cloud computing is allowed by my institution and/or employer.")
                ),
                actionButton("submitRegBtn", "Submit", class = "darkBtn"),
                div(style = "padding-top: 10%; text-align: center;", actionButton("backBtn", "Back", class = "subtleBtn"))
              )
          )
        ),
        
        # Account (Dashboard shell)
        "account" = tagList(
          div(style = "width: 100vw; left: 0; top: 0;",
              dashboardPage(
                dashboardHeader(
                  title = tagList(
                    span("GBAD", style = "font-weight: 900; font-size: 40px;"),
                    span("s",    style = "color: #f55c2c; font-weight: 900; font-size: 40px; margin-left: -5px;")
                  ),
                  tags$li(class = "dropdown", actionButton("logoutBtn", "Log out", class = "headerBtn"))
                ),
                dashboardSidebar(
                  sidebarMenu(
                    id = "tabs",
                    menuItem("Models",            tabName = "models",           icon = icon("cube"),     selected = TRUE),
                    menuItem("Dashboard Builder", tabName = "dashboardViewer",  icon = icon("dashboard")),
                    menuItem("Account",           tabName = "account",          icon = icon("user"))
                  )
                ),
                dashboardBody(
                  tags$script(HTML("$(document).ready(function(){setTimeout(function(){ $(window).trigger('resize'); }, 10);});")),
                  tabItems(
                    
                    # Models Tab
                    tabItem(
                      tabName = "models",
                      h2("Your Models"),
                      wellPanel(
                        style = "max-width: 2500px; margin-right:20px;",
                        actionButton("refreshMeta", "Refresh", icon = icon("arrows-rotate"), class = "lightBtn", style  = "float: right;"),
                        DTOutput("modelTable", width = "100%"),
                        downloadButton("downloadOutputsZip", "Download", style = "display:none;")
                      ),
                      br(),
                      h2("Your Saved Scenarios"),
                      wellPanel(
                        style = "max-width: 1000px; margin-right:20px;",
                        actionButton("refreshScenarios", "Refresh", icon = icon("arrows-rotate"), class = "lightBtn", style  = "float: right;"),
                        DTOutput("scenarioTable", width = "100%")
                      ),
                      br(),
                      wellPanel(style = "max-width: 2500px; margin-right:20px;", uiOutput("workflow"))
                    ),
                    
                    # Dashboard Tab
                    tabItem(tabName = "dashboardViewer",
                            wellPanel(style = "max-width: 2500px; margin-right:20px;", uiOutput("outputDashboard"))
                    ),
                    
                    # Account Tab
                    tabItem(tabName = "account",
                            div(
                              style = "width: 100%; text-align: center; padding: 20px; height: 130px; background-color: #F2F2F2;",
                              h2(paste(userData()[["firstname"]], userData()[["lastname"]], sep = " "), style = "margin: 0;"),
                              p(paste(userData()[["email"]]), style = "margin: 5px 0; font-size: 0.9em; color: gray;")
                            ),
                            div(
                              style = "width: 100%; text-align: center; padding: 20px; background-color: #FFF;",
                              img(src = "https://www.nosm.ca/wp-content/uploads/2024/01/Photo-placeholder-1024x1024.jpg",
                                  width = "150px", height = "150px",
                                  style = "border-radius: 50%; border: 5px solid white; box-shadow: 0px 0px 10px rgba(0,0,0,0.1); margin-top:-40px")
                            ),
                            div(style = "margin-top:20px; width: 99%;",
                                panel(
                                  heading = "Profile",
                                  div(style = "margin-bottom: 10px;", strong("First Name: "),        userData()[["firstname"]]),
                                  div(style = "margin-bottom: 10px;", strong("Last Name: "),         userData()[["lastname"]]),
                                  div(style = "margin-bottom: 10px;", strong("Email: "),             userData()[["email"]]),
                                  div(style = "margin-bottom: 10px;", strong("Current Role: "),      userData()[["role"]]),
                                  div(style = "margin-bottom: 10px;", strong("Country: "),           userData()[["country"]]),
                                  div(style = "margin-bottom: 10px;", strong("Preferred Language: "),userData()[["language"]]),
                                  hr(),
                                  actionButton("deleteAccount", "Delete Account", icon = icon("trash"), class = "deleteBtn", style = "width: 100%;")
                                )
                            )
                    )
                  )
                )
              )
          )
        )
      )
    )
  })
  
  # ============================================================================
  # Registration
  # ============================================================================
  observeEvent(input$submitRegBtn, {
    missing <- character(0)
    if (input$firstName == "") missing <- c(missing, "First Name")
    if (input$email    == "") missing <- c(missing, "Email Address")
    if (input$country  == "") missing <- c(missing, "Country")
    if (input$language == "") missing <- c(missing, "Preferred Language")
    
    if (length(missing) > 0) {
      sendSweetAlert(session, "Missing Information.", paste("Please fill out:", paste(missing, collapse = ", ")), type = "error"); return()
    }
    if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", input$email)) {
      sendSweetAlert(session, "Missing Information.", "Your email address does not appear to be properly formatted.", type = "error"); return()
    }
    if (isTRUE(db_query(check_email_exists(input$email))$exists)) {
      sendSweetAlert(session, "Account Taken.", "That email address is already associated with another account. Use the Login page.", type = "error"); return()
    }
    if (input$cloudConsent != TRUE) {
      sendSweetAlert(session, "Cloud Consent Required.",
                     HTML(paste("Cloud computing consent is required to access the GBADs client portal.",
                                a("Available here.", href = "https://gbadske.org", target = "_blank"))),
                     type = "error", html = TRUE); return()
    }
    
    role <- if (nzchar(input$currentRole)) input$currentRole else "User"
    try({
      new_user <- create_user(
        firstname = input$firstName,
        lastname  = input$lastName,
        email     = input$email,
        country   = input$country,
        language  = input$language,
        role      = role
      )
      if (!is.null(new_user) && !is.null(new_user$user_id)) {
        sendSweetAlert(session, "Account Created 🎉",
                       HTML(paste0("<b>Your account number is: <span style='font-size:1.2em;'>", new_user$user_id, "</span></b><br><br>",
                                   "Please <b>save this number</b> somewhere safe; you'll need it to login.")),
                       type = "success", html = TRUE)
        currentPage("main")
      } else {
        sendSweetAlert(session, "Account Created (Check Email)",
                       "Your account appears to be created, but we couldn't read the account number from the response.",
                       type = "warning")
      }
    }, silent = TRUE) -> try_res
    
    if (inherits(try_res, "try-error")) {
      sendSweetAlert(session, "Couldn’t Create Account",
                     "Something went wrong while creating your account. Please try again.",
                     type = "error")
    }
  })
  
  # ============================================================================
  # Login
  # ============================================================================
  observeEvent(input$submitCodeBtn, {
    entered_email <- trimws(tolower(input$userEmail %||% ""))
    entered_code  <- trimws(input$userCode %||% "")
    
    if (!nzchar(entered_email) && !nzchar(entered_code)) {
      sendSweetAlert(session, "Missing Information", "Please enter both your email and account code.", type = "error")
      return(invisible(NULL))
    }
    if (!nzchar(entered_email)) {
      sendSweetAlert(session, "Missing Email", "Please enter your email address.", type = "error")
      return(invisible(NULL))
    }
    if (!nzchar(entered_code)) {
      sendSweetAlert(session, "Missing Code", "Please enter your account code.", type = "error")
      return(invisible(NULL))
    }
    
    # Validate email format
    if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", entered_email)) {
      sendSweetAlert(session, "Invalid Email", "Please enter a valid email address.", type = "error")
      return(invisible(NULL))
    }
    
    if (!grepl("^[0-9]+$", entered_code)) {
      sendSweetAlert(session, "Invalid Code", "Your account code should be digits only.", type = "error")
      return(invisible(NULL))
    }

    user <- safe_get_user(entered_code)
    if (is.null(user)) {
      sendSweetAlert(session, "Not Found", "We couldn’t find an account with the email and code you provided. If you haven’t registered yet, please create an account. If you think this is a mistake, please reach out to support.", type = "error")
      return(invisible(NULL))
    }
    
    record_email <- trimws(tolower(or_default(user$user_email, "")))
    if (!nzchar(record_email) || entered_email != record_email) {
      sendSweetAlert(session, "Not Found", "We couldn’t find an account with the email and code you provided. If you haven’t registered yet, please create an account. If you think this is a mistake, please reach out to support.", type = "error")
      return(invisible(NULL))
    }
    
    userData(list(
      user_id   = user$user_id,
      firstname = user$user_firstname,
      lastname  = user$user_lastname,
      email     = user$user_email,
      country   = user$user_country,
      language  = user$user_language,
      role      = user$user_role
    ))
    
    user_id(user$user_id)
    sendSweetAlert(session, "Welcome back!", paste0("Logging in as ", user$user_firstname, " ", user$user_lastname, "."), type = "success")
    currentPage("account")
  })
  
  # ============================================================================
  # Account Deletion Prompt
  # ============================================================================
  observeEvent(input$deleteAccount, {
    showModal(modalDialog(
      title = "Confirm Deletion",
      "Are you sure you want to delete your account and all associated data? This action is not reversible.",
      easyClose = FALSE,
      footer = tagList(modalButton("Cancel"), actionButton("confirmDelete", "Yes, Delete"))
    ))
  })
  
  # ============================================================================
  # Models: Metadata Table Build/Refresh
  # ============================================================================
  metadata_table <- reactiveVal(data.frame())
  
  refresh_metadata_table <- function() {
    files <- list_metadata_files_from_api(bucket = "inputs", user_id = user_id())
    if (is.null(files) || !length(files)) { metadata_table(data.frame()); return(invisible(NULL)) }
    
    out_raw <- tryCatch(list_outputs_files_from_api(bucket = "outputs", user_id = user_id()), error = function(e) NULL)
    output_bases <- character(0)
    if (!is.null(out_raw) && length(out_raw)) {
      out_vec <- if (is.character(out_raw)) {
        out_raw
      } else if (is.list(out_raw) && all(vapply(out_raw, is.character, logical(1)))) {
        unlist(out_raw, use.names = FALSE)
      } else if (is.data.frame(out_raw)) {
        nm <- intersect(c("name","Name","key","Key","object_name","ObjectName","path","Path"), names(out_raw))
        if (length(nm)) out_raw[[nm[1]]] else character(0)
      } else character(0)
      out_vec <- as.character(out_vec)
      out_vec <- out_vec[!is.na(out_vec) & nzchar(out_vec)]
      output_bases <- tools::file_path_sans_ext(basename(out_vec))
    }
    
    expected_suffixes <- c("_current", "_ideal", "_zeroMortality", "_zeroMorbidity")
    esc_rx <- function(x) gsub("([][{}()+*^$|\\.?\\\\-])", "\\\\\\1", x)
    
    rows <- lapply(files, function(f) {
      meta <- download_json_from_api(bucket = "inputs", user_id = user_id(), file_name = f)
      if (is.null(meta)) return(NULL)
      
      model_name   <- or_default(meta$model_name, gsub("_metadata\\.json$", "", f, ignore.case = TRUE))
      notes        <- or_default(meta$notes, "")
      date_created <- or_default(meta$date_created, NA_character_)
      orig_current <- or_default(meta$original_current_name, NA_character_)
      orig_ideal   <- or_default(meta$original_ideal_name,   NA_character_)
      
      present <- vapply(expected_suffixes, function(suf) {
        pat <- paste0("^", esc_rx(model_name), esc_rx(suf), "([_-].*|$)")
        any(grepl(pat, output_bases, ignore.case = TRUE))
      }, logical(1))
      n_present <- sum(present)
      
      status <- "Failed"
      if (n_present >= length(expected_suffixes)) {
        status <- "Complete"
      } else {
        created <- suppressWarnings(as.POSIXct(date_created, tz = "UTC"))
        if (!is.na(created)) {
          hrs <- as.numeric(difftime(Sys.time(), created, units = "hours"))
          status <- if (hrs <= 24) "Processing" else "Failed"
        } else {
          status <- "Processing"
        }
      }
      
      status_pill <- switch(
        status,
        "Complete"   = "<span class='status-pill-complete'>Complete</span>",
        "Processing" = "<span class='status-pill-processing'>Processing</span>",
        "<span class='status-pill-failed'>Failed</span>"
      )
      
      data.frame(
        `Model Name`   = model_name,
        `Date Created` = date_created,
        `Current File` = orig_current,
        `Ideal File`   = orig_ideal,
        `Notes`        = notes,
        `StatusRaw`    = status,
        `Status`       = status_pill,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    })
    
    rows <- Filter(Negate(is.null), rows)
    if (!length(rows)) {
      metadata_table(data.frame())
    } else {
      df <- do.call(rbind, rows)
      df <- as.data.frame(df, check.names = FALSE)
      if ("Date Created" %in% names(df)) {
        o <- order(df$`Date Created`, decreasing = TRUE, na.last = TRUE)
        df <- df[o, , drop = FALSE]
      }
      metadata_table(df)
    }
  }
  
  observe({ refresh_metadata_table() })
  
  observeEvent(input$refreshMeta, {
    if (requireNamespace("shinyjs", quietly = TRUE)) shinyjs::disable("refreshMeta")
    n_id <- showNotification("Refreshing metadata…", type = "message", duration = NULL)
    on.exit({ removeNotification(n_id); if (requireNamespace("shinyjs", quietly = TRUE)) shinyjs::enable("refreshMeta") }, add = TRUE)
    refresh_metadata_table()
  }, ignoreInit = TRUE)
  
  # ============================================================================
  # Models: Table Rendering (Download / Delete actions)
  # ============================================================================
  output$modelTable <- DT::renderDT({
    df <- metadata_table()
    if (is.null(df) || !nrow(df)) {
      return(DT::datatable(data.frame(`You do not have any models yet.` = ""), colnames = NULL, rownames = FALSE, options = list(dom = 't', paging = FALSE)))
    }
    
    dl_btn <- ifelse(
      df$StatusRaw == "Complete",
      sprintf("<button class='btn btn-default btn-sm' title='Download'
                 onClick=\"Shiny.setInputValue('%s', '%s', {priority:'event'})\">
                 <i class='fa fa-download'></i></button>",
              "downloadModel", htmltools::htmlEscape(df$`Model Name`)),
      ""
    )
    
    del_btn <- ifelse(
      df$StatusRaw != "Processing",
      sprintf("<button class='btn btn-danger btn-sm' title='Delete'
                 onClick=\"Shiny.setInputValue('%s', '%s', {priority:'event'})\">
                 <i class='fa fa-trash'></i></button>",
              "deleteModel", htmltools::htmlEscape(df$`Model Name`)),
      ""
    )
    
    df$Actions <- paste(dl_btn, del_btn)
    
    desired_cols <- c("Model Name", "Date Created", "Current File", "Ideal File", "Notes", "Status", "Actions")
    keep        <- intersect(desired_cols, names(df))
    display_df  <- df[, keep, drop = FALSE]
    
    notes_idx_js   <- which(names(display_df) == "Notes")  - 1L
    status_idx_js  <- which(names(display_df) == "Status") - 1L
    actions_idx_js <- which(names(display_df) == "Actions")- 1L
    
    DT::datatable(
      display_df,
      escape = FALSE,
      selection = "none",
      rownames  = FALSE,
      style     = "bootstrap",
      options = list(
        pageLength = 10,
        autoWidth  = TRUE,
        columnDefs = list(
          list(width = "30%", targets = notes_idx_js),
          list(orderable = FALSE, targets = c(status_idx_js, actions_idx_js))
        )
      )
    )
  })
  
  # ============================================================================
  # Models: Download Outputs (ZIP)
  # ============================================================================
  selected_uid_for_zip  <- reactiveVal("")
  selected_base_for_zip <- reactiveVal("")
  
  observeEvent(input$downloadModel, {
    req(user_id())
    selected_uid_for_zip(as.character(user_id()))
    selected_base_for_zip(as.character(or_default(input$downloadModel, "")))
    showModal(modalDialog(
      title = "Download model outputs",
      tags$p(sprintf("Model: %s", selected_base_for_zip())),
      tags$p(sprintf("User: %s",  selected_uid_for_zip())),
      footer = tagList(modalButton("Cancel"), downloadButton("downloadOutputsZip", "Download ZIP", class = "btn btn-primary")),
      easyClose = TRUE
    ))
  })
  
  output$downloadOutputsZip <- downloadHandler(
    filename = function() {
      uid  <- or_default(selected_uid_for_zip(),  or_default(user_id(), ""))
      base <- or_default(selected_base_for_zip(), "")
      paste0(if (nzchar(base)) paste0(base, "_") else "", "outputs_user_", if (nzchar(uid)) uid else "unknown", ".zip")
    },
    contentType = "application/zip",
    content = function(file) {
      uid  <- or_default(selected_uid_for_zip(),  or_default(user_id(), ""))
      base <- or_default(selected_base_for_zip(), "")
      make_zip_with      <- function(path, files) zip::zipr(zipfile = path, files = files, include_directories = FALSE)
      write_manifest_zip <- function(path, lines) {
        td <- tempfile("zipnote_"); dir.create(td, showWarnings = FALSE)
        note <- file.path(td, "manifest.txt"); writeLines(lines, note)
        make_zip_with(path, note)
      }
      if (!nzchar(uid)) { write_manifest_zip(file, "No user_id available."); return(invisible()) }
      
      raw_list <- try(list_files_from_api("outputs", uid), silent = TRUE)
      if (inherits(raw_list, "try-error") || is.null(raw_list)) {
        write_manifest_zip(file, sprintf("Could not list outputs for user_id=%s.", uid))
        return(invisible())
      }
      
      all_paths <- if (is.character(raw_list)) {
        raw_list
      } else if (is.list(raw_list) && all(vapply(raw_list, is.character, logical(1)))) {
        unlist(raw_list, use.names = FALSE)
      } else if (is.data.frame(raw_list)) {
        nm <- intersect(c("name","Name","key","Key","object_name","ObjectName","path","Path"), names(raw_list))
        if (length(nm)) raw_list[[nm[1]]] else character(0)
      } else character(0)
      all_paths <- as.character(all_paths)
      all_paths <- all_paths[!is.na(all_paths) & nzchar(all_paths)]
      
      wanted_bns <- find_model_output_basenames(all_paths, base)
      pre_manifest <- c(
        sprintf("user_id: %s", uid),
        sprintf("model_base: %s", base),
        "",
        "== RAW LISTING ==",
        if (length(all_paths)) paste(all_paths, collapse = "\n") else "<none>",
        "",
        "== SELECTED BASENAMES ==",
        if (length(wanted_bns)) paste(wanted_bns, collapse = "\n") else "<none>"
      )
      
      td <- tempfile("dl_"); dir.create(td, showWarnings = FALSE)
      manifest <- file.path(td, "manifest.txt")
      writeLines(pre_manifest, manifest)
      
      if (!length(wanted_bns)) {
        make_zip_with(file, c(manifest))
        showNotification("No matching CSVs for that model base. See manifest.txt", type="warning", duration=5)
        return(invisible())
      }
      
      wrote_any <- FALSE
      failed    <- character(0)
      for (bn in wanted_bns) {
        df <- try(download_csv_from_api(bucket = "outputs", user_id = uid, file_name = bn), silent = TRUE)
        if (inherits(df, "try-error") || is.null(df)) { failed <- c(failed, bn); next }
        utils::write.csv(as.data.frame(df, stringsAsFactors = FALSE), file = file.path(td, bn), row.names = FALSE, na = "")
        wrote_any <- TRUE
      }
      
      if (length(failed)) {
        cat("\n== DOWNLOAD FAILURES ==\n", file = manifest, append = TRUE)
        cat(paste(failed, collapse = "\n"), file = manifest, append = TRUE)
      }
      
      files_to_zip <- c(list.files(td, full.names = TRUE), manifest)
      zip::zipr(zipfile = file, files = files_to_zip, include_directories = FALSE)
      
      if (!wrote_any) {
        showNotification("Matched files but downloads failed. See manifest.txt", type="error", duration=6)
      } else {
        showNotification(sprintf("Downloaded %d file(s).", sum(basename(files_to_zip) != "manifest.txt")), type="message", duration=4)
      }
    }
  )
  
  outputOptions(output, "downloadOutputsZip", suspendWhenHidden = FALSE)
  
  # ============================================================================
  # Models: Delete Model (inputs + outputs)
  # ============================================================================
  .pending_model_delete <- reactiveVal(NULL)
  
  observeEvent(input$deleteModel, ignoreInit = TRUE, {
    req(input$deleteModel)
    .pending_model_delete(input$deleteModel)
    showModal(modalDialog(
      title = "Delete model?",
      sprintf("Are you sure you want to delete “%s” and its related files from cloud storage?", input$deleteModel),
      easyClose = TRUE,
      footer = tagList(modalButton("Cancel"), actionButton("confirm_delete_model", "Delete", class = "btn btn-danger"))
    ))
  })
  
  observeEvent(input$confirm_delete_model, ignoreInit = TRUE, {
    removeModal()
    base <- .pending_model_delete()
    uid  <- user_id()
    req(base, uid)
    
    if (requireNamespace("shinyjs", quietly = TRUE)) shinyjs::disable("confirm_delete_model")
    note_id <- showNotification(sprintf("Deleting files for “%s”…", base), type = "message", duration = NULL)
    on.exit({ removeNotification(note_id); if (requireNamespace("shinyjs", quietly = TRUE)) shinyjs::enable("confirm_delete_model") }, add = TRUE)
    
    raw_list <- try(list_files_from_api("outputs", uid), silent = TRUE)
    paths <- if (inherits(raw_list, "try-error") || is.null(raw_list)) character(0) else {
      if (is.character(raw_list)) raw_list
      else if (is.list(raw_list) && all(vapply(raw_list, is.character, logical(1)))) unlist(raw_list, use.names = FALSE)
      else if (is.data.frame(raw_list)) {
        nm <- intersect(c("name","Name","key","Key","object_name","ObjectName","path","Path"), names(raw_list))
        if (length(nm)) raw_list[[nm[1]]] else character(0)
      } else character(0)
    }
    paths <- paths[!is.na(paths) & nzchar(paths)]
    outs_basenames <- find_model_output_basenames(paths, base)
    
    input_files <- c(
      paste0(base, "_current.yaml"),
      paste0(base, "_ideal.yaml"),
      paste0(base, "_zeroMortality.yaml"),
      paste0(base, "_zeroMorbidity.yaml"),
      paste0(base, "_metadata.json")
    )
    
    deleted <- 0L
    failed  <- character(0)
    
    if (length(outs_basenames)) {
      for (fn in outs_basenames) {
        ok <- try(delete_file_from_api(bucket = "outputs", user_id = uid, file_name = fn), silent = TRUE)
        if (inherits(ok, "try-error") || isFALSE(ok)) failed <- c(failed, paste0("outputs/", fn)) else deleted <- deleted + 1L
      }
    }
    
    for (fn in input_files) {
      ok <- try(delete_file_from_api(bucket = "inputs", user_id = uid, file_name = fn), silent = TRUE)
      if (inherits(ok, "try-error") || isFALSE(ok)) failed <- c(failed, paste0("inputs/", fn)) else deleted <- deleted + 1L
    }
    
    if (length(failed)) {
      showNotification(sprintf("Some files could not be deleted (%d removed, %d failed).", deleted, length(failed)), type = "warning", duration = 6)
    } else {
      showNotification(sprintf("Deleted %d file(s) for “%s”.", deleted, base), type = "message", duration = 4)
    }
    
    .pending_model_delete(NULL)
    refresh_metadata_table()
  })
  
  # ============================================================================
  # Scenarios: List, Refresh, View, Delete
  # ============================================================================
  scenario_files <- reactiveVal(character(0))
  
  refresh_scenario_files <- function() {
    uid <- isolate(user_id())
    if (is.null(uid) || is.na(uid)) { scenario_files(character(0)); return(invisible(NULL)) }
    
    out <- tryCatch(list_files_from_api(bucket = "storage", user_id = uid), error = function(e) NULL)
    if (is.null(out) || length(out) == 0) { scenario_files(character(0)); return(invisible(NULL)) }
    
    files <- if (is.character(out)) {
      out
    } else if (is.list(out) && all(vapply(out, is.character, logical(1)))) {
      unlist(out, use.names = FALSE)
    } else if (is.data.frame(out)) {
      nm <- intersect(c("name","Name","key","Key","object_name","ObjectName","path","Path"), names(out))
      if (length(nm)) out[[nm[1]]] else character(0)
    } else character(0)
    
    files <- as.character(files)
    files <- files[!is.na(files) & nzchar(files)]
    yaml_files <- basename(files)
    yaml_files <- yaml_files[grepl("\\.(ya?ml)$", yaml_files, ignore.case = TRUE)]
    scenario_files(sort(yaml_files))
  }
  
  observe({ refresh_scenario_files() })
  
  observeEvent(user_id(), { refresh_scenario_files() }, ignoreInit = TRUE)
  
  observeEvent(input$refreshScenarios, {
    if (requireNamespace("shinyjs", quietly = TRUE)) shinyjs::disable("refreshScenarios")
    n_id <- showNotification("Refreshing scenarios…", type = "message", duration = NULL)
    on.exit({ removeNotification(n_id); if (requireNamespace("shinyjs", quietly = TRUE)) shinyjs::enable("refreshScenarios") }, add = TRUE)
    refresh_scenario_files()
  }, ignoreInit = TRUE)
  
  output$scenarioTable <- DT::renderDT({
    files <- scenario_files()
    if (!length(files)) {
      return(DT::datatable(data.frame(`You do not have any stored scenarios yet.` = ""), colnames = NULL, rownames = FALSE, options = list(dom = 't', paging = FALSE)))
    }
    
    df <- data.frame(`File Name` = files, stringsAsFactors = FALSE, check.names = FALSE)
    
    view_btn <- sprintf("<button class='btn btn-default btn-sm' title='View'
                          onClick=\"Shiny.setInputValue('%s', '%s', {priority:'event'})\">
                          <i class='fa fa-eye'></i></button>",
                        "viewScenario", htmltools::htmlEscape(df$`File Name`))
    
    delete_btn <- sprintf("<button class='btn btn-danger btn-sm' title='Delete'
                            onClick=\"Shiny.setInputValue('%s', '%s', {priority:'event'})\">
                            <i class='fa fa-trash'></i></button>",
                          "deleteScenario", htmltools::htmlEscape(df$`File Name`))
    
    df$Actions <- paste(view_btn, delete_btn)
    
    name_idx_js    <- which(names(df) == "File Name") - 1L
    actions_idx_js <- which(names(df) == "Actions")   - 1L
    
    DT::datatable(
      df[, c("File Name", "Actions")],
      escape    = FALSE,
      selection = "none",
      rownames  = FALSE,
      style     = "bootstrap",
      options = list(
        pageLength = 10,
        autoWidth  = TRUE,
        columnDefs = list(
          list(width = "70%", targets = name_idx_js),
          list(orderable = FALSE, targets = actions_idx_js)
        )
      )
    )
  })
  
  observeEvent(input$viewScenario, {
    req(input$viewScenario)
    fn  <- input$viewScenario
    txt <- get_yaml_text_from_api("storage", user_id(), fn)
    if (is.null(txt)) { showNotification(sprintf("Could not load '%s' from storage.", fn), type = "error"); return(invisible(NULL)) }
    
    showModal(modalDialog(
      title = paste("YAML:", fn),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tags$pre(style = "max-height: 70vh; overflow:auto; white-space: pre-wrap; background:#f8f9fa; padding:12px; border:1px solid #ddd;",
               htmltools::htmlEscape(txt))
    ))
  })
  
  .pending_delete <- reactiveVal(NULL)
  
  observeEvent(input$deleteScenario, ignoreInit = TRUE, {
    req(input$deleteScenario)
    .pending_delete(input$deleteScenario)
    showModal(modalDialog(
      title = "Delete scenario?",
      easyClose = TRUE,
      footer = tagList(modalButton("Cancel"), actionButton("confirm_delete_scn", "Delete", class = "btn btn-danger")),
      sprintf("Are you sure you want to delete “%s” from storage?", input$deleteScenario)
    ))
  })
  
  observeEvent(input$confirm_delete_scn, ignoreInit = TRUE, {
    removeModal()
    fn <- .pending_delete(); req(fn)
    if (requireNamespace("shinyjs", quietly = TRUE)) shinyjs::disable("confirm_delete_scn")
    note_id <- showNotification("Deleting…", type = "message", duration = NULL)
    on.exit({ removeNotification(note_id); if (requireNamespace("shinyjs", quietly = TRUE)) shinyjs::enable("confirm_delete_scn") }, add = TRUE)
    
    ok <- delete_file_from_api(bucket = "storage", user_id = user_id(), file_name = fn)
    if (isTRUE(ok)) {
      showNotification(sprintf("Deleted “%s”.", fn), type = "message")
      refresh_scenario_files()
      .pending_delete(NULL)
    } else {
      showNotification("Delete failed. Please try again.", type = "error")
    }
  })
  
  # ============================================================================
  # Workflow Panel UI
  # ============================================================================
  output$workflow <- renderUI({
    tagList(
      h3("Create a new model", style = "font-weight: 900; color: #555; text-align: center;"),
      div(class = "flow-container",
          div(class = "flow-step",
              div(class = "title", "1. Download Template"),
              div(class = "subtext", "Select the species you want to model to download the corresponding Excel template."),
              actionButton("downloadModal", "Download", icon = icon("download"), class = "darkBtn")
          ),
          div(class = "arrow"),
          div(class = "flow-step",
              div(class = "title", "2. Upload a Scenario"),
              div(class = "subtext", "Open the downloaded template and adjust the values as necessary to create a new scenario."),
              actionButton("uploadModal", "Upload", icon = icon("upload"), class = "darkBtn")
          ),
          div(class = "arrow"),
          div(class = "flow-step",
              div(class = "title", "3. Run a Model"),
              div(class = "subtext", "When you are ready to create a new model, click the button below to upload your completed template file(s)."),
              actionButton("runModal", "Run Model", icon = icon("play"), class = "darkBtn")
          ),
          div(class = "arrow"),
          div(class = "flow-step",
              div(class = "title", "4. Wait For Model to Run"),
              div(class = "subtext", "Running a new model can take several minutes, you can check the progress in the table above.")
          )
      )
    )
  })
  
  # ============================================================================
  # Templates: Download
  # ============================================================================
  observeEvent(input$downloadModal, {
    cattle_url <- "https://gbads-modelling.s3.ca-central-1.amazonaws.com/cattle_DPM_template_version_1.0.xlsm"
    showModal(modalDialog(
      title = "Download Scenario Template",
      tagList(
        h5("Active"),
        radioImages("selectTemplate_active",
                    images = c("https://www.svgrepo.com/show/99158/cow-silhouette.svg"),
                    values = c(cattle_url),
                    texts  = c("Cattle")),
        tags$hr(),
        h5("Coming soon"),
        div(
          id = "comingSoonGroup",
          radioImages(
            "selectTemplate_soon",
            images = c(
              "https://www.svgrepo.com/show/307506/horse-horse-riding-farm-animal-equestrian.svg",
              "https://www.svgrepo.com/show/159080/rooster.svg",
              "https://www.svgrepo.com/show/169029/sheep.svg",
              "https://www.svgrepo.com/show/95738/pig-side-view-silhouette.svg"
            ),
            values = c("equine", "poultry", "smallruminants", "swine"),
            texts  = c("Equine", "Poultry", "Small Ruminants", "Swine")
          )
        ),
        tags$em("Templates for additional species will be available soon."),
        tags$style(HTML("#comingSoonGroup { opacity: 0.45; pointer-events: none; }"))
      ),
      easyClose = TRUE,
      footer = tagList(actionButton("customCloseBtn", "Close", class = "lightBtn"),
                       actionButton("downloadTemplate", "Download", class = "uploadBtn")),
      size = "m"
    ))
  })
  
  observeEvent(input$customCloseBtn, removeModal())
  
  observeEvent(input$downloadTemplate, {
    cattle_url <- "https://gbads-modelling.s3.ca-central-1.amazonaws.com/cattle_DPM_template_version_1.0.xlsm"
    shinyjs::runjs(sprintf("window.open('%s', '_blank');", cattle_url))
  })
  
  # ============================================================================
  # Scenarios: Upload
  # ============================================================================
  observeEvent(input$uploadModal, {
    showModal(modalDialog(
      title = "Upload a Scenario",
      fileInput(
        "newScenarioUpload",
        label = NULL,
        multiple = FALSE,
        width = "100%",
        buttonLabel = "Browse",
        placeholder = "No file selected",
        accept = c(".yaml", ".yml")  # accept both
      ),
      easyClose = TRUE, size = "m",
      footer = tagList(
        actionButton("customCloseBtn", "Close", class = "lightBtn"),
        actionButton("submitScenarioBtn", "Upload", icon = icon("upload"), class = "uploadBtn")
      )
    ))
  })
  
  refreshTrigger <- reactiveVal(0)
  observeEvent(input$runModal, {
    refreshTrigger(refreshTrigger() + 1)
  })
  
  existingScenarios <- reactive({
    req(user_id())
    refreshTrigger()
    
    files <- tryCatch(list_files_from_api("storage", user_id()), error = function(e) NULL)
    empty_named <- list(all = setNames(character(0), character(0)))
    if (is.null(files) || length(files) == 0) return(empty_named)
    
    keys <- NULL
    if (is.character(files)) {
      keys <- files
    } else if (is.list(files) && all(vapply(files, is.character, logical(1)))) {
      keys <- unlist(files, use.names = FALSE)
    } else if (is.data.frame(files)) {
      cand <- c("Key","key","name","Name","path","Path","object","Object")
      hit  <- cand[cand %in% names(files)]
      if (length(hit)) keys <- as.character(files[[hit[1]]])
    }
    if (is.null(keys)) return(empty_named)
    
    keys <- keys[!is.na(keys) & nzchar(keys)]
    yaml_files <- keys[grepl("\\.ya?ml$", keys, ignore.case = TRUE)]
    if (!length(yaml_files)) return(empty_named)
    
    display_names <- basename(yaml_files)
    display_names <- display_names[!is.na(display_names) & nzchar(display_names)]
    if (!length(display_names)) return(empty_named)
    
    named_files <- setNames(yaml_files, display_names)
    list(all = named_files)
  })
  
  upload_state <- reactiveValues(pending_file = NULL, ready_to_upload = FALSE)
  
  observeEvent(input$submitScenarioBtn, {
    req(input$newScenarioUpload)
    
    uploaded_file <- input$newScenarioUpload
    file_name <- basename(if (is.null(uploaded_file$name)) "" else uploaded_file$name)
    
    existing_list  <- tryCatch(existingScenarios(), error = function(e) NULL)
    existing_all   <- if (is.null(existing_list)) NULL else existing_list$all
    existing_files <- if (is.null(existing_all)) character(0) else names(existing_all)
    
    # extension guard before parsing
    if (!grepl("\\.ya?ml$", file_name, ignore.case = TRUE)) {
      showModal(modalDialog(
        title = "Unsupported file type",
        "Please upload a .yaml or .yml file.",
        easyClose = TRUE, footer = modalButton("OK")
      ))
      return(invisible(NULL))
    }
    
    if (file_name %in% existing_files) {
      upload_state$pending_file <- uploaded_file
      showModal(modalDialog(
        title  = "File already exists",
        paste0("A file named '", file_name, "' already exists. Overwrite?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirmOverwrite", "Overwrite", class = "btn-danger")
        )
      ))
    } else {
      upload_state$pending_file    <- uploaded_file
      upload_state$ready_to_upload <- TRUE
    }
  })
  
  observeEvent(input$confirmOverwrite, {
    upload_state$ready_to_upload <- TRUE
    removeModal()
  })
  
  observeEvent(upload_state$ready_to_upload, {
    req(upload_state$pending_file)
    uploaded_file <- upload_state$pending_file
    
    # --- SAFE YAML PARSE ---
    yaml_obj <- tryCatch(
      yaml::read_yaml(uploaded_file$datapath),
      error = function(e) e
    )
    
    if (inherits(yaml_obj, "error")) {
      showModal(modalDialog(
        title = "Invalid YAML file",
        tagList(
          p("We couldn’t read that YAML file. Please fix the formatting and try again."),
          p("Common issues include unquoted colons in values, inconsistent indentation, or tabs."),
          tags$pre(style = "white-space: pre-wrap;", conditionMessage(yaml_obj))
        ),
        easyClose = TRUE, footer = modalButton("OK")
      ))
      # reset state so it doesn't loop
      upload_state$pending_file    <- NULL
      upload_state$ready_to_upload <- FALSE
      return(invisible(NULL))
    }
    
    # --- UPLOAD ---
    success <- tryCatch(
      upload_yaml_to_api(
        bucket = "storage",
        user_id = user_id(),
        file_name = uploaded_file$name,
        yaml_object = yaml_obj
      ),
      error = function(e) FALSE
    )
    
    if (isTRUE(success)) {
      sendSweetAlert(session, "Success!", "Your parameter file has been saved.", type = "success",
                     btn_labels = "OK", closeOnClickOutside = TRUE)
      # refresh the scenarios list so the file appears immediately
      refreshTrigger(refreshTrigger() + 1)
    } else {
      sendSweetAlert(session, "Oops...", "Your parameter file could not be uploaded.",
                     type = "error", btn_labels = "OK", closeOnClickOutside = TRUE)
    }
    
    upload_state$pending_file    <- NULL
    upload_state$ready_to_upload <- FALSE
  })
  
  # ============================================================================
  # Run Model (Create inputs + metadata)
  # ============================================================================
  existingModels <- reactive({
    req(user_id())
    empty_named <- list(all = setNames(character(0), character(0)))
    files <- tryCatch(list_files_from_api("inputs", user_id()), error = function(e) NULL)
    if (is.null(files) || length(files) == 0) return(empty_named)
    
    keys <- NULL
    if (is.character(files)) {
      keys <- files
    } else if (is.list(files) && all(vapply(files, is.character, logical(1)))) {
      keys <- unlist(files, use.names = FALSE)
    } else if (is.data.frame(files)) {
      cand <- c("Key","key","name","Name","path","Path","object","Object")
      hit  <- cand[cand %in% names(files)]
      if (length(hit)) keys <- as.character(files[[hit[1]]])
    }
    if (is.null(keys)) return(empty_named)
    
    keys <- keys[!is.na(keys) & nzchar(keys)]
    yaml_files <- keys[grepl("\\.ya?ml$", keys, ignore.case = TRUE)]
    if (!length(yaml_files)) return(empty_named)
    
    filenames <- basename(yaml_files)
    keep      <- !is.na(filenames) & nzchar(filenames)
    if (!any(keep)) return(empty_named)
    yaml_files <- yaml_files[keep]
    filenames  <- filenames[keep]
    
    user_defined_names <- vapply(filenames, function(name) {
      m  <- regexec("^user_\\d+_([^_]+)_", name)
      mm <- regmatches(name, m)[[1]]
      if (length(mm) > 1) mm[2] else sub("\\.[Yy][Aa]?[Mm][Ll]$", "", name)
    }, character(1))
    
    if (anyDuplicated(user_defined_names)) user_defined_names <- make.unique(user_defined_names, sep = "_")
    named_files <- setNames(yaml_files, user_defined_names)
    list(all = named_files)
  })
  
  observeEvent(input$runModal, {
    showModal(modalDialog(
      title = "Create a New Model",
      h5("1. Name your model"),
      div(textInput("newModelName", label = NULL, placeholder = "Enter name"), uiOutput("modelNameWarning")),
      hr(),
      h5("2. Select a model to be used as your 'Current' scenario, or upload a new YAML file"),
      uiOutput("currentTemplateUpload"),
      hr(),
      h5("3. Select a model to be used as your 'Ideal' scenario, or upload a new YAML file"),
      uiOutput("idealTemplateUpload"),
      hr(),
      h5("4. Notes (optional)"),
      textAreaInput("newModelNotes", label = NULL, value = "", rows = 5, width = "100%"),
      easyClose = TRUE, size = "m",
      footer = tagList(actionButton("customCloseBtn", "Close", class = "lightBtn"),
                       actionButton("submitModelBtn", "Run", icon = icon("play"), class = "uploadBtn"))
    ))
  })
  
  observeEvent(input$customCloseBtn, { removeModal() })
  
  output$currentTemplateUpload <- renderUI({
    req(existingScenarios())
    current_files <- existingScenarios()$all
    current_files <- current_files[!is.na(current_files) & current_files != ""]
    tagList(
      selectInput("existingCurrentModel", label = NULL, choices = c("", "New file...", current_files), selected = ""),
      uiOutput("newCurrentUploadUI")
    )
  })
  
  output$newCurrentUploadUI <- renderUI({
    req(input$existingCurrentModel)
    tagList(if (input$existingCurrentModel == "New file...") {
      fileInput("newCurrentUpload", label = NULL, multiple = FALSE, width = "100%", buttonLabel = "Browse", placeholder = "No file selected")
    } else NULL)
  })
  
  output$idealTemplateUpload <- renderUI({
    req(existingScenarios())
    ideal_files <- existingScenarios()$all
    ideal_files <- ideal_files[!is.na(ideal_files) & ideal_files != ""]
    tagList(
      selectInput("existingIdealModel", label = NULL, choices = c("", "New file...", ideal_files), selected = ""),
      uiOutput("newIdealUploadUI")
    )
  })
  
  output$newIdealUploadUI <- renderUI({
    req(input$existingIdealModel)
    tagList(if (input$existingIdealModel == "New file...") {
      fileInput("newIdealUpload", label = NULL, multiple = FALSE, width = "100%", buttonLabel = "Browse", placeholder = "No file selected")
    } else NULL)
  })
  
  specialCharWarning        <- reactiveVal(FALSE)
  missingModelNameWarning   <- reactiveVal(FALSE)
  existingModelNameWarning  <- reactiveVal(FALSE)
  missingTemplatesWarning   <- reactiveVal(FALSE)
  
  observe({ specialCharWarning(grepl("[^a-zA-Z0-9_.-]", input$newModelName)) })
  
  observeEvent(input$submitModelBtn, {
    missingModelNameWarning(FALSE)
    existingModelNameWarning(FALSE)
    specialCharWarning(FALSE)
    missingTemplatesWarning(FALSE)
    
    if (is.null(input$newModelName) || input$newModelName == "") { missingModelNameWarning(TRUE); return() }
    if (grepl("[^a-zA-Z0-9_.-]", input$newModelName)) { specialCharWarning(TRUE); return() }
    
    existing_model_names <- names(existingModels()$all)
    if (input$newModelName %in% existing_model_names) { existingModelNameWarning(TRUE); return() }
    
    if ((is.null(input$existingCurrentModel) || input$existingCurrentModel == "") && is.null(input$newCurrentUpload)) { missingTemplatesWarning(TRUE); return() }
    if ((is.null(input$existingIdealModel)   || input$existingIdealModel   == "") && is.null(input$newIdealUpload))   { missingTemplatesWarning(TRUE); return() }
    
    original_current_name <- NULL
    if (!is.null(input$newCurrentUpload)) {
      current_yaml <- yaml::read_yaml(input$newCurrentUpload$datapath)
      original_current_name <- input$newCurrentUpload$name
      success <- upload_yaml_to_api("storage", user_id(), original_current_name, current_yaml)
      if (!success) { sendSweetAlert(session, "Upload Error", "Error uploading your Current file to the cloud.", "error"); return() }
    } else {
      selected_file <- input$existingCurrentModel
      if (is.null(selected_file) || selected_file == "") { missingTemplatesWarning(TRUE); return() }
      original_current_name <- basename(selected_file)
      current_yaml <- download_yaml_from_api("storage", user_id(), original_current_name)
      if (is.null(current_yaml)) { sendSweetAlert(session, "Load Error", "Error loading your Current YAML from the cloud.", "error"); return() }
    }
    
    original_ideal_name <- NULL
    if (!is.null(input$newIdealUpload)) {
      ideal_yaml <- yaml::read_yaml(input$newIdealUpload$datapath)
      original_ideal_name <- input$newIdealUpload$name
      success <- upload_yaml_to_api("storage", user_id(), original_ideal_name, ideal_yaml)
      if (!success) { sendSweetAlert(session, "Upload Error", "Error uploading your Ideal file to the cloud.", "error"); return() }
    } else {
      selected_file <- input$existingIdealModel
      if (is.null(selected_file) || selected_file == "") { missingTemplatesWarning(TRUE); return() }
      original_ideal_name <- basename(selected_file)
      ideal_yaml <- download_yaml_from_api("storage", user_id(), original_ideal_name)
      if (is.null(ideal_yaml)) { sendSweetAlert(session, "Load Error", "Error loading your Ideal YAML from the cloud.", "error"); return() }
    }
    
    zero_mort <- create_zero_mort(current_yaml, ideal_yaml)
    zero_morb <- create_zero_morb(current_yaml, ideal_yaml)
    
    model_name             <- input$newModelName
    current_name_simplified <- paste0(model_name, "_current.yaml")
    ideal_name_simplified   <- paste0(model_name, "_ideal.yaml")
    zmort_name_simplified   <- paste0(model_name, "_zeroMortality.yaml")
    zmorb_name_simplified   <- paste0(model_name, "_zeroMorbidity.yaml")
    
    upload_success <- c(
      upload_yaml_to_api("inputs", user_id(), current_name_simplified, current_yaml),
      upload_yaml_to_api("inputs", user_id(), ideal_name_simplified,   ideal_yaml),
      upload_yaml_to_api("inputs", user_id(), zmort_name_simplified,   zero_mort),
      upload_yaml_to_api("inputs", user_id(), zmorb_name_simplified,   zero_morb)
    )
    if (any(!upload_success)) { sendSweetAlert(session, "Upload Error", "One or more YAML files could not be uploaded to the cloud.", "error"); return() }
    
    meta <- list(
      model_name = model_name,
      user_id    = user_id(),
      date_created = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
      original_current_name = original_current_name,
      original_ideal_name   = original_ideal_name,
      stored_files = list(
        current       = current_name_simplified,
        ideal         = ideal_name_simplified,
        zeroMortality = zmort_name_simplified,
        zeroMorbidity = zmorb_name_simplified
      ),
      notes = or_default(input$newModelNotes, "")
    )
    
    meta_file_name <- paste0(model_name, "_metadata.json")
    meta_ok <- upload_json_to_api("inputs", user_id(), meta_file_name, meta)
    if (!meta_ok) { sendSweetAlert(session, "Upload Error", "Metadata file could not be uploaded to the cloud.", "error"); return() }
    
    removeModal()
    sendSweetAlert(session, "Success!", "Your files have been uploaded and your model is being run.", type = "success",
                   btn_labels = "OK", closeOnClickOutside = TRUE)
  })
  
  output$modelNameWarning <- renderUI({
    if (missingModelNameWarning())       h6("Please enter a name for your model.", style = "color: red;")
    else if (specialCharWarning())       h6("Avoid using special characters in the model name.", style = "color: red;")
    else if (existingModelNameWarning()) h6("This model name already exists. Please choose another.", style = "color: red;")
    else if (missingTemplatesWarning())  h6("Please select both an ideal and current template.", style = "color: red;")
  })
  
  # ============================================================================
  # Dashboard Module Mount
  # ============================================================================
  output$outputDashboard <- renderUI({ outputDashboardUI("db") })
  observeEvent(userData(), { req(userData()); outputDashboardServer("db", userData) }, ignoreInit = TRUE)
}
