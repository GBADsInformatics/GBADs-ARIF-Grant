function(input, output, session) {
  currentPage <- reactiveVal("main")
  userData <- reactiveVal("")
  
  # Update currentPage based on button clicks
  observeEvent(input$loginBtn, {
    currentPage("login")
  })
  
  observeEvent(input$registerBtn, {
    currentPage("register")
  })
  
  observeEvent(input$backBtn, {
    currentPage("main")
  })
  
  observeEvent(input$logoutBtn, {
    currentPage("main")
  })
  
  output$dynamicContent <- renderUI({
    req(currentPage())
    tagList(
      
      # Logo Header ------------------------------------------------------------
      if (currentPage() != "account") {
        tags$div(
          class = "titlePanel",
          div(
            h1(
              tags$span("GBAD", style = "font-weight: 900; font-size: clamp(60px, 10vw, 120px);"),
              tags$span("s", style = "font-weight: 900; font-size: clamp(60px, 10vw, 120px); color: #f55c2c;")
            )
          )
        )
        
      } else {
        tags$div() 
      },
      
      switch(
        currentPage(),
        
        # Landing Page ------------------------------------------------------------
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
                  actionButton("loginBtn", "Login", class = "darkBtn"),
                  actionButton("registerBtn", "Register", class = "lightBtn")
              )
          )
        ),
        
        # Login Page --------------------------------------------------------------
        "login" = tagList(
          div(style = "maxwidth: 550px; margin: 0 auto;",
              h3("Login",
                 style = "color: #f55c2c; text-align: center; font-weight: bold;"
              ),
              div(style = "text-align: center;",
                  h5("Please enter your unique code.",
                     style = "text-align: center;"
                  )
              ),
              div(style = "width: 100%; display: flex; justify-content: center;",
                  textInput("userCode", label = NULL, placeholder = "Code")
              ),
              div(style = "text-align: center;", 
                  actionButton("submitCodeBtn", "Submit", class = "darkBtn")
              ),
              div(style = "text-align: center;",
                  actionButton("backBtn", "Back", class = "subtleBtn")
              )
          )
        ),
        
        # Register Page -----------------------------------------------------------
        "register" = tagList(
          div(style = "max-width: 550px; margin: 0 auto;",
              h3(
                "Register",
                style = "color: #f55c2c; line-height: 1.5; text-align: center; font-weight: bold; padding: 0 20%;"
              ),
              h5(
                "Please fill out the registration form to get started.",
                style = "padding: 5% 20%; line-height: 1.5; text-align: center;"
              ),
              div(
                style = "text-align: center;",
                div(
                  style = "display: flex; justify-content: space-between; gap: 10px; width: 100%;",
                  textInput("firstName", label = NULL, placeholder = "First Name", width = "48%"),
                  textInput("lastName", label = NULL, placeholder = "Last Name (Optional)", width = "48%")
                ),
                textInput("email", label = NULL, placeholder = "Email Address", width = "100%"),
                textInput("currentRole", label = NULL, placeholder = "Current Role (Optional)", width = "100%"),
                selectInput(
                  inputId = "country", 
                  label = NULL, 
                  choices = c("Country" = "", countries),
                  selected = "",
                  width = "100%"
                ),
                selectInput(
                  inputId = "language", 
                  label = NULL, 
                  choices = c("Preferred Language" = "", languages),
                  selected = "",
                  width = "100%"
                ),
                div(
                  style = "text-align: left; margin: 10px auto; width: 100%;",
                  checkboxInput(
                    "infoConsent", 
                    "I consent to my current role, country of residence, and preferred language being used by GBADs for the purpose of grant proposals and user demographic tracking."
                  ),
                  checkboxInput(
                    "cloudConsent", 
                    "I understand that GBADs uses cloud computing to enhance data processing speeds, and certify that the use of cloud computing is allowed by my institution and/or employer."
                  )
                ),
                actionButton("submitRegBtn", "Submit", class = "darkBtn"),
                div(
                  style = "padding-top: 10%; text-align: center;",
                  actionButton("backBtn", "Back", class = "subtleBtn")
                )
              )
          )
        ),
        
        # Account page ------------------------------------------------------------
        "account" = tagList(
          div(style = "width: 100vw; left: 0; top: 0;",
              dashboardPage(
                
                dashboardHeader(
                  title = tagList(
                    span("GBAD", style = "font-weight: 900; font-size: 40px;"),
                    span("s", style = "color: #f55c2c; font-weight: 900; font-size: 40px; margin-left: -5px;")
                  ),
                  tags$li(class = "dropdown", actionButton("logoutBtn", "Log out", class = "headerBtn"))
                ),
                
                dashboardSidebar(
                  sidebarMenu(
                    menuItem("Models", tabName = "models", icon = icon("cube"), selected = T),
                    menuItem("Dashboards", tabName = "dashboards", icon = icon("dashboard")),
                    menuItem("Account", tabName = "account", icon = icon("user"))
                  )
                ),
                
                dashboardBody(
                  tags$script(HTML("$(document).ready(function() {setTimeout(function() { $(window).trigger('resize'); }, 10);});")),
                  tabItems(
                    
                    
                    # Models Tab --------------------------------------------------------------
                    tabItem(tabName = "models",
                            h2("Your Models"),
                            wellPanel(
                              style = "max-width: 2500px; margin-right:20px;",
                              DTOutput("modelTable", width = "100%"),
                              div(style = "text-align: right; margin-top: 10px;",
                                  actionButton("launchOutputDB", "Open Dashboard Viewer", icon = icon("rocket"), class = "darkBtn")
                              )
                            ),
                            br(),
                            wellPanel(style = "max-width: 2500px; margin-right:20px;",
                                      uiOutput("workflow"),
                            )
                    ),
                    
                    # Dashboard Tabs ----------------------------------------------------------
                    tabItem(tabName = "dashboards",
                            h2("Your Dashboards"),
                            wellPanel(style = "max-width: 2500px; margin-right:20px;",
                                      DTOutput("dashTable")
                            )
                    ),
                    
                    # Account Tab -------------------------------------------------------------
                    tabItem(tabName = "account",
                            div(
                              style = "width: 100%; text-align: center; padding: 20px;height: 130px; background-color: #F2F2F2;",
                              h2(paste(userData()[["firstname"]], userData()[["lastname"]], sep = " "), style = "margin: 0;"),
                              p(paste(userData()[["email"]]), style = "margin: 5px 0; font-size: 0.9em; color: gray;")
                            ),
                            
                            # Second Div: Circular Profile Picture with White Border
                            div(
                              style = "width: 100%; text-align: center; padding: 20px; background-color: #FFF;",
                              img(src = "https://www.nosm.ca/wp-content/uploads/2024/01/Photo-placeholder-1024x1024.jpg", 
                                  width = "150px", height = "150px", 
                                  style = "border-radius: 50%; border: 5px solid white; box-shadow: 0px 0px 10px rgba(0,0,0,0.1); margin-top:-40px")
                            ),
                            
                            div(style = "margin-top:20px; width: 99%;",
                                panel(
                                  heading = "Profile",
                                  
                                  div(style = "margin-bottom: 10px;", strong("First Name: "), userData()[["firstname"]]),
                                  div(style = "margin-bottom: 10px;", strong("Last Name: "), userData()[["lastname"]]),
                                  div(style = "margin-bottom: 10px;", strong("Email: "), userData()[["email"]]),
                                  div(style = "margin-bottom: 10px;", strong("Current Role: "), userData()[["role"]]),
                                  div(style = "margin-bottom: 10px;", strong("Country: "), userData()[["country"]]),
                                  div(style = "margin-bottom: 10px;", strong("Preferred Language: "), userData()[["language"]]),
                                  
                                  hr(),
                                  
                                  actionButton("deleteAccount", "Delete Account", icon = icon("trash"), 
                                               class = "deleteBtn", style = "width: 100%;")
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
  
# ------------------------------------------------------------------------------
# Account Tab
# ------------------------------------------------------------------------------
  
  # Submit Registration Button Logic ----------------------------------------
  observeEvent(input$submitRegBtn, {
    
    #Check for missing required information
    missingFields <- NULL
    
    if (input$firstName == "") {
      missingFields <- c(missingFields, "First Name")
    }
    
    if (input$email == "") {
      missingFields <- c(missingFields, "Email Address")
    }
    
    if (input$country == "") {
      missingFields <- c(missingFields, "Country")
    }
    
    if (input$language == "") {
      missingFields <- c(missingFields, "Preferred Language")
    }
    
    if (length(missingFields) > 0) {
      sendSweetAlert(
        session = session,
        title = "Missing Information.",
        text = paste("Please fill out the following fields: ", paste(missingFields, collapse = ", ")),
        type = "error"
      )
      
      #Check for properly formatted email address
    } else if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", input$email)){
      sendSweetAlert(
        session = session,
        title = "Missing Information.",
        text = paste("Your email address does not appear to be properly formatted. Please double-check and try again."),
        type = "error"
      )
      
      #Check if email already exists
    } else if (isTRUE(db_query(check_email_exists(input$email))$exists)){
      sendSweetAlert(
        session = session,
        title = "Account Taken.",
        text = paste("That email address is already associated with another account. Please use the 'Login' page to access your account."),
        type = "error"
      )
      
      #Check for cloud computing consent
    } else if (input$cloudConsent != T) {
      sendSweetAlert(
        session = session,
        title = "Cloud Consent Required.",
        text = HTML(
          paste(
            "Cloud computing consent is required to access the GBADs client portal. If you are unable to consent to the use of cloud computing, please use our offline solution.",
            a("Available here.", href = "https://gbadske.org", target = "_blank")
          )
        ),
        type = "error",
        html = TRUE
      )
      
      #Create user information file if all required information is provided.
    } else {
      formData <- list(
        firstName = input$firstName,
        lastName = input$lastName,
        email = input$email,
        currentRole = input$currentRole,
        country = input$country,
        language = input$language,
        infoConsent = input$infoConsent,
        cloudConsent = input$cloudConsent
      )
      jsonData <- toJSON(formData, pretty = TRUE)
      print(jsonData)
      sendSweetAlert(
        session = session,
        title = "You're all set!",
        text = "You will recieve an email shortly with your unique code, and instructions on how to get started.",
        type = "success"
      )
      currentPage("main")
    }
    
  })
  
  # Submit User Code Button Logic ------------------------------------------------
  user_id <- reactiveVal(NULL)
  
  observeEvent(input$submitCodeBtn, {
    req(input$userCode)
    result <- db_query(check_user_exists(input$userCode))
    
    if (isTRUE(result$exists)) {
      flatData <- db_query(get_user_info(input$userCode))
      
      userData(list(
        user_id = flatData$user_id,
        firstname = flatData$user_firstname,
        lastname = flatData$user_lastname,
        email = flatData$user_email,
        country = flatData$user_country,
        language = flatData$user_language,
        role = flatData$user_role
      ))
      
      user_id(flatData$user_id)
      currentPage("account")
      
    } else {
      sendSweetAlert(
        session = session,
        title = "Oops!",
        text = "It doesn't seem like you have an account. Please use the 'Registration' page to get started",
        type = "error"
      )
    }
    
  })
  
  # Delete Account Button ---------------------------------------------------
  
  observeEvent(input$deleteAccount, {
    showModal(modalDialog(
      title = "Confirm Deletion",
      "Are you sure you want to delete your account and all associated data? This action is not reversible.",
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmDelete", "Yes, Delete")
      )
    ))
  })
  
  
# ------------------------------------------------------------------------------
# Models Tab
# ------------------------------------------------------------------------------
  
  # Available Models Table
  output$modelTable <- renderDT({
    modelData$Action <- sprintf(
      "<button class='btn btn-default' onClick=\"Shiny.setInputValue('download', '%s')\"><i class='fa fa-download'></i></button>
       <button class='btn btn-danger' onClick=\"Shiny.setInputValue('delete', '%s')\"><i class='fa fa-trash'></i></button>",
      rownames(modelData), rownames(modelData), rownames(modelData)
    )
    
    datatable(modelData, 
              escape = FALSE, 
              selection = "none", 
              rownames = F, 
              style = "bootstrap", 
              colnames = c("Model Name", "Model Type", "Date Created", "Date Completed", "Model Version", 
                           "Status", "Actions")
    )
  })
  
  # Workflow well panel
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
              div(class = "subtext", "Running a new model can take several minutes, you can check the progress in the table above. No need to stick around!")
          )
      ) 
    )
  })
  
# ------------------------------------------------------------------------------
# Download Templates Button                 
# ------------------------------------------------------------------------------
  observeEvent(input$downloadModal, {
    showModal(modalDialog(
      title = "Download Scenario Template",
      h5("Which species template would you like to download?"),
      radioImages(
        "selectTemplate",
        images = c("https://www.svgrepo.com/show/99158/cow-silhouette.svg",
                   "https://www.svgrepo.com/show/159080/rooster.svg",
                   "https://www.svgrepo.com/show/169029/sheep.svg"),
        values = c("https://gbads-modelling.s3.ca-central-1.amazonaws.com/20250310_DPM_template_cattle.xlsx",
                   "https://gbads-modelling.s3.ca-central-1.amazonaws.com/20250310_DPM_template_poultry.xlsx",
                   "https://gbads-modelling.s3.ca-central-1.amazonaws.com/20250310_DPM_template_smallruminants.xlsx"),
        texts = c("Cattle",
                  "Poultry",
                  "Small Ruminants")
      ),
      easyClose = TRUE,
      footer = tagList(
        actionButton("customCloseBtn", "Close", class = "lightBtn"),
        actionButton("downloadTemplate", "Download", class = "uploadBtn")
      ),
      size = "m"
    ))
  })
  
  observeEvent(input$downloadTemplate, {
    url <- input$selectTemplate
    runjs(sprintf("window.open('%s', '_blank');", url))
  })
  
# ------------------------------------------------------------------------------
# Upload Scenario Button                 
# ------------------------------------------------------------------------------
  observeEvent(input$uploadModal, {
    showModal(modalDialog(
     title = "Upload a Scenario",
     fileInput("newScenarioUpload",
               label = NULL, 
               multiple = FALSE, 
               width = "100%", 
               buttonLabel = "Browse", 
               placeholder = "No file selected",
               accept = ".yaml"
     ),
     
     easyClose = TRUE,
     size = "m",
     
     footer = tagList(
       actionButton("customCloseBtn", "Close", class = "lightBtn"),
       actionButton("submitScenarioBtn", "Upload", icon = icon("upload"), class = "uploadBtn")
     )
     
    ))
  })
  
  # Get available YAML (Bucket 1)
  existingScenarios <- reactive({
    user_id <- user_id()
    files <- list_files_from_api("storage", user_id)
    
    yaml_files <- files[grepl("\\.ya?ml$", files, ignore.case = TRUE)]
    
    display_names <- basename(yaml_files)
    named_files <- setNames(yaml_files, display_names)
    
    list(all = named_files)
  })
  
  # Create reactives for checking if upload is an overwrite
  upload_state <- reactiveValues(
    pending_file = NULL,
    ready_to_upload = FALSE
  )
  
  # Button logic for upload scenario to Bucket 1
  observeEvent(input$submitScenarioBtn, {
    req(input$newScenarioUpload)
    
    uploaded_file <- input$newScenarioUpload
    file_name <- uploaded_file$name
    existing_files <- names(existingScenarios()$all)
    
    if (file_name %in% existing_files) {
      # Save the file temporarily and show confirm modal
      upload_state$pending_file <- uploaded_file
      showModal(
        modalDialog(
          title = "File already exists",
          paste0("A file named '", file_name, "' already exists. Overwrite?"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirmOverwrite", "Overwrite", class = "btn-danger")
          )
        )
      )
    } else {
      # No conflict — proceed immediately
      upload_state$pending_file <- uploaded_file
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
    yaml_object <- yaml::read_yaml(uploaded_file$datapath)
    
    success <- upload_yaml_to_api(
      bucket = "storage",
      user_id = user_id(),
      file_name = uploaded_file$name,
      yaml_object = yaml_object
    )
    
    if (isTRUE(success)) {
      sendSweetAlert(
        session = session,
        title = "Success!",
        text = "Your parameter file has been saved.",
        type = "success",
        btn_labels = "OK",
        closeOnClickOutside = TRUE
      )
    } else {
      sendSweetAlert(
        session = session,
        title = "Oops...",
        text = "Your parameter file was not able to be uploaded",
        type = "error",
        btn_labels = "OK",
        closeOnClickOutside = TRUE
      )
    }
    
    # Reset states
    upload_state$pending_file <- NULL
    upload_state$ready_to_upload <- FALSE
  })
  
# ------------------------------------------------------------------------------
# Run Model Button                  
# ------------------------------------------------------------------------------
  existingModels <- reactive({
    user_id <- user_id()
    files <- list_files_from_api("inputs", user_id)
    yaml_files <- files[grepl("\\.ya?ml$", files, ignore.case = TRUE)]
    filenames <- basename(yaml_files)
    
    # Extract user-defined names using regex
    user_defined_names <- vapply(filenames, function(name) {
      match <- regmatches(name, regexec("user_\\d+_([^_]+)_", name))[[1]]
      if (length(match) > 1) match[2] else NA_character_
    }, character(1))
    
    # Create named vector: display = "test", value = full file path
    named_files <- setNames(yaml_files, user_defined_names)
    
    list(all = named_files)
  })
  
  observeEvent(input$runModal, {
    showModal(modalDialog(
      title = "Create a New Model",
      
      h5("1. Name your model"),
      div(
        textInput("newModelName", label = NULL, placeholder = "Enter name"),
        uiOutput("modelNameWarning")
      ),
      
      hr(),
      h5("2. Select a model to be used as your 'Current' scenario, or upload a new YAML file"),
      uiOutput("currentTemplateUpload"),
      
      hr(),
      h5("3. Select a model to be used as your 'Ideal' scenario, or upload a new YAML file"),
      uiOutput("idealTemplateUpload"),
      
      easyClose = TRUE,
      size = "m",
      
      footer = tagList(
        actionButton("customCloseBtn", "Close", class = "lightBtn"),
        actionButton("submitModelBtn", "Run", icon = icon("play"), class = "uploadBtn")
      )
    ))
    
  })
  
  observeEvent(input$customCloseBtn, {
    removeModal()
  })
  
  # Upload logic for "Current"
  output$currentTemplateUpload <- renderUI({
    req(existingScenarios())
    
    current_files <- existingScenarios()$all
    current_files <- current_files[!is.na(current_files) & current_files != ""]
    
    tagList(
      selectInput("existingCurrentModel", 
                  label = NULL,
                  choices = c("", "New file...", current_files),
                  selected = ""),
      uiOutput("newCurrentUploadUI")
    )
  })
  
  output$newCurrentUploadUI <- renderUI({
    req(input$existingCurrentModel)
    
    tagList(
      if (input$existingCurrentModel == "New file...") {
        fileInput(
          "newCurrentUpload", 
          label = NULL, 
          multiple = FALSE, 
          width = "100%", 
          buttonLabel = "Browse", 
          placeholder = "No file selected"
        )
      } else {
        NULL
      }
    )
  })
  
  # Upload logic for "Ideal"
  output$idealTemplateUpload <- renderUI({
    req(existingScenarios())
    
    ideal_files <- existingScenarios()$all
    ideal_files <- ideal_files[!is.na(ideal_files) & ideal_files != ""]
    
    tagList(
      selectInput("existingIdealModel", 
                  label = NULL,
                  choices = c("", "New file...", ideal_files),
                  selected = ""),
      uiOutput("newIdealUploadUI")
    )
  })
  
  output$newIdealUploadUI <- renderUI({
    req(input$existingIdealModel)
    
    tagList(
      if (input$existingIdealModel == "New file...") {
        fileInput(
          "newIdealUpload", 
          label = NULL, 
          multiple = FALSE, 
          width = "100%", 
          buttonLabel = "Browse", 
          placeholder = "No file selected"
        )
      } else {
        NULL
      }
    )
  })
  
  # Submit Model Button Logic
  # Error handling flags
  specialCharWarning <- reactiveVal(FALSE)
  missingModelNameWarning <- reactiveVal(FALSE)
  existingModelNameWarning <- reactiveVal(FALSE)
  missingTemplatesWarning <- reactiveVal(FALSE)
  
  # Check for special characters
  observe({
    specialCharWarning(grepl("[^a-zA-Z0-9_.-]", input$newModelName))
  })
  
  observeEvent(input$submitModelBtn, {
    # Clear old warnings
    missingModelNameWarning(FALSE)
    existingModelNameWarning(FALSE)
    specialCharWarning(FALSE)
    missingTemplatesWarning(FALSE)
    
    # Validate model name
    if (is.null(input$newModelName) || input$newModelName == "") {
      missingModelNameWarning(TRUE)
      return()
    }
    
    if (grepl("[^a-zA-Z0-9_.-]", input$newModelName)) {
      specialCharWarning(TRUE)
      return()
    }
    
    #Check if model with same name already exists in Bucket2
    existing_model_names <- names(existingModels()$all)
    
    # Check if the new name already exists
    if (input$newModelName %in% existing_model_names) {
      existingModelNameWarning(TRUE)
      return()
    }
    
    # Check than "Current" and "Ideal" are both present
    if ((is.null(input$existingCurrentModel) || input$existingCurrentModel == "") &&
        is.null(input$newCurrentUpload)) {
      missingTemplatesWarning(TRUE)
      return()
    }
    
    if ((is.null(input$existingIdealModel) || input$existingIdealModel == "") &&
        is.null(input$newIdealUpload)) {
      missingTemplatesWarning(TRUE)
      return()
    }
    
    # ---- CURRENT YAML ----
    if (!is.null(input$newCurrentUpload)) {
      # Use the uploaded file
      current_yaml <- yaml::read_yaml(input$newCurrentUpload$datapath)
      
      # Upload it to the cloud (Bucket 1 = "storage")
      success <- upload_yaml_to_api(
        bucket = "storage",
        user_id = user_id(),  # assuming this returns something like "user_1"
        file_name = input$newCurrentUpload$name,
        yaml_object = current_yaml
      )
      
      if (!success) {
        sendSweetAlert(
          session = session,
          title = "Upload Error",
          text = "There was an error uploading your file to the cloud.",
          type = "error"
        )
        return()
      }
      
    } else {
      # User selected an existing file from the dropdown
      selected_file <- input$existingCurrentModel
      
      if (is.null(selected_file) || selected_file == "") {
        missingTemplatesWarning(TRUE)
        return()
      }
      
      # Download YAML from Bucket 1 (storage)
      current_yaml <- download_yaml_from_api(
        bucket = "storage",                      # short-form bucket name
        user_id = user_id(),                     # just the number (e.g. "1")
        file_name = basename(selected_file)      # just "trial_CLM_current.yaml"
      )
      
      if (is.null(current_yaml)) {
        sendSweetAlert(
          session = session,
          title = "Load Error",
          text = "There was an error loading your existing YAML from the cloud.",
          type = "error"
        )
        return()
      }
    }
    
    
    # ---- IDEAL YAML ----
    if (!is.null(input$newIdealUpload)) {
      # Read from uploaded file
      ideal_yaml <- yaml::read_yaml(input$newIdealUpload$datapath)
      
      success <- upload_yaml_to_api(
        bucket = "storage",
        user_id = user_id(),
        file_name = input$newIdealUpload$name,
        yaml_object = ideal_yaml
      )
      
      if (!success) {
        sendSweetAlert(
          session = session,
          title = "Upload Error",
          text = "There was an error uploading your ideal file to the cloud.",
          type = "error"
        )
        return()
      }
      
    } else {
      selected_file <- input$existingIdealModel
      
      if (is.null(selected_file) || selected_file == "") {
        missingTemplatesWarning(TRUE)
        return()
      }
      
      # Download from "storage"
      ideal_yaml <- download_yaml_from_api(
        bucket = "storage",                      # short-form bucket name
        user_id = user_id(),                     # just the number (e.g. "1")
        file_name = basename(selected_file)      # just "trial_CLM_current.yaml"
      )
      
      if (is.null(ideal_yaml)) {
        sendSweetAlert(
          session = session,
          title = "Load Error",
          text = "There was an error loading your ideal YAML from the cloud.",
          type = "error"
        )
        return()
      }
    }
    
    # Create zeroMortality and zeroMorbidity
    zero_mort <- create_zero_mort(current_yaml, ideal_yaml)
    zero_morb <- create_zero_morb(current_yaml, ideal_yaml)
    
    # Build timestamped file prefix
    timestamp <- format(Sys.time(), "%Y%m%dT%H%M%S")
    user_id_val <- paste("user", user_id(), sep = "_")
    scenario_name <- input$newModelName
    file_prefix <- paste0(timestamp, "_", user_id_val, "_", scenario_name)
    
    # Upload all 4 YAMLs to "inputs" bucket (formerly Bucket2)
    upload_success <- c(
      upload_yaml_to_api("inputs", user_id(), paste0(file_prefix, "_current.yaml"), current_yaml),
      upload_yaml_to_api("inputs", user_id(), paste0(file_prefix, "_ideal.yaml"), ideal_yaml),
      upload_yaml_to_api("inputs", user_id(), paste0(file_prefix, "_zeroMortality.yaml"), zero_mort),
      upload_yaml_to_api("inputs", user_id(), paste0(file_prefix, "_zeroMorbidity.yaml"), zero_morb)
    )
    
    # Check for any failures
    if (any(!upload_success)) {
      sendSweetAlert(
        session = session,
        title = "Upload Error",
        text = "One or more YAML files could not be uploaded to the cloud.",
        type = "error"
      )
      return()
    }
    
    # Clear modal
    removeModal()
    
    # Notify user
    sendSweetAlert(
      session = session,
      title = "Success!",
      text = "Your parameter files have been uploaded and your model is being run.",
      type = "success",
      btn_labels = "OK",
      closeOnClickOutside = TRUE
    )
    
  })
  
  # Error handling outputs
  output$modelNameWarning <- renderUI({
    if (missingModelNameWarning()) {
      h6("Please enter a name for your model.", style = "color: red;")
    } else if (specialCharWarning()) {
      h6("Avoid using special characters in the model name.", style = "color: red;")
    } else if (existingModelNameWarning()) {
      h6("This model name already exists. Please choose another.", style = "color: red;")
    } else if (missingTemplatesWarning()) {
      h6("Please select both an ideal and current template.", style = "color: red;")
    }
  })
  
# ------------------------------------------------------------------------------
# Dashboards Tab                 
# ------------------------------------------------------------------------------
  
  # Available Dashboards Table
  output$dashTable <- renderDT({
    dashData$Action <- sprintf(
      "<button class='btn btn-default' onClick=\"Shiny.setInputValue('viewDashboard', '%s')\"><i class='fa-solid fa-rocket'></i></button>
       <button class='btn btn-danger' onClick=\"Shiny.setInputValue('delete', '%s')\"><i class='fa fa-trash'></i></button>",
      rownames(dashData), rownames(dashData)
    )
    
    datatable(dashData, 
              escape = FALSE, 
              selection = "none", 
              rownames = F, 
              style = "bootstrap", 
              colnames = c("Dashboard Name", "Date Created", "Model Used", "Actions")
    )
  })
  
}