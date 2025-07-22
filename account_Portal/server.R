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
                            wellPanel(style = "max-width: 2500px; margin-right:20px;",
                                      DTOutput("modelTable", width = "100%")
                            ),
                            br(),
                            wellPanel(
                              uiOutput("workflow"),
                              style = "margin-right: 20px;"
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
  
  # Submit Model Button Logic ------------------------------------------
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
    
    existing <- yamlTemplates()
    all_existing <- unique(c(existing$current, existing$ideal))
    
    if (input$newModelName %in% all_existing) {
      existingModelNameWarning(TRUE)
      return()
    }
    
    # Validate that a template is selected or uploaded for BOTH current and ideal
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
    
    # Load current YAML (from existing file or uploaded file)
    if (!is.null(input$newCurrentUpload)) {
      current_yaml <- yaml::read_yaml(input$newCurrentUpload$datapath)
    } else {
      scenario_name <- gsub("_(current|ideal)$", "", input$existingCurrentModel)
      all_files <- list.files("./Data", pattern = "\\.yaml$", full.names = TRUE)
      pattern <- paste0("_(?:", scenario_name, ")_current\\.yaml$")
      current_file <- grep(pattern, all_files, value = TRUE)
      if (length(current_file) != 1) {
        missingTemplatesWarning(TRUE)
        return()
      }
      current_yaml <- yaml::read_yaml(current_file)
    }
    
    # Load ideal YAML (from existing file or uploaded file)
    if (!is.null(input$newIdealUpload)) {
      ideal_yaml <- yaml::read_yaml(input$newIdealUpload$datapath)
    } else {
      scenario_name <- gsub("_(current|ideal)$", "", input$existingIdealModel)
      all_files <- list.files("./Data", pattern = "\\.yaml$", full.names = TRUE)
      pattern <- paste0("_(?:", scenario_name, ")_ideal\\.yaml$")
      ideal_file <- grep(pattern, all_files, value = TRUE)
      if (length(ideal_file) != 1) {
        missingTemplatesWarning(TRUE)
        return()
      }
      ideal_yaml <- yaml::read_yaml(ideal_file)
    }
    
    # Create zeroMortality and zeroMorbidity
    zero_mort <- create_zero_mort(current_yaml, ideal_yaml)
    zero_morb <- create_zero_morb(current_yaml, ideal_yaml)
    
    # Build output filenames and save all 4 YAMLs
    timestamp <- format(Sys.time(), "%Y%m%dT%H%M%S")
    user_id <- "user123"
    scenario_name <- input$newModelName
    base <- file.path("Data", paste0(timestamp, "_", user_id, "_", scenario_name))
    
    yaml::write_yaml(current_yaml, paste0(base, "_current.yaml"))
    yaml::write_yaml(ideal_yaml,   paste0(base, "_ideal.yaml"))
    yaml::write_yaml(zero_mort,    paste0(base, "_zeroMortality.yaml"))
    yaml::write_yaml(zero_morb,    paste0(base, "_zeroMorbidity.yaml"))
    
    # Clear modal
    removeModal()
    
    # User feedback that files have been saved
    sendSweetAlert(
      session = session,
      title = "Success!",
      text = "Your parameter files have been saved and your model is being run.",
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
  
  # Available Models Table --------------------------------------------------
  output$modelTable <- renderDT({
    modelData$Action <- sprintf(
      "<button class='btn btn-default' onClick=\"Shiny.setInputValue('createDashboard', '%s')\"><i class='fa-solid fa-chart-line'></i></button>
       <button class='btn btn-default' onClick=\"Shiny.setInputValue('download', '%s')\"><i class='fa fa-download'></i></button>
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
  
  # Available Dashboards Table ----------------------------------------------
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
  
  # Download Template -------------------------------------------------------
  observeEvent(input$downloadModal, {
    showModal(modalDialog(
      title = "Download New Template",
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
  
  # Ideal Template Upload ---------------------------------------------------
  output$workflow <- renderUI({
    tagList(
      h3("Create a new model", style = "font-weight: 900; color: #555; text-align: center;"),
      div(class = "flow-container",
          div(class = "flow-step",
              div(class = "title", "1. Download Template"),
              div(class = "subtext", "Select the species you want to model to download the corresponding Excel template."),
              actionButton("downloadModal", "Download", class = "lightBtn")
          ),
          div(class = "arrow"),
          div(class = "flow-step",
              div(class = "title", "2. Adjust Values"),
              div(class = "subtext", "Open the downloaded template and adjust the values as necessary to create a new scenario.")
          ),
          div(class = "arrow"),
          div(class = "flow-step",
              div(class = "title", "3. Upload Values"),
              div(class = "subtext", "When you are ready to create a new model, click the button below to upload your completed template file(s)."),
              actionButton("newModelModal", "Create Model", class = "darkBtn")
          ),
          div(class = "arrow"),
          div(class = "flow-step",
              div(class = "title", "4. Wait For Email"),
              div(class = "subtext", "Running a new model can take several minutes. No need to stick around, we will send you an email once your run is complete!")
          )
      ) 
    )
  })
  
  # Modal when "Create New Model" is clicked --------------------------------
  observeEvent(input$newModelModal, {
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
        actionButton("submitModelBtn", "Submit", class = "uploadBtn")
      )
    ))
    
  })
  
  observeEvent(input$customCloseBtn, {
    removeModal()
  })
  

# Get available YAML ------------------------------------------------------
  yamlTemplates <- reactive({
    extract_yaml_names("./Data")
  })  
  
  # Upload logic for "Current" --------------------------------------------
  output$currentTemplateUpload <- renderUI({
    req(yamlTemplates())
    
    tagList(
      selectInput("existingCurrentModel", 
                  label = NULL,
                  choices = c("", "New file...", paste0(yamlTemplates()$current, "_current")),
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
  
  # Upload logic for "Ideal" --------------------------------------------
  output$idealTemplateUpload <- renderUI({
    req(yamlTemplates())
    
    tagList(
      selectInput("existingIdealModel", 
                  label = NULL,
                  choices = c("", "New file...", paste0(yamlTemplates()$ideal, "_ideal")),
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
  
}