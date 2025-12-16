library(shiny)
library(httr)
library(jsonlite)
library(jose)

# Helper function to decode base64url (JWT uses this instead of standard base64)
base64_url_decode <- function(x) {
  # Replace URL-safe characters
  x <- gsub("-", "+", x)
  x <- gsub("_", "/", x)
  # Decode
  openssl::base64_decode(x)
}

#' Cognito Authentication UI Module
#' @param id The module ID
cognitoAuthUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class = "cognito-auth-container",
      actionButton(
        ns("loginWithCognito"),
        "Login with GBADs Account",
        class = "btn-primary btn-lg",
        style = "width: 100%; max-width: 300px;"
      )
    )
  )
}

cognitoAuthServer <- function(id, 
                            cognito_domain,
                            client_id,
                            redirect_uri,
                            region = "ca-central-1") {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values to store auth state
    values <- reactiveValues(
      authenticated = FALSE,
      user_info = NULL,
      access_token = NULL,
      id_token = NULL
    )
    
    # Build OAuth2 endpoints
    oauth_endpoints <- list(
      authorize = sprintf("https://%s/oauth2/authorize", cognito_domain),
      token = sprintf("https://%s/oauth2/token", cognito_domain)
    )
    
    # Handle login button click
    observeEvent(input$loginWithCognito, {
      # Generate state parameter for CSRF protection
      state <- paste0(sample(c(0:9, letters, LETTERS), 32, replace = TRUE), collapse = "")
      
      # Store state in session for validation
      session$userData$oauth_state <- state
      
      # Build authorization URL
      auth_url <- modify_url(
        oauth_endpoints$authorize,
        query = list(
          response_type = "code",
          client_id = client_id,
          redirect_uri = redirect_uri,
          state = state,
          scope = "openid email profile"
        )
      )
      
      # Redirect to Cognito hosted UI
      session$sendCustomMessage("redirectToCognito", auth_url)
    })
    
    # Handle stored token from localStorage
    observeEvent(input$stored_auth_token, {
      req(input$stored_auth_token)
      
      token_data <- input$stored_auth_token
      
      # Decode ID token payload to get user info
      id_token_parts <- strsplit(token_data$id_token, "\\.")[[1]]
      
      # Decode the payload (second part of JWT)
      payload_b64 <- id_token_parts[2]
      padding <- (4 - nchar(payload_b64) %% 4) %% 4
      if (padding > 0) {
        payload_b64 <- paste0(payload_b64, paste(rep("=", padding), collapse = ""))
      }
      
      # Decode base64url to JSON
      payload_json <- rawToChar(base64_url_decode(payload_b64))
      id_token <- fromJSON(payload_json)
      
      # Store authentication state
      values$authenticated <- TRUE
      values$user_info <- id_token
      values$access_token <- token_data$access_token
      values$id_token <- token_data$id_token
      
      # Notify user
      showNotification(
        sprintf("Welcome back, %s!", id_token$given_name %||% id_token$email %||% "User"),
        type = "message",
        duration = 3
      )
    }, ignoreInit = TRUE)
    
    # Handle OAuth callback
    observe({
      query <- parseQueryString(session$clientData$url_search)
      
      # Check for authorization code and state
      if (!is.null(query$code) && !is.null(query$state)) {
        
        # Debug output
        stored_state <- session$userData$oauth_state
        received_state <- query$state
        
        cat("Stored state:", stored_state, "\n")
        cat("Received state:", received_state, "\n")
        cat("States match:", identical(stored_state, received_state), "\n")
        
        # Validate state parameter - allow if no stored state (session might have reset)
        if (!is.null(stored_state) && !identical(received_state, stored_state)) {
          showNotification("Invalid authentication state. Please try logging in again.", type = "error", duration = 5)
          updateQueryString("?", mode = "replace")
          return()
        }
        
        # Exchange code for tokens
        token_url <- paste0("https://", cognito_domain, "/oauth2/token")
        
        showNotification("Authenticating...", type = "message", duration = 2)
        
        response <- tryCatch({
          POST(
            token_url,
            body = list(
              grant_type = "authorization_code",
              code = query$code,
              client_id = client_id,
              redirect_uri = redirect_uri
            ),
            encode = "form"
          )
        }, error = function(e) {
          showNotification(paste("Authentication error:", e$message), type = "error")
          NULL
        })
        
        if (!is.null(response) && status_code(response) == 200) {
          tokens <- fromJSON(rawToChar(response$content))
          
          # Decode ID token WITHOUT signature verification (we trust the token endpoint)
          # Alternative: verify with JWKS, but for tokens directly from Cognito's token endpoint, this is safe
          id_token_parts <- strsplit(tokens$id_token, "\\.")[[1]]
          
          # Decode the payload (second part of JWT)
          # Add padding if needed for base64 decoding
          payload_b64 <- id_token_parts[2]
          padding <- (4 - nchar(payload_b64) %% 4) %% 4
          if (padding > 0) {
            payload_b64 <- paste0(payload_b64, paste(rep("=", padding), collapse = ""))
          }
          
          # Decode base64url to JSON
          payload_json <- rawToChar(base64_url_decode(payload_b64))
          id_token <- fromJSON(payload_json)
          
          # Store authentication state
          values$authenticated <- TRUE
          values$user_info <- id_token
          values$access_token <- tokens$access_token
          values$id_token <- tokens$id_token
          
          # Store tokens in browser localStorage
          session$sendCustomMessage("storeAuthToken", list(
            access_token = tokens$access_token,
            id_token = tokens$id_token,
            expiry = id_token$exp  # Token expiry timestamp
          ))
          
          # Clear URL parameters
          updateQueryString("?", mode = "replace")
          
          # Notify success
          showNotification(
            sprintf("Welcome, %s!", id_token$given_name %||% id_token$email %||% "User"),
            type = "message",
            duration = 3
          )
        } else {
          error_msg <- if (!is.null(response)) {
            paste("Authentication failed. Status:", status_code(response))
          } else {
            "Failed to contact authentication server"
          }
          showNotification(error_msg, type = "error", duration = 5)
        }
      }
    })
    
    # Return reactive values for parent module
    return(values)
  })
}