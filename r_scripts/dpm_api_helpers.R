library(httr)
library(jsonlite)
library(readr)
library(yaml)

auth_token <- ""

headers <- add_headers(Authorization = paste("Bearer", auth_token))

# ------------------------------------------------------------------------------
# List available inputs (Bucket 1 - storage; Bucket 2 - run)                
# ------------------------------------------------------------------------------

list_files_from_api <- function(bucket, user_id = NULL) {
  
  bucket_name <- paste0("gbads-modelling-", bucket)
  url <- paste0("https://gbadske.org/api/dpm/list?bucket_name=", bucket_name)
  full_id <- paste0("user_", user_id)
  
  if (!is.null(user_id)) {
    prefix <- paste0("dpm/", full_id, "/")
    url <- paste0(url, "&prefix=", URLencode(prefix))
  }
  
  res <- GET(url, headers)
  
  if (status_code(res) == 200) {
    parsed <- content(res, "parsed", simplifyVector = TRUE)
    return(parsed)
  } else {
    return(NULL)
  }
}

list_files_from_api("outputs", "1")

# ------------------------------------------------------------------------------
# Download .yaml (Bucket 1 - storage; Bucket 2 - run)                 
# ------------------------------------------------------------------------------
download_yaml_from_api <- function(bucket, user_id, file_name) {
  
  full_id <- paste0("user_", user_id)
  
  object_name <- paste0("dpm/", full_id, "/", file_name)
  bucket_name <- paste0("gbads-modelling-", bucket)
  url <- paste0(
    "https://gbadske.org/api/dpm/download?",
    "bucket_name=", bucket_name,
    "&object_name=", URLencode(object_name)
  )
  
  res <- GET(url, headers)
  
  if (status_code(res) == 200) {
    raw_data <- content(res, "raw")
    yaml_text <- rawToChar(raw_data)
    parsed_yaml <- yaml::yaml.load(yaml_text)
    return(parsed_yaml)
  } else {
    return(NULL)
  }
}

# Example use
#df <- download_yaml_from_api("storage", "1", "trial_CLM_current.yaml")

# ------------------------------------------------------------------------------
# Download .csv (Bucket 3)                 
# ------------------------------------------------------------------------------
download_csv_from_api <- function(bucket, user_id, file_name) {
  full_id <- paste0("user_", user_id)
  object_name <- paste0("dpm/", full_id, "/", file_name)
  bucket_name <- paste0("gbads-modelling-", bucket)
  
  url <- paste0(
    "https://gbadske.org/api/dpm/download?",
    "bucket_name=", bucket_name,
    "&object_name=", URLencode(object_name)
  )
  
  res <- GET(url, headers)
  
  if (status_code(res) == 200) {
    raw_data <- content(res, "raw")
    con <- rawConnection(raw_data, open = "rb")
    df <- suppressMessages(readr::read_csv(con, show_col_types = FALSE))
    return(df)
  } else {
    return(NULL)
  }
}

# Example use
#df <- download_csv_from_api("outputs", "123", "params_cattle_full.csv")

# ------------------------------------------------------------------------------
# Upload inputs (Bucket 1 - to be stored; Bucket 2 - to be run)                 
# ------------------------------------------------------------------------------
upload_yaml_to_api <- function(bucket, user_id, file_name, yaml_object) {
  full_id <- paste0("user_", user_id)
  yaml_text <- as.yaml(yaml_object)
  tmp_file <- tempfile(fileext = ".yaml")
  writeLines(yaml_text, tmp_file)
  
  object_name <- paste0("dpm/", full_id, "/", file_name)
  bucket_name <- paste0("gbads-modelling-", bucket)
  url <- paste0(
    "https://gbadske.org/api/dpm/upload?",
    "bucket_name=", bucket_name,
    "&object_name=", URLencode(object_name)
  )
  
  res <- POST(
    url = url,
    body = list(file = upload_file(tmp_file)),
    headers
  )
  
  if (status_code(res) %in% 200:299) {
    cat("YAML uploaded successfully:", file_name, "\n")
    return(TRUE)
  } else {
    cat("YAML not uploaded:", file_name, "\n")
    return(FALSE)
  }
}

# Example use
#upload_yaml_to_api("storage", "1", "params_cattle.yaml", df)
