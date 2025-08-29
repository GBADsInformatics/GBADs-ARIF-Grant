# ==============================================================================
# Dependencies
# ==============================================================================
library(httr)
library(jsonlite)
library(readr)
library(yaml)

# ==============================================================================
# Auth (DO NOT COMMIT SECRETS)
# ==============================================================================
auth_token <- ""
headers    <- httr::add_headers(Authorization = paste("Bearer", auth_token))

# ==============================================================================
# Helpers
# ==============================================================================
.api_base <- "https://gbadske.org/api/dpm"

.bucket_name <- function(bucket) paste0("gbads-modelling-", bucket)
.user_prefix <- function(user_id) paste0("dpm/user_", user_id, "/")

# ==============================================================================
# Users
# ==============================================================================
get_user_info <- function(user_id) {
  url <- paste0(.api_base, "/user/", user_id)
  res <- httr::GET(url, headers)
  if (httr::status_code(res) == 200) {
    httr::content(res, "parsed", simplifyVector = TRUE)
  } else {
    warning("Request failed: ", httr::status_code(res))
    NULL
  }
}

safe_get_user <- function(user_id) {
  out <- tryCatch(get_user_info(as.character(user_id)), error = function(e) NULL)
  if (is.null(out) || is.null(out$user_id)) NULL else out
}

create_user <- function(firstname, lastname, email, country, language, role) {
  url  <- paste0(.api_base, "/user")
  body <- list(
    user_firstname = firstname,
    user_lastname  = lastname,
    user_email     = email,
    user_country   = country,
    user_language  = language,
    user_role      = role
  )
  res <- httr::POST(
    url,
    headers,
    httr::add_headers(Accept = "application/json", `Content-Type` = "application/json"),
    body = body,
    encode = "json"
  )
  httr::stop_for_status(res)
  httr::content(res, "parsed", simplifyVector = TRUE)
}

delete_user <- function(user_id) {
  url <- paste0(.api_base, "/user/", user_id)
  res <- httr::DELETE(url, headers, httr::add_headers(Accept = "application/json"))
  httr::stop_for_status(res)
  ct <- httr::headers(res)[["content-type"]]
  if (!is.null(ct) && grepl("application/json", ct, ignore.case = TRUE)) {
    httr::content(res, "parsed", simplifyVector = TRUE)
  } else {
    invisible(TRUE)
  }
}

# ==============================================================================
# Object Delete
# ==============================================================================
delete_file_from_api <- function(bucket, user_id = NULL, file_name) {
  stopifnot(is.character(bucket), nzchar(bucket), is.character(file_name), nzchar(file_name))
  bucket_name <- .bucket_name(bucket)
  
  if (!startsWith(file_name, "dpm/")) {
    prefix <- if (!is.null(user_id)) .user_prefix(user_id) else "dpm/"
    object_name <- paste0(prefix, file_name)
  } else {
    object_name <- file_name
  }
  
  res <- httr::DELETE(
    url   = paste0(.api_base, "/delete"),
    query = list(
      bucket_name = bucket_name,
      object_name = object_name
    ),
    headers
  )
  
  code <- httr::status_code(res)
  if (code %in% c(200L, 204L)) return(TRUE)
  
  msg <- tryCatch({
    parsed <- httr::content(res, "parsed", simplifyVector = TRUE)
    if (is.list(parsed) && length(parsed)) paste(capture.output(str(parsed)), collapse = "\n") else ""
  }, error = function(e) "")
  warning(sprintf("Delete failed (%s). %s", code, msg))
  FALSE
}

# ==============================================================================
# Listing
# ==============================================================================
list_files_from_api <- function(bucket, user_id = NULL) {
  bucket_name <- .bucket_name(bucket)
  url <- paste0(.api_base, "/list?bucket_name=", utils::URLencode(bucket_name, reserved = TRUE))
  if (!is.null(user_id)) {
    url <- paste0(
      url,
      "&prefix=",
      utils::URLencode(.user_prefix(user_id), reserved = TRUE)
    )
  }
  res <- httr::GET(url, headers)
  if (httr::status_code(res) == 200) {
    httr::content(res, "parsed", simplifyVector = TRUE)
  } else {
    NULL
  }
}

list_outputs_files_from_api <- function(bucket, user_id) {
  bucket_name <- .bucket_name(bucket)
  url <- paste0(
    .api_base, "/list?",
    "bucket_name=", utils::URLencode(bucket_name, reserved = TRUE),
    "&prefix=",     utils::URLencode(.user_prefix(user_id), reserved = TRUE)
  )
  res <- httr::GET(url, headers)
  if (httr::status_code(res) != 200) return(character(0))
  
  out <- tryCatch(
    jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8")),
    error = function(e) NULL
  )
  if (is.null(out)) return(character(0))
  
  files <- if (is.character(out)) {
    out
  } else {
    nm <- intersect(c("name","Name","key","Key","object_name","ObjectName"), names(out))
    if (length(nm)) out[[nm[1]]] else character(0)
  }
  basename(files)
}

list_metadata_files_from_api <- function(bucket, user_id) {
  bucket_name <- .bucket_name(bucket)
  url <- paste0(
    .api_base, "/list?",
    "bucket_name=", utils::URLencode(bucket_name, reserved = TRUE),
    "&prefix=",     utils::URLencode(.user_prefix(user_id), reserved = TRUE)
  )
  res <- httr::GET(url, headers)
  if (httr::status_code(res) != 200) return(character(0))
  
  out <- tryCatch(
    jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8")),
    error = function(e) NULL
  )
  if (is.null(out)) return(character(0))
  
  files <- if (is.character(out)) {
    out
  } else {
    nm <- intersect(c("name","Name","key","Key","object_name","ObjectName"), names(out))
    if (length(nm)) out[[nm[1]]] else character(0)
  }
  
  files <- basename(files)
  files[grepl("_metadata\\.json$", files, ignore.case = TRUE)]
}

# ==============================================================================
# Downloads
# ==============================================================================
download_yaml_from_api <- function(bucket, user_id, file_name) {
  bucket_name <- .bucket_name(bucket)
  object_name <- paste0(.user_prefix(user_id), file_name)
  url <- paste0(
    .api_base, "/download?",
    "bucket_name=", bucket_name,
    "&object_name=", utils::URLencode(object_name, reserved = TRUE)
  )
  
  res <- httr::GET(url, headers)
  if (httr::status_code(res) != 200) return(NULL)
  
  raw_data  <- httr::content(res, "raw")
  yaml_text <- rawToChar(raw_data)
  yaml::yaml.load(yaml_text)
}

get_yaml_text_from_api <- function(bucket, user_id, file_name) {
  bucket_name <- .bucket_name(bucket)
  object_name <- paste0(.user_prefix(user_id), file_name)
  url <- paste0(
    .api_base, "/download?",
    "bucket_name=", utils::URLencode(bucket_name, reserved = TRUE),
    "&object_name=", utils::URLencode(object_name, reserved = TRUE)
  )
  res <- httr::GET(url, headers)
  if (httr::status_code(res) == 200) {
    raw <- httr::content(res, "raw")
    rawToChar(raw)
  } else {
    NULL
  }
}

download_json_from_api <- function(bucket, user_id, file_name) {
  bucket_name <- .bucket_name(bucket)
  object_name <- paste0(.user_prefix(user_id), file_name)
  url <- paste0(
    .api_base, "/download?",
    "bucket_name=", bucket_name,
    "&object_name=", utils::URLencode(object_name, reserved = TRUE)
  )
  
  res <- httr::GET(url, headers)
  if (httr::status_code(res) != 200) return(NULL)
  
  raw_data  <- httr::content(res, "raw")
  json_text <- rawToChar(raw_data)
  jsonlite::fromJSON(json_text, simplifyVector = FALSE)
}

download_csv_from_api <- function(bucket, user_id, file_name) {
  bucket_name <- .bucket_name(bucket)
  object_name <- paste0(.user_prefix(user_id), file_name)
  url <- paste0(
    .api_base, "/download?",
    "bucket_name=", utils::URLencode(bucket_name, reserved = TRUE),
    "&object_name=", utils::URLencode(object_name, reserved = TRUE)
  )
  
  res <- httr::GET(url, headers)
  if (httr::status_code(res) != 200) return(NULL)
  
  raw_data <- httr::content(res, "raw")
  tryCatch(
    readr::read_csv(I(raw_data), show_col_types = FALSE),
    error = function(e) {
      txt <- rawToChar(raw_data)
      con <- textConnection(txt)
      on.exit(close(con), add = TRUE)
      utils::read.csv(con, stringsAsFactors = FALSE)
    }
  )
}

# ==============================================================================
# Uploads
# ==============================================================================
upload_yaml_to_api <- function(bucket, user_id, file_name, yaml_object) {
  yaml_text <- as.yaml(yaml_object)
  tmp_file  <- tempfile(fileext = ".yaml")
  writeLines(yaml_text, tmp_file)
  
  bucket_name <- .bucket_name(bucket)
  object_name <- paste0(.user_prefix(user_id), file_name)
  url <- paste0(
    .api_base, "/upload?",
    "bucket_name=", bucket_name,
    "&object_name=", utils::URLencode(object_name, reserved = TRUE)
  )
  
  res <- httr::POST(url = url, body = list(file = httr::upload_file(tmp_file)), headers)
  if (httr::status_code(res) %in% 200:299) {
    cat("YAML uploaded successfully:", file_name, "\n")
    TRUE
  } else {
    cat("YAML not uploaded:", file_name, "\n")
    FALSE
  }
}

upload_json_to_api <- function(bucket, user_id, file_name, json_object) {
  json_txt <- jsonlite::toJSON(json_object, auto_unbox = TRUE, pretty = TRUE)
  tmp_file <- tempfile(fileext = ".json")
  writeLines(json_txt, tmp_file)
  on.exit(unlink(tmp_file), add = TRUE)
  
  bucket_name <- .bucket_name(bucket)
  object_name <- paste0(.user_prefix(user_id), file_name)
  url <- paste0(
    .api_base, "/upload?",
    "bucket_name=", bucket_name,
    "&object_name=", utils::URLencode(object_name, reserved = TRUE)
  )
  
  res <- httr::POST(url = url, body = list(file = httr::upload_file(tmp_file)), headers)
  if (httr::status_code(res) %in% 200:299) {
    cat("JSON uploaded successfully:", file_name, "\n")
    TRUE
  } else {
    cat("JSON not uploaded:", file_name, "\n")
    FALSE
  }
}
