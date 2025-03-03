library(DBI)
library(config)

source('queries.R')

db_connect <- function() {
  
  # Read config file
  db_config <- config::get("db")
  
  # Connect to the database
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = db_config$dbname,
    host = db_config$host,
    port = db_config$port,
    user = db_config$user,
    password = db_config$password
  )
  
  return(con)
  
}

db_query <- function(query) {
  con <- db_connect()
  
  res <- dbSendQuery(con, query)
  result <- dbFetch(res)

  dbDisconnect(con)
  return(result)
}



