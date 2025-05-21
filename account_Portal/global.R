library(shiny)
library(fresh)
library(shinyWidgets)
library(ISOcodes)
library(jsonlite)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(DBI)
library(config)
library(yaml)
library(shinyjs)
library(httr)

countries <- ISOcodes::ISO_3166_1$Name
languages <- ISOcodes::ISO_639_2$Name

config <- yaml::read_yaml("../config.yml")
source("../r_scripts/db_utils.R")
source("../r_scripts/queries.R")

#Sample model data 
modelData <- data.frame(
  modelName = c("Ethiopia Cattle", "Ethiopia Cattle2", "Indonesia Poultry"),
  dateCreated = as.POSIXct(c("2025-01-01 12:00:00", "2025-01-15 08:30:00", "2025-01-20 14:45:00")),
  dateCompleted = as.POSIXct(c("2025-01-02 12:00:00", "2025-01-16 09:00:00", NA)),
  modelVersion = c("v1.0", "v1.1", "v1.2"),
  statusTags = c("Complete", "Failed", "Processing"),
  stringsAsFactors = FALSE
)

modelData$statusTags <- ifelse(
  modelData$statusTags == "Complete", 
  "<span class='status-pill-complete'>Complete</span>", 
  ifelse(
    modelData$statusTags == "Processing", 
    "<span class='status-pill-processing'>Processing</span>", 
    "<span class='status-pill-failed'>Failed</span>"
  )
)

#Sample Dashboard Data
dashData <- data.frame(
  dashboardName = c("Dashboard A", "Dashboard B", "Dashboard C"),
  dateCreated = as.POSIXct(c("2025-01-01 12:00:00", "2025-01-15 08:30:00", "2025-01-20 14:45:00")),
  model = c("Ethiopia Cattle", "Ethiopia Cattle", "Indonesia Poultry"),
  stringsAsFactors = FALSE
)
