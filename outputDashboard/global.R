#Need to restart R Session to run

# Load packages -----------------------------------------------------------
library(shiny)
library(fresh)
library(plotly)
library(shinyWidgets)
library(ggplot2)
library(rlang)
library(jsonlite)
library(stringr)
library(dplyr)
library(scales)
library(shinyjs)
library(shinycssloaders)
library(purrr)
library(doParallel)
library(foreach)
library(ggrepel)
library(flextable)

# Modules -----------------------------------------------------------------
source("./Modules/cellContentModule.R")


# Plots -------------------------------------------------------------------
source("./Functions/singlePlots.R")
source("./Functions/ahlePlots.R")


# Preprocessing -----------------------------------------------------------
source("./Functions/singlePreprocessing.R")
source("./Functions/ahlePreprocessing.R")


# List available data -----------------------------------------------------
data_folder <- "data"
file_names <- if (dir.exists(data_folder)) {
  list.files(path = data_folder, full.names = FALSE)
} else {
  character(0)
}

# Parse model names -------------------------------------------------------
parse_model_name <- function(filenames) {
  str_match(filenames, "^[^_]+_[^_]+_([^_]+_(ideal|current|zeromorb|zeromort))_[^_]+_full\\.csv$")[, 2]
}

parse_model_group <- function(filenames) {
  str_match(filenames, "^[^_]+_[^_]+_([^_]+)_(ideal|current|zeromorb|zeromort)_[^_]+_full\\.csv$")[, 2]
}

clean_model_label <- function(model_names) {
  model_names %>%
    stringr::str_replace("_ideal$", " (Ideal)") %>%
    stringr::str_replace("_current$", " (Current)") %>%
    stringr::str_replace("_zeromorb$", " (Zero Morbidity)") %>%
    stringr::str_replace("_zeromort$", " (Zero Mortality)")
}


single_models <- parse_model_name(file_names)
valid_idx <- !is.na(single_models)
single_models <- single_models[valid_idx]
single_model_labels <- clean_model_label(single_models)
file_names_cleaned <- file_names[valid_idx]
named_choices <- setNames(file_names_cleaned, single_model_labels)

group_model_labels <- parse_model_group(file_names) %>% .[!is.na(.)]


loadData <- function(filePath) {
  if (file.exists(filePath)) {
    config <- fromJSON(filePath)
    return(config)
  } else {
    return(NULL)
  }
}


# Clean dropdown menu labels ----------------------------------------------
plot_list_single_run <- list(
  "Population" = list(
    "Popualtion Bar" = "populationBar",
    "Population Bar (by Sex)" = "populationBarSex",
    "Stacked Count by Sex" = "populationStackedCountSex",
    "Stacked Percent by Sex" = "populationStackedPercentSex",
    "Stacked Count by Age" = "populationStackedCountAge",
    "Stacked Percent by Age" = "populationStackedPercentAge",
    "Pie by Sex" = "populationPieSex",
    "Pie by Age" = "populationPieAge"
  ),
  "Mortality" = list(
    "Mortality Bar" = "mortalityBar",
    "Bar by Sex" = "mortalityBarSex",
    "Stacked Count by Sex" = "mortalityStackedCountSex",
    "Stacked Percent by Sex" = "mortalityStackedPercentSex",
    "Stacked Count by Age" = "mortalityStackedCountAge",
    "Stacked Percent by Age" = "mortalityStackedPercentAge",
    "Pie by Sex" = "mortalityPieSex",
    "Pie by Age" = "mortalityPieAge"
  ),
  "Production & Value" = list(
    "Liveweight Bar" = "liveweightBarCount",
    "Population Growth (Count)" = "populationGrowthBarCount",
    "Population Growth (Percent)" = "populationGrowthBarPercent",
    "Manure Production" = "productionManureCount",
    "Hide Production" = "productionHideCount",
    "Milk Production" = "productionMilkCount",
    "Manure Value" = "valueManureCount",
    "Hide Value" = "valueHideCount",
    "Offtake Value" = "valueOfftakeCount"
  ),
  "Costs" = list(
    "Feed Cost" = "costFeedCount",
    "Labour Cost" = "costLabourCount",
    "Health Cost" = "costHealthCount",
    "Infrastructure Cost" = "costInfrastructureCount",
    "Capital Cost" = "costCapitalCount",
    "Total Cost by Age" = "costTotalCountAge",
    "Total Cost by Item" = "costTotalCountItem",
    "Stacked Total Cost by Age" = "costTotalStackedAge",
    "Stacked Total Cost by Item" = "costTotalStackedItem"
  ),
  "Summary Metrics" = list(
    "Gross Margin" = "grossMargin",
    "Net Value" = "netValue",
    "Value Increase" = "valueIncrease",
    "Herd Value Increase" = "valueHerdIncrease",
    "Cost vs Value Waterfall" = "costValueWaterfall"
  )
)

plot_list_ahle <- list(
  "Plots" = list(
    "Gross Margin" = "ahle_gross_margin",
    "Costs" = "ahle_cost_plot",
    "Costs vs Values" = "ahle_values_vs_cost",
    "Attribution (Overall)" = "ahle_donut_overall",
    "Attribution by Category" = "ahle_donut_breakdown",
    "Attribution Treemap" = "ahle_treemap"
  ),
  "Tables" = list(
    "Summary Table" = "ahle_summary_table"
  )
)
