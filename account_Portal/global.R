library(config)
library(DBI)
library(dplyr)
library(DT)
library(flextable)
library(foreach)
library(fresh)
library(ggplot2)
library(ggrepel)
library(httr)
library(ISOcodes)
library(jsonlite)
library(plotly)
library(purrr)
library(rlang)
library(scales)
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyWidgets)
library(stringr)
library(yaml)
library(doParallel)

countries <- ISOcodes::ISO_3166_1$Name
languages <- ISOcodes::ISO_639_2$Name

# ------------------------------------------------------------------------------
# Source Cofig                 
# ------------------------------------------------------------------------------
config <- yaml::read_yaml("../config.yml")

# ------------------------------------------------------------------------------
# Source Helper Functions                 
# ------------------------------------------------------------------------------
source("../r_scripts/db_utils.R")
source("../r_scripts/queries.R")
source("../r_scripts/dpm_api_helpers.R")

source("./Functions/radioImages.R")
source("./Functions/create_yaml.R")
source("./Functions/extract_yaml_names.R")
source("./Functions/singlePlots.R")
source("./Functions/ahlePlots.R")
source("./Functions/singlePreprocessing.R")
source("./Functions/ahlePreprocessing.R")

# ------------------------------------------------------------------------------
# Source Modules                 
# ------------------------------------------------------------------------------
source("./Modules/outputDashboardModule.R")
source("./Modules/cellContentModule.R")

# ------------------------------------------------------------------------------
# Define 'Status" Tags for Model Table             
# ------------------------------------------------------------------------------
#Sample model data 
modelData <- data.frame(
  modelName = c(""),
  modelType = c(""),
  dateCreated = c(""),
  dateCompleted = c(""),
  modelVersion = c(""),
  statusTags = c(""),
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

# List available data -----------------------------------------------------
data_folder <- "data"
file_names <- if (dir.exists(data_folder)) {
  list.files(path = data_folder, full.names = FALSE)
} else {
  character(0)
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
    "Population Bar" = populationBar,
    "Population Bar (by Sex)" = populationBarSex,
    "Stacked Count by Sex" = populationStackedCountSex,
    "Stacked Percent by Sex" = populationStackedPercentSex,
    "Stacked Count by Age" = populationStackedCountAge,
    "Stacked Percent by Age" = populationStackedPercentAge,
    "Pie by Sex" = populationPieSex,
    "Pie by Age" = populationPieAge,
    "Mortality Bar" = mortalityBar,
    "Bar by Sex" = mortalityBarSex,
    "Stacked Count by Sex" = mortalityStackedCountSex,
    "Stacked Percent by Sex" = mortalityStackedPercentSex,
    "Stacked Count by Age" = mortalityStackedCountAge,
    "Stacked Percent by Age" = mortalityStackedPercentAge,
    "Pie by Sex" = mortalityPieSex,
    "Pie by Age" = mortalityPieAge,
    "Liveweight Bar" = liveweightBarCount,
    "Population Growth (Count)" = populationGrowthBarCount,
    "Population Growth (Percent)" = populationGrowthBarPercent,
    "Manure Production" = productionManureCount,
    "Hide Production" = productionHideCount,
    "Milk Production" = productionMilkCount,
    "Manure Value" = valueManureCount,
    "Hide Value" = valueHideCount,
    "Offtake Value" = valueOfftakeCount,
    "Feed Cost" = costFeedCount,
    "Labour Cost" = costLabourCount,
    "Health Cost" = costHealthCount,
    "Infrastructure Cost" = costInfrastructureCount,
    "Capital Cost" = costCapitalCount,
    "Total Cost by Age" = costTotalCountAge,
    "Total Cost by Item" = costTotalCountItem,
    "Stacked Total Cost by Age" = costTotalStackedAge,
    "Stacked Total Cost by Item" = costTotalStackedItem,
    "Gross Margin" = grossMargin,
    "Net Value" = netValue,
    "Value Increase" = valueIncrease,
    "Herd Value Increase" = valueHerdIncrease,
    "Cost vs Value Waterfall" = costValueWaterfall
)

plot_list_ahle <- list(
    "Gross Margin" = ahle_gross_margin,
    "Costs" = ahle_cost_plot,
    "Costs vs Values" = ahle_values_vs_cost,
    "Attribution (Overall)" = ahle_donut_overall,
    "Attribution by Category" = ahle_donut_breakdown,
    "Attribution Treemap" = ahle_treemap,
    "Summary Table" = ahle_summary_table
)
