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

# Load local files --------------------------------------------------------
source("./Modules/cellContentModule.R")
source("./Functions/plots.R")

data <- read.csv("./Data/cattle_trial_CLM_current_summary.csv")

loadData <- function(filePath) {
  if (file.exists(filePath)) {
    config <- fromJSON(filePath)
    return(config)
  } else {
    return(NULL)
  }
}

plotTypes <- list(
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