library(dplyr)
library(tidyr)

summarize_data <- function(data){
  data <- data %>%
    pivot_longer(-1, names_to = "Run", values_to = "Value") %>%
    mutate(Value = as.numeric(Value)) %>%
    group_by(X = .[[1]]) %>%
    summarise(
      Min = min(Value, na.rm = TRUE),
      Q1 = quantile(Value, 0.25, na.rm = TRUE),
      Median = median(Value, na.rm = TRUE),
      Mean = mean(Value, na.rm = TRUE),
      Q3 = quantile(Value, 0.75, na.rm = TRUE),
      Max = max(Value, na.rm = TRUE),
      SD = sd(Value, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(data)
}

