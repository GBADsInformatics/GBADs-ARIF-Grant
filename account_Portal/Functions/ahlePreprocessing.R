# Helper Functions --------------------------------------------------------
format_large_num <- function(x) {
  ifelse(
    abs(x) >= 1e9, 
    paste0(round(x / 1e9, 3), "B"),
    ifelse(abs(x) >= 1e6,
           paste0(round(x / 1e6, 3), "M"),
           ifelse(abs(x) >= 1e3,
                  paste0(round(x / 1e3, 3), "K"),
                  as.character(round(x, 0))
           )
    )
  )
}

# Data Full ---------------------------------------------------------------
preprocessing_data_full <- function(all_data){
  
  group_map <- c("JM"="Juvenile Male", "JF"="Juvenile Female", 
               "SM"="Subadult Male", "SF"="Subadult Female", 
               "AM"="Adult Male", "AF"="Adult Female")

data_full <- all_data %>%
  mutate(
    Group = recode(Group, !!!group_map),
    Group_upper = sub(" .*", "", Group)  # Extract "Juvenile", "Subadult", etc.
  )
  
  data_full <- all_data %>%
    mutate(
      Group = recode(Group,
                     "JM" = "Juvenile Male", "JF" = "Juvenile Female",
                     "SM" = "Subadult Male", "SF" = "Subadult Female",
                     "AM" = "Adult Male", "AF" = "Adult Female"
      ),
      Group_upper = recode(Group,
                           "Juvenile Male" = "Juvenile", "Juvenile Female" = "Juvenile",
                           "Subadult Male" = "Subadult", "Subadult Female" = "Subadult",
                           "Adult Male" = "Adult", "Adult Female" = "Adult"
      )
    )
  
  data_full <- bind_rows(
    data_full %>%
      filter(Group_upper %in% c("Juvenile", "Subadult", "Adult")) %>%
      group_by(Item, Scenario, Iteration, Group_upper) %>%
      summarize(Value = sum(Value), .groups = "drop") %>%
      rename(Group = Group_upper),
    data_full %>% select(-Group_upper)
  ) %>%
    rename(
      scenario = Scenario,
      iteration = Iteration,
      value = Value,
      group = Group,
      variable = Item
    )
  
  iteration_ranks <- data_full %>%
    filter(variable == "Gross Margin", group == "Overall") %>%
    group_by(scenario) %>%
    mutate(iteration_rank = rank(-value, ties.method = "min")) %>%
    select(scenario, iteration, iteration_rank)
  
  data_full <- data_full %>%
    left_join(iteration_ranks, by = c("scenario", "iteration")) %>%
    select(-iteration) %>%
    rename(iteration = iteration_rank)
  
  data_full <- data_full %>%
    mutate(sign = case_when(
      variable == "Gross Margin" ~ 0,
      variable %in% c("Health Cost", "Labour Cost", "Feed Cost") ~ -1,
      variable %in% c("Value of Manure", "Value of Milk", "Value of Offtake", "Value of Herd Increase") ~ 1,
      TRUE ~ NA_real_
    ))
  data_full
}

# Data Summary ------------------------------------------------------------
preprocessing_data_summary <- function(data_full){
  data_summary <- data_full %>%
    group_by(variable, sign, scenario) %>%
    summarize(
      mean = mean(value),
      q2_5 = quantile(value, 0.025),
      q97_5 = quantile(value, 0.975),
      .groups = "drop"
    ) %>%
    filter(!is.na(sign)) %>%
    mutate(
      mean_pretty = case_when(
        mean < 0.01 ~ formatC(mean, digits = 3, format = "f"),
        mean < 20 ~ formatC(mean, digits = 2, format = "f"),
        TRUE ~ formatC(mean, digits = 0, format = "f", big.mark = ",")
      ),
      mean_sign = if_else(sign == 0, 1, sign) * mean,
      q2_5_sign = if_else(sign == 0, 1, sign) * q2_5,
      q97_5_sign = if_else(sign == 0, 1, sign) * q97_5,
      mean_sign_label = if_else(
        abs(mean) < 1e8,
        paste0(formatC(mean_sign * 1e-6, format = "f", digits = 1), " M"),
        paste0(formatC(mean_sign * 1e-9, format = "f", digits = 1), " B")
      ),
      variable_type = case_when(
        variable %in% c("Feed Cost", "Health Cost", "Labour Cost") ~ "Cost",
        variable == "Gross Margin" ~ "Gross Margin",
        TRUE ~ "Value"
      )
    )
  
  data_summary
}


# AHLE Results ------------------------------------------------------------
preprocessing_ahle_results <- function(data_full){
  ahle_results <- data_full %>%
    filter(variable %in% c("Gross Margin", "Health Cost"),
           group %in% c("Overall", "Adult Female", "Adult Male", "Subadult", "Juvenile")) %>%
    select(iteration, scenario, group, variable, value) %>%
    pivot_wider(names_from = scenario, values_from = value) %>%
    {
      ahle_data <- .
      ahle_ahe <- ahle_data %>%
        filter(variable == "Health Cost", !is.na(Current)) %>%
        select(iteration, group, AHLEahe = Current)
      
      ahle_data %>%
        filter(variable == "Gross Margin") %>%
        left_join(ahle_ahe, by = c("iteration", "group")) %>%
        mutate(
          AHLEmort  = ZeroMort - Current - AHLEahe,
          AHLEmorb  = ZeroMorb - Current - AHLEahe,
          AHLEtot   = Ideal - Current,
          AHLEjoint = AHLEtot - AHLEahe - AHLEmorb - AHLEmort
        ) %>%
        select(group, AHLEmort, AHLEmorb, AHLEjoint, AHLEahe, AHLEtot) %>%
        pivot_longer(cols = AHLEmort:AHLEtot, names_to = "AHLE_type", values_to = "value") %>%
        group_by(group, AHLE_type) %>%
        summarize(
          mean_value = mean(value),
          q2_5 = quantile(value, 0.025),
          q97_5 = quantile(value, 0.975),
          sd = sd(value),
          .groups = "drop_last"
        ) %>%
        mutate(
          perc = if_else(
            AHLE_type %in% c("AHLEahe", "AHLEmorb", "AHLEmort", "AHLEjoint"),
            100 * mean_value / mean_value[AHLE_type == "AHLEtot"],
            NA_real_
          )
        ) %>%
        ungroup() %>%
        mutate(AHLE_type_text = recode(
          AHLE_type,
          AHLEahe   = "Animal health expenditure",
          AHLEmorb  = "Direct morbidity losses",
          AHLEmort  = "Direct mortality losses",
          AHLEjoint = "Joint morbidity and mortality losses",
          AHLEtot   = "Total AHLE",
          AHLEtot_ton = "AHLE per ton of meat produced"
        ))
    }
  
  ahle_results
}

