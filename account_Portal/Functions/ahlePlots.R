# Colours -----------------------------------------------------------------
colours_1 <-  c("#1A8FE3","#E6C229","#A155B9","#009B72")
colours_2 <- c("#F17105","#1A8FE3","#E6C229","#D11149","#6610F2","#006E90")
ahle_colour <- "#F17105"

# Gross Margin ------------------------------------------------------------
format_short <- function(x, digits = 1) {
  ax <- abs(x)
  sign_chr <- ifelse(x < 0, "\u2212", "")  # nice minus
  out <- ifelse(
    ax >= 1e9, paste0(formatC(ax/1e9, format="f", digits=digits), " B"),
    ifelse(ax >= 1e6, paste0(formatC(ax/1e6, format="f", digits=digits), " M"),
           ifelse(ax >= 1e3, paste0(formatC(ax/1e3, format="f", digits=digits), " K"),
                  formatC(ax, format="f", digits=0))
    )
  )
  paste0(sign_chr, out)
}

ahle_gross_margin <- function(ahle_data) {
  data_summary <- ahle_data$summary
  
  data <- data_summary %>%
    mutate(
      mean_sign = as.numeric(mean_sign),
      q2_5_sign = as.numeric(q2_5_sign),
      q97_5_sign = as.numeric(q97_5_sign),
      
      variable_group = dplyr::case_when(
        variable %in% c("Feed Cost", "Health Cost", "Labour Cost") ~ "Costs",
        variable %in% c("Value of Herd Increase", "Value of Manure", "Value of Milk", "Value of Offtake") ~ "Values",
        variable == "Gross Margin" ~ "",
        TRUE ~ "Other"
      ),
      variable_clean = dplyr::case_when(
        grepl("Cost", variable) ~ gsub(" Cost", "", variable),
        grepl("Value of ", variable) ~ sub("Value of ", "", variable),
        TRUE ~ variable
      ),
      mean_sign_label2 = format_short(mean_sign, digits = 1)
    )
  
  data$variable_group <- factor(data$variable_group, levels = c("Values", "Costs", "", "Other"))
  
  n_vars <- length(unique(data$variable_clean))
  hlines <- seq(1.5, n_vars - 0.5, by = 1)
  
  ggplot(data, aes(y = mean_sign, x = variable_clean, group = scenario, fill = scenario)) +
    geom_vline(xintercept = hlines, color = "gray80", size = 0.5) +
    geom_hline(yintercept = 0, colour = "#111111") +
    geom_bar(stat = "identity", position = position_dodge(width = 1)) +
    geom_errorbar(aes(ymin = q2_5_sign, ymax = q97_5_sign),
                  width = 0.2, position = position_dodge(width = 1)) +
    geom_label(
      aes(
        label = mean_sign_label2,
        y = ifelse(sign != -1,
                   q97_5_sign + 0.02 * max(q97_5_sign, na.rm = TRUE),
                   q97_5_sign - 0.02 * max(abs(q97_5_sign), na.rm = TRUE)),
        hjust = ifelse(sign != -1, 0, 1)
      ),
      position = position_dodge(width = 1),
      size = 4
    ) +
    coord_flip() +
    facet_grid(variable_group ~ ., scales = "free_y", space = "free_y", switch = "y") +
    labs(y = "Value (local currency unit)", x = "", fill = "Scenario") +
    scale_fill_manual(values = colours_1) +
    scale_y_continuous(
      labels = scales::label_number(accuracy = NULL, scale = 10^-9, suffix = "B"),
      expand = expansion(mult = c(0.1, 0.2))
    ) +
    theme_minimal() +
    theme(
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text.y.left = element_text(angle = 90, size = 14, face = "bold"),
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      text = element_text(size = 14),
      axis.title = element_text(size = 14, face = "bold"),
      legend.position = "bottom",
      panel.grid.major.y = element_blank()
    )
}

# Cost Breakdown ----------------------------------------------------------
ahle_cost_plot <- function(ahle_data){
  
  data_summary <- ahle_data$summary
  
  cost_plot <- data_summary %>% filter(sign==-1) %>%
    ggplot(aes(y = mean, x = scenario, fill = variable)) +
    geom_bar(stat = "identity") +
    labs(y="Value in local currency unit", x="Scenario", fill="Cost category") +
    scale_fill_manual(values=c( "#F17105",
                                "#1A8FE3",
                                "#E6C229",
                                "#D11149",
                                "#6610F2",
                                "#006E90")) +
    scale_y_continuous(labels = label_number(accuracy = NULL, scale = 10^-9, suffix = "B")) +
    theme_minimal() +
    theme(text = element_text(size = 14),
          legend.position = "bottom") 
  
  cost_plot
}

# AHLE: Values vs Costs ---------------------------------------------------
ahle_values_vs_cost <- function(ahle_data){
  
  data_full <- ahle_data$data_full
  
  ahle_summary <- data_full %>%
    filter(scenario %in% c("Current", "Ideal"), variable != "Population", !is.na(sign)) %>%
    group_by(group, scenario, variable, sign) %>%
    summarize(
      mean = mean(value),
      q2_5 = quantile(value, 0.025),
      q97_5 = quantile(value, 0.975),
      .groups = "drop"
    )
  
  ahle_diff <- ahle_summary %>%
    pivot_wider(names_from = scenario, values_from = c(mean, q2_5, q97_5)) %>%
    mutate(
      mean = mean_Ideal - mean_Current,
      q2_5 = q2_5_Ideal - q2_5_Current,
      q97_5 = q97_5_Ideal - q97_5_Current,
      scenario = "Difference"
    ) %>%
    select(group, scenario, variable, sign, mean, q2_5, q97_5)
  
  ahle_diff_data <- bind_rows(ahle_summary, ahle_diff) %>%
    mutate(
      color_label = factor(sign, levels = c(0, 1, -1), labels = c("AHLE", "Revenue", "Expenditure")),
      variable = factor(variable, levels = c(
        "Value of Offtake", "Value of Herd Increase", "Value of Milk",
        "Value of Manure", "Health Cost", "Labour Cost", "Feed Cost", "Gross Margin"
      )),
      variable_group = case_when(
        variable %in% c("Health Cost", "Labour Cost", "Feed Cost") ~ "Costs",
        variable %in% c("Value of Offtake", "Value of Herd Increase", "Value of Milk", "Value of Manure") ~ "Values",
        variable == "Gross Margin" ~ "AHLE"
      ),
      variable_group = factor(variable_group, levels = c("Values", "Costs", "AHLE")),
      variable_clean = str_remove(variable, "^Value of\\s*") %>%
        str_remove(" Cost$")
    )
  
  
  ahle_graph_difference <- ggplot(
    data = ahle_diff_data %>% filter(group == "Overall", scenario == "Difference"),
    aes(y = mean, x = variable_clean, group = color_label, fill = color_label)
  ) +
    geom_bar(stat = "identity", position = position_dodge(width = 1)) +
    geom_errorbar(aes(ymin = q2_5, ymax = q97_5), width = 0.2, position = position_dodge(width = 1)) +
    facet_grid(. ~ variable_group, scales = "free_x", space = "free_x", switch = "x") +
    labs(y = "Difference: Ideal - Current (FCFA)", x = "") +
    scale_fill_manual(values = c(ahle_colour, "#1A8FE3", "#D11149")) +
    scale_y_continuous(labels = label_number(accuracy = NULL, scale = 10^-9, suffix = "B")) +
    theme_bw() +
    theme(
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text.x = element_text(angle = 0, size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      panel.border = element_blank(),
      text = element_text(family = "Arial", size = 12),
      axis.text = element_text(size = 12),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = "none"
    )
  
  ahle_graph_difference
}

# AHLE Summary Table ------------------------------------------------------
ahle_summary_table <- function(ahle_data){
  
  ahle_results <- ahle_data$results
  
  ahle_results_ft <- ahle_results %>% 
    arrange(group) %>%
    mutate(
      mean_value_fmt = format_large_num(mean_value),
      q2_5_fmt = format_large_num(q2_5),
      q97_5_fmt = format_large_num(q97_5)
    ) %>% 
    flextable(col_keys= c("group", "AHLE_type_text", "perc", "mean_value_fmt", "q2_5_fmt", "q97_5_fmt")) %>% 
    theme_booktabs() %>% 
    colformat_double(j = 3, digits = 1, suffix=" %") %>% 
    bg(i = ~AHLE_type == "AHLEtot", bg=ahle_colour) %>% 
    set_header_labels(
      group = "Group",
      AHLE_type_text = "AHLE",
      perc = "Proportion of total AHLE",
      mean_value_fmt = "Mean estimate",
      q2_5_fmt = "Lower bound of the 95% prediction interval",
      q97_5_fmt = "Upper bound of the 95% prediction interval"
    ) %>% 
    merge_v(j = "group") %>% 
    autofit() %>% 
    set_caption(
      caption = "(Scroll for more)"
    )
  
  ahle_results_ft
}

# Donut Plots -------------------------------------------------------------
## Breakdown --------------------------------------------------------------
ahle_donut_breakdown <- function(ahle_data){
  ahle_results <- ahle_data$results
  
  donut_df <- ahle_results %>%
    filter(AHLE_type != "AHLEtot", group != "Overall") %>%
    group_by(group) %>%
    mutate(
      perc = ifelse(is.na(perc), 0, perc),
      frac = pmax(perc, 0) / 100 
    ) %>%
    mutate(
      total_frac = sum(frac, na.rm = TRUE),
      frac = ifelse(total_frac > 0, frac / total_frac, 0)
    ) %>%
    ungroup() %>%
    arrange(group, AHLE_type_text) %>%
    group_by(group) %>%
    mutate(
      ymax = cumsum(frac),
      ymin = lag(ymax, default = 0),
      labelPosition = (ymax + ymin) / 2,
      label = scales::percent(frac, accuracy = 1)
    ) %>%
    ungroup()
  
  ggplot(
    donut_df,
    aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = AHLE_type_text)
  ) +
    geom_rect(color = "white", size = 0.3) +
    ggrepel::geom_label_repel(
      aes(x = 3.5, y = labelPosition, label = label),
      size = 4, show.legend = FALSE,
      segment.color = "grey70",
      box.padding = 0, point.padding = 0, min.segment.length = 0
    ) +
    scale_fill_manual(values = colours_2) +
    coord_polar(theta = "y", clip = "off") +
    xlim(c(2.4, 4)) +
    labs(fill = NULL) +
    theme_void(base_size = 11) +
    theme(
      legend.text = element_text(size = 10),
      strip.text = element_text(face = "bold", size = 12),
      legend.position = "bottom",
      plot.margin = margin(6, 18, 6, 6)
    ) +
    facet_wrap(~group, ncol = 2) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
}

## Overall ---------------------------------------------------------------
ahle_donut_overall <- function(ahle_data){
  
  ahle_results <- ahle_data$results
  
  donut_plot_overall <- ahle_results %>%
    filter(AHLE_type != "AHLEtot",
           group == "Overall") %>%
    group_by(group) %>%
    mutate(
      perc = perc / 100,
      ymax = cumsum(perc),
      ymin = lag(ymax, default = 0),
      labelPosition = (ymax + ymin) / 2,
      label = paste0(round(perc * 100), "%")
    ) %>%
    ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = AHLE_type_text)) +
    geom_rect(color = "white", size = 0.3) +
    geom_label_repel(
      aes(x = 3.5, y = labelPosition, label = label),
      size = 3,
      show.legend = FALSE,
      segment.color = "grey70",
      box.padding = 0,
      point.padding = 0,
      min.segment.length = 0
    ) +
    scale_fill_manual(values = colours_2) +
    coord_polar(theta = "y") +
    xlim(c(2.4, 4)) + 
    labs(fill = NULL) + 
    theme_void(base_size = 11) +
    theme(
      legend.text = element_text(size = 10),
      strip.text = element_text(face = "bold", size = 12),
      legend.position = "bottom"
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
  
  donut_plot_overall
}

# Treemap -----------------------------------------------------------------
ahle_treemap <- function(ahle_data){
  
  ahle_results <- ahle_data$results
  
  attribution_data <- ahle_results %>%
    filter(AHLE_type != "AHLEtot", group != "Overall") %>%
    select(group, AHLE_type_text, mean_value) %>%
    mutate(Level1 = group,
           Level2 = AHLE_type_text)
  
  level2_data <- attribution_data %>%
    group_by(Level2) %>%
    summarize(Value = sum(mean_value), .groups = "drop") %>%
    mutate(Label = Level2,
           Text = Level2,
           Parent = "AHLE")
  
  level1_data <- attribution_data %>%
    mutate(Label = paste0(Level2, "_", Level1),
           Text = Level1,
           Parent = Level2,
           Value = mean_value,
           ColorGroup = Level2) %>%  # <- assign color group to match parent
    select(Label, Parent, Value, Text, ColorGroup)
  
  treemap_data <- bind_rows(
    level2_data %>% select(Label, Parent, Value, Text, Level2),
    level1_data
  )
  
  treemap_data <- bind_rows(
    level2_data %>% mutate(ColorGroup = Level2) %>% select(Label, Parent, Value, Text, ColorGroup),
    level1_data
  ) %>%
    mutate(formatted_values = case_when(
      Value < 1e8 ~ paste0(formatC(Value / 1e6, format = "f", digits = 1), " M"),
      TRUE        ~ paste0(formatC(Value / 1e9, format = "f", digits = 1), " B")
    ))
  
  treemap_colours <- c(
    "Animal health expenditure" = colours_2[1],
    "Direct morbidity losses" = colours_2[2],
    "Direct mortality losses" = colours_2[3],
    "Joint morbidity and mortality losses" = colours_2[4]
  )
  
  treemap_data <- treemap_data %>%
    mutate(color_value = treemap_colours[ColorGroup])
  
  tree_plot_two_levels <- plot_ly(
    type = "treemap",
    branchvalues = "total",
    ids = treemap_data$Label,
    labels = treemap_data$Text,
    parents = treemap_data$Parent,
    values = treemap_data$Value,
    hovertext = treemap_data$formatted_values,
    hovertemplate = "<b>%{label}</b><br>Value: %{hovertext} local currency unit<br><extra></extra>",
    marker = list(colors = treemap_data$color_value)
  ) %>%
    layout(
      margin = list(t = 40, l = 0, r = 0, b = 0),
      title = list(text = "Attribution of AHLE Components", font = list(size = 14)),
      uniformtext = list(minsize = 11, mode = "hide")
    )
  
  tree_plot_two_levels
}
