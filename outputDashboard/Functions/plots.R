
# Formatting --------------------------------------------------------------
format_y_labels <- function(x) {
  if (max(x, na.rm = TRUE) >= 1e9) {
    return(label_number(scale = 1e-9, suffix = " bln"))
  } else if (max(x, na.rm = TRUE) >= 1e6) {
    return(label_number(scale = 1e-6, suffix = " mil"))
  } else if (max(x, na.rm = TRUE) >= 1e3) {
    return(label_number(scale = 1e-3, suffix = "k"))
  } else {
    return(label_number())
  }
}


format_x_labels <- function(x) {
  
  category.map <- c(
    "JF" = "Juvenile Females",
    "JM" = "Juvenile Males",
    "SubAF" = "Sub-Adult Females",
    "SubAM" = "Sub-Adult Males",
    "AF" = "Adult Females",
    "AM" = "Adult Males",
    "Ox" = "Oxen"
  )
  
  sapply(x, function(val) {
    matched_keys <- names(category.map)[sapply(names(category.map), function(k) grepl(k, val))]
    if (length(matched_keys) > 0) {
      best_match <- matched_keys[which.max(nchar(matched_keys))]
      category.map[[best_match]]
    } else {
      val
    }
  })
}

# Theming -----------------------------------------------------------------
theme <- theme_minimal() + 
  theme(
    text = element_text(family = "Arial", size = 12),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12)
  )

colours <- c(
  "#F17105",
  "#1A8FE3",
  "#E6C229",
  "#D11149",
  "#6610F2",
  "#006E90",
  "#F18F01",
  "#A155B9",
  "#009B72",
  "#FF6F61",
  "#3D348B",
  "#808080"
)

# Plots -------------------------------------------------------------------

populationBar <- function(){
  plot.df <- data %>%
    filter(grepl("Num_", X) & !grepl("_offtake", X) & !grepl("_N", X)) %>% 
    mutate(X = format_x_labels(X))
  
  ggplot(plot.df, aes(x = X, y = Mean, fill = "Consant")) +
    geom_bar(stat = "identity")+
    scale_fill_manual(values = colours)+
    scale_y_continuous(labels = format_y_labels(plot.df$Mean)) +
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    labs(title = NULL)+
    xlab("Sub-group")+
    ylab("Head")+
    theme +
    theme(legend.position = "none")
}


populationBarSex <- function(){
  plot.df <- data %>%
    filter(grepl("Num_", X) & !grepl("_offtake", X) & !grepl("_N", X)) %>% 
    mutate(Sex = case_when(
      grepl("JF|SubAF|AF", X) ~ "Female",
      grepl("JM|SubAM|AM|Ox", X) ~ "Male",
      TRUE ~ "Other"
    ),
    X = format_x_labels(X))
  
  ggplot(data = plot.df, aes(x = X, y = Mean, fill = Sex)) +
    geom_bar(stat = "identity", position = "dodge") + 
    scale_y_continuous(labels = format_y_labels(plot.df$Mean)) +
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    facet_grid(~ Sex, scales = "free_x", space = "free_x") +
    scale_fill_manual(values = c("Male" = colours[1], "Female" = colours[2]))+
    labs(title = NULL) +
    xlab("Sub-group") +
    ylab("Head") +
    theme +
    theme(legend.position = "none")
}


populationStackedCountSex <- function(){
  plot.df <- data %>%
    filter(grepl("Num_", X) & !grepl("_offtake", X) & !grepl("_N", X)) %>% 
    mutate(Sex = case_when(
      grepl("JF|SubAF|AF", X) ~ "Female",
      grepl("JM|SubAM|AM|Ox", X) ~ "Male",
      TRUE ~ "Other"
    ),
    X = format_x_labels(X))
  
  plot.df$AgeGroup <- with(plot.df, 
                           ifelse(grepl("Juvenile", X), "Juvenile",
                                  ifelse(grepl("Sub-Adult", X), "Sub-Adult",
                                         ifelse(grepl("Oxen", X), "Oxen",
                                                ifelse(grepl("Adult", X), "Adult", NA)))))
  plot.df$AgeGroup <- factor(plot.df$AgeGroup, levels = c("Juvenile", "Sub-Adult", "Adult", "Oxen"))
  
  ggplot(plot.df, aes(x = Sex, y = Mean, fill = AgeGroup)) +
    geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
    scale_y_continuous(labels = format_y_labels(plot.df$Mean)) +
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    scale_fill_manual(values = colours)+
    labs(title = NULL, fill = "Sub-Group") +
    xlab(NULL) +
    ylab("Head") +
    theme +
    theme(legend.position = "bottom")+
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
}


populationStackedPercentSex <- function(){
  plot.df <- data %>%
    filter(grepl("Num_", X) & !grepl("_offtake", X) & !grepl("_N", X)) %>% 
    mutate(Sex = case_when(
      grepl("JF|SubAF|AF", X) ~ "Female",
      grepl("JM|SubAM|AM|Ox", X) ~ "Male",
      TRUE ~ "Other"
    ),
    X = format_x_labels(X))
  
  plot.df$AgeGroup <- with(plot.df, 
                           ifelse(grepl("Juvenile", X), "Juvenile",
                                  ifelse(grepl("Sub-Adult", X), "Sub-Adult",
                                         ifelse(grepl("Oxen", X), "Oxen",
                                                ifelse(grepl("Adult", X), "Adult", NA)))))
  plot.df$AgeGroup <- factor(plot.df$AgeGroup, levels = c("Juvenile", "Sub-Adult", "Adult", "Oxen"))
  
  ggplot(plot.df, aes(x = Sex, y = Mean, fill = AgeGroup)) +
    geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) +
    scale_y_continuous(labels = percent_format()) +
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    scale_fill_manual(values = colours)+
    labs(title = NULL, fill = "Sub-Group") +
    xlab(NULL) +
    ylab("Head") +
    theme +
    theme(legend.position = "bottom")+
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
}


populationStackedCountAge <- function(){
  plot.df <- data %>%
    filter(grepl("Num_", X) & !grepl("_offtake", X) & !grepl("_N", X)) %>% 
    mutate(Sex = case_when(
      grepl("JF|SubAF|AF", X) ~ "Female",
      grepl("JM|SubAM|AM|Ox", X) ~ "Male",
      TRUE ~ "Other"
    ),
    X = format_x_labels(X))
  
  plot.df$AgeGroup <- with(plot.df, 
                           ifelse(grepl("Juvenile", X), "Juvenile",
                                  ifelse(grepl("Sub-Adult", X), "Sub-Adult",
                                         ifelse(grepl("Oxen", X), "Oxen",
                                                ifelse(grepl("Adult", X), "Adult", NA)))))
  plot.df$AgeGroup <- factor(plot.df$AgeGroup, levels = c("Juvenile", "Sub-Adult", "Adult", "Oxen"))
  
  ggplot(plot.df, aes(x = AgeGroup, y = Mean, fill = Sex)) +
    geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
    scale_y_continuous(labels = format_y_labels(plot.df$Mean)) +
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    scale_fill_manual(values = c("Male" = colours[1], "Female" = colours[2]))+
    labs(title = NULL) +
    xlab(NULL) +
    ylab("Head") +
    theme +
    theme(legend.position = "bottom")+
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
}


populationStackedPercentAge <- function(){
  plot.df <- data %>%
    filter(grepl("Num_", X) & !grepl("_offtake", X) & !grepl("_N", X)) %>% 
    mutate(Sex = case_when(
      grepl("JF|SubAF|AF", X) ~ "Female",
      grepl("JM|SubAM|AM|Ox", X) ~ "Male",
      TRUE ~ "Other"
    ),
    X = format_x_labels(X))
  
  plot.df$AgeGroup <- with(plot.df, 
                           ifelse(grepl("Juvenile", X), "Juvenile",
                                  ifelse(grepl("Sub-Adult", X), "Sub-Adult",
                                         ifelse(grepl("Oxen", X), "Oxen",
                                                ifelse(grepl("Adult", X), "Adult", NA)))))
  plot.df$AgeGroup <- factor(plot.df$AgeGroup, levels = c("Juvenile", "Sub-Adult", "Adult", "Oxen"))
  
  ggplot(plot.df, aes(x = AgeGroup, y = Mean, fill = Sex)) +
    geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) +
    scale_y_continuous(labels = percent_format()) +
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    scale_fill_manual(values = c("Male" = colours[1], "Female" = colours[2]))+
    labs(title = NULL) +
    xlab(NULL) +
    ylab("Head") +
    theme +
    theme(legend.position = "bottom")+
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
}


populationPieSex <- function(){
  plot.df <- data %>%
    filter(grepl("Num_", X) & !grepl("_offtake", X) & !grepl("_N", X)) %>% 
    mutate(Sex = case_when(
      grepl("JF|SubAF|AF", X) ~ "Female",
      grepl("JM|SubAM|AM|Ox", X) ~ "Male",
      TRUE ~ "Other"
    ),
    X = format_x_labels(X))
  
  plot.df$AgeGroup <- with(plot.df, 
                           ifelse(grepl("Juvenile", X), "Juvenile",
                                  ifelse(grepl("Sub-Adult", X), "Sub-Adult",
                                         ifelse(grepl("Oxen", X), "Oxen",
                                                ifelse(grepl("Adult", X), "Adult", NA)))))
  plot.df$AgeGroup <- factor(plot.df$AgeGroup, levels = c("Juvenile", "Sub-Adult", "Adult", "Oxen"))
  
  agg_data <- plot.df %>%
    group_by(Sex) %>%
    summarise(total = sum(Mean)) %>% 
    mutate(perc = total / sum(total) * 100,
           label = paste0(round(perc, 1), "%"))
  
  ggplot(agg_data, aes(x = "", y = total, fill = Sex)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    geom_text(aes(label = label), 
              position = position_stack(vjust = 0.5), 
              size = 6,
              color = "white") +
    scale_fill_manual(values = colours) +
    labs(title = NULL, fill = "Sex") +
    xlab(NULL) +
    ylab(NULL) +
    theme +
    theme(legend.position = "bottom",
          panel.grid = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.x=element_blank()
    ) +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
}


populationPieAge <- function(){
  plot.df <- data %>%
    filter(grepl("Num_", X) & !grepl("_offtake", X) & !grepl("_N", X)) %>% 
    mutate(Sex = case_when(
      grepl("JF|SubAF|AF", X) ~ "Female",
      grepl("JM|SubAM|AM|Ox", X) ~ "Male",
      TRUE ~ "Other"
    ),
    X = format_x_labels(X))
  
  plot.df$AgeGroup <- with(plot.df, 
                           ifelse(grepl("Juvenile", X), "Juvenile",
                                  ifelse(grepl("Sub-Adult", X), "Sub-Adult",
                                         ifelse(grepl("Oxen", X), "Oxen",
                                                ifelse(grepl("Adult", X), "Adult", NA)))))
  plot.df$AgeGroup <- factor(plot.df$AgeGroup, levels = c("Juvenile", "Sub-Adult", "Adult", "Oxen"))
  
  agg_data <- plot.df %>%
    group_by(AgeGroup) %>%
    summarise(total = sum(Mean)) %>% 
    mutate(perc = total / sum(total) * 100,
           label = paste0(round(perc, 1), "%"))
  
  ggplot(agg_data, aes(x = "", y = total, fill = AgeGroup)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    geom_text(aes(label = label), 
              position = position_stack(vjust = 0.5), 
              size = 6,
              color = "white") +
    scale_fill_manual(values = colours) +
    labs(title = NULL, fill = "Sub-Group") +
    xlab(NULL) +
    ylab(NULL) +
    theme +
    theme(legend.position = "bottom",
          panel.grid = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.x=element_blank()
    ) +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
}


mortalityBar <- function(){
  plot.df <- data %>%
    filter(grepl("Total_mortality_", X)) %>% 
    mutate(X = format_x_labels(X))
  
  ggplot(data = plot.df, aes(x = X, y = Mean)) +
    geom_bar(stat = "identity", fill = colours[1]) + 
    scale_y_continuous(labels = format_y_labels(plot.df$Mean)) +
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    labs(title = NULL)+
    xlab("Sub-group")+
    ylab("Deaths")+
    theme
}


mortalityBarSex <- function(){
  plot.df <- data %>%
    filter(grepl("Total_mortality_", X)) %>% 
    mutate(Sex = case_when(
      grepl("JF|SubAF|AF", X) ~ "Female",
      grepl("JM|SubAM|AM|Ox", X) ~ "Male",
      TRUE ~ "Other"
    ),
    X = format_x_labels(X))
  
  ggplot(data = plot.df, aes(x = X, y = Mean, fill = Sex)) +
    geom_bar(stat = "identity", position = "dodge") + 
    scale_y_continuous(labels = format_y_labels(plot.df$Mean)) +
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    facet_grid(~ Sex, scales = "free_x", space = "free_x") +
    scale_fill_manual(values = c("Male" = colours[1], "Female" = colours[2]))+
    labs(title = NULL) +
    xlab("Sub-group") +
    ylab("Deaths") +
    theme +
    theme(legend.position = "none")
}


mortalityStackedCountSex <- function(){
  plot.df <- data %>%
    filter(grepl("Total_mortality_", X)) %>% 
    mutate(Sex = case_when(
      grepl("JF|SubAF|AF", X) ~ "Female",
      grepl("JM|SubAM|AM|Ox", X) ~ "Male",
      TRUE ~ "Other"
    ),
    X = format_x_labels(X))
  
  plot.df$AgeGroup <- with(plot.df, 
                           ifelse(grepl("Juvenile", X), "Juvenile",
                                  ifelse(grepl("Sub-Adult", X), "Sub-Adult",
                                         ifelse(grepl("Oxen", X), "Oxen",
                                                ifelse(grepl("Adult", X), "Adult", NA)))))
  plot.df$AgeGroup <- factor(plot.df$AgeGroup, levels = c("Juvenile", "Sub-Adult", "Adult", "Oxen"))
  
  ggplot(plot.df, aes(x = Sex, y = Mean, fill = AgeGroup)) +
    geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
    scale_y_continuous(labels = format_y_labels(plot.df$Mean)) +
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    scale_fill_manual(values = colours)+
    labs(title = NULL, fill = "Sub-Group") +
    xlab(NULL) +
    ylab("Deaths") +
    theme +
    theme(legend.position = "bottom")+
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
}


mortalityStackedPercentSex <- function(){
  plot.df <- data %>%
    filter(grepl("Total_mortality_", X)) %>% 
    mutate(Sex = case_when(
      grepl("JF|SubAF|AF", X) ~ "Female",
      grepl("JM|SubAM|AM|Ox", X) ~ "Male",
      TRUE ~ "Other"
    ),
    X = format_x_labels(X))
  
  plot.df$AgeGroup <- with(plot.df, 
                           ifelse(grepl("Juvenile", X), "Juvenile",
                                  ifelse(grepl("Sub-Adult", X), "Sub-Adult",
                                         ifelse(grepl("Oxen", X), "Oxen",
                                                ifelse(grepl("Adult", X), "Adult", NA)))))
  plot.df$AgeGroup <- factor(plot.df$AgeGroup, levels = c("Juvenile", "Sub-Adult", "Adult", "Oxen"))
  
  ggplot(plot.df, aes(x = Sex, y = Mean, fill = AgeGroup)) +
    geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) +
    scale_y_continuous(labels = percent_format()) +
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    scale_fill_manual(values = colours)+
    labs(title = NULL, fill = "Sub-Group") +
    xlab(NULL) +
    ylab("Deaths") +
    theme +
    theme(legend.position = "bottom")+
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
}


mortalityStackedCountAge <- function(){
  plot.df <- data %>%
    filter(grepl("Total_mortality_", X)) %>% 
    mutate(Sex = case_when(
      grepl("JF|SubAF|AF", X) ~ "Female",
      grepl("JM|SubAM|AM|Ox", X) ~ "Male",
      TRUE ~ "Other"
    ),
    X = format_x_labels(X))
  
  plot.df$AgeGroup <- with(plot.df, 
                           ifelse(grepl("Juvenile", X), "Juvenile",
                                  ifelse(grepl("Sub-Adult", X), "Sub-Adult",
                                         ifelse(grepl("Oxen", X), "Oxen",
                                                ifelse(grepl("Adult", X), "Adult", NA)))))
  plot.df$AgeGroup <- factor(plot.df$AgeGroup, levels = c("Juvenile", "Sub-Adult", "Adult", "Oxen"))
  
  ggplot(plot.df, aes(x = AgeGroup, y = Mean, fill = Sex)) +
    geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
    scale_y_continuous(labels = format_y_labels(plot.df$Mean)) +
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    scale_fill_manual(values = c("Male" = colours[1], "Female" = colours[2]))+
    labs(title = NULL) +
    xlab(NULL) +
    ylab("Deaths") +
    theme +
    theme(legend.position = "bottom")+
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
}


mortalityStackedPercentAge <- function(){
  plot.df <- data %>%
    filter(grepl("Total_mortality_", X)) %>% 
    mutate(Sex = case_when(
      grepl("JF|SubAF|AF", X) ~ "Female",
      grepl("JM|SubAM|AM|Ox", X) ~ "Male",
      TRUE ~ "Other"
    ),
    X = format_x_labels(X))
  
  plot.df$AgeGroup <- with(plot.df, 
                           ifelse(grepl("Juvenile", X), "Juvenile",
                                  ifelse(grepl("Sub-Adult", X), "Sub-Adult",
                                         ifelse(grepl("Oxen", X), "Oxen",
                                                ifelse(grepl("Adult", X), "Adult", NA)))))
  plot.df$AgeGroup <- factor(plot.df$AgeGroup, levels = c("Juvenile", "Sub-Adult", "Adult", "Oxen"))
  
   ggplot(plot.df, aes(x = AgeGroup, y = Mean, fill = Sex)) +
    geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) +
    scale_y_continuous(labels = percent_format()) +
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    scale_fill_manual(values = c("Male" = colours[1], "Female" = colours[2]))+
    labs(title = NULL) +
    xlab(NULL) +
    ylab("Deaths") +
    theme +
    theme(legend.position = "bottom")+
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
}


mortalityPieSex <- function(){
  plot.df <- data %>%
    filter(grepl("Total_mortality_", X)) %>% 
    mutate(Sex = case_when(
      grepl("JF|SubAF|AF", X) ~ "Female",
      grepl("JM|SubAM|AM|Ox", X) ~ "Male",
      TRUE ~ "Other"
    ),
    X = format_x_labels(X))
  
  plot.df$AgeGroup <- with(plot.df, 
                           ifelse(grepl("Juvenile", X), "Juvenile",
                                  ifelse(grepl("Sub-Adult", X), "Sub-Adult",
                                         ifelse(grepl("Oxen", X), "Oxen",
                                                ifelse(grepl("Adult", X), "Adult", NA)))))
  plot.df$AgeGroup <- factor(plot.df$AgeGroup, levels = c("Juvenile", "Sub-Adult", "Adult", "Oxen"))
  
  agg_data <- plot.df %>%
    group_by(Sex) %>%
    summarise(total = sum(Mean))%>% 
    mutate(perc = total / sum(total) * 100,
           label = paste0(round(perc, 1), "%"))
  
  ggplot(agg_data, aes(x = "", y = total, fill = Sex)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    geom_text(aes(label = label), 
              position = position_stack(vjust = 0.5), 
              size = 6,
              color = "white") +
    scale_fill_manual(values = colours) +
    labs(title = NULL, fill = "Sex") +
    xlab(NULL) +
    ylab(NULL) +
    theme +
    theme(legend.position = "bottom",
          panel.grid = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.x=element_blank()
    ) +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
}


mortalityPieAge <- function(){
  plot.df <- data %>%
    filter(grepl("Total_mortality_", X)) %>% 
    mutate(Sex = case_when(
      grepl("JF|SubAF|AF", X) ~ "Female",
      grepl("JM|SubAM|AM|Ox", X) ~ "Male",
      TRUE ~ "Other"
    ),
    X = format_x_labels(X))
  
  plot.df$AgeGroup <- with(plot.df, 
                           ifelse(grepl("Juvenile", X), "Juvenile",
                                  ifelse(grepl("Sub-Adult", X), "Sub-Adult",
                                         ifelse(grepl("Oxen", X), "Oxen",
                                                ifelse(grepl("Adult", X), "Adult", NA)))))
  plot.df$AgeGroup <- factor(plot.df$AgeGroup, levels = c("Juvenile", "Sub-Adult", "Adult", "Oxen"))
  
  agg_data <- plot.df %>%
    group_by(AgeGroup) %>%
    summarise(total = sum(Mean)) %>% 
    mutate(perc = total / sum(total) * 100,
           label = paste0(round(perc, 1), "%"))
  
  ggplot(agg_data, aes(x = "", y = total, fill = AgeGroup)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    geom_text(aes(label = label), 
              position = position_stack(vjust = 0.5), 
              size = 6,
              color = "white") +
    scale_fill_manual(values = colours) +
    labs(title = NULL, fill = "Sub-Group") +
    xlab(NULL) +
    ylab(NULL) +
    theme +
    theme(legend.position = "bottom",
          panel.grid = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.x=element_blank()
    ) +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
}


liveweightBarCount <- function(){
  plot.df <- data %>%
    filter(grepl("Quantity_liveweight_kg_", X)) %>% 
    mutate(X = format_x_labels(X))
  
  ggplot(data = plot.df, aes(x = X, y = Mean)) +
    geom_bar(stat = "identity", fill = colours[1]) + 
    scale_y_continuous(labels = format_y_labels(plot.df$Mean)) +
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    labs(title = NULL)+
    xlab("Sub-group")+
    ylab("Liveweight")+
    theme
}


populationGrowthBarCount <- function(){
  plot.df <- data %>%
    filter(grepl("Pop_growth_", X)) %>%
    mutate(X = format_x_labels(X))
  
  ggplot(data = plot.df, aes(x = X, y = Mean, fill = Mean < 0)) +
    geom_bar(stat = "identity") + 
    scale_fill_manual(values = c("FALSE" = colours[1], "TRUE" = colours[2])) +
    scale_y_continuous(labels = format_y_labels(plot.df$Mean)) +
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    labs(title = NULL) +
    xlab("Sub-group") +
    ylab("Population Growth") +
    theme +
    theme(legend.position = "none")
}


populationGrowthBarPercent <- function(){
  plot.df <- data %>%
    filter((grepl("Pop_growth_", X) | grepl("Num_", X)) &
             !grepl("_offtake", X) & 
             !grepl("_N", X)) %>%
    mutate(Group = gsub("^(Pop_growth_|Num_)", "", X))
  
  growth.df <- plot.df %>%
    filter(grepl("Pop_growth_", X)) %>%
    select(Group, growth_mean = Mean)
  
  pop.df <- plot.df %>%
    filter(grepl("Num_", X)) %>%
    select(Group, pop_mean = Mean)
  
  plot.df <- left_join(growth.df, pop.df, by = "Group") %>%
    mutate(pop_growth_pct = (growth_mean / pop_mean) * 100) %>% 
    mutate(X = format_x_labels(Group))
  
  ggplot(data = plot.df, aes(x = X, y = pop_growth_pct, fill = pop_growth_pct < 0)) +
    geom_bar(stat = "identity") + 
    scale_fill_manual(values = c("FALSE" = colours[1], "TRUE" = colours[2])) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    labs(title = NULL) +
    xlab("Sub-group") +
    ylab("Population Growth (% of population)") +
    theme + 
    theme(legend.position = "none")
}


productionManureCount <- function(){
  plot.df <- data %>%
    filter(grepl("Quantity_manure_", X)) %>% 
    mutate(X = format_x_labels(X))
  
  ggplot(data = plot.df, aes(x = X, y = Mean)) +
    geom_bar(stat = "identity", fill = colours[1]) + 
    scale_y_continuous(labels = format_y_labels(plot.df$Mean)) +
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    labs(title = NULL)+
    xlab("Sub-group")+
    ylab("Manure Production (kg)")+
    theme
}


productionHideCount <- function(){
  plot.df <- data %>%
    filter(grepl("Quantity_hides_", X)) %>% 
    mutate(X = format_x_labels(X))
  
  ggplot(data = plot.df, aes(x = X, y = Mean)) +
    geom_bar(stat = "identity", fill = colours[1]) + 
    scale_y_continuous(labels = format_y_labels(plot.df$Mean)) +
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    labs(title = NULL)+
    xlab("Sub-group")+
    ylab("Hide Production (Hide Count)")+
    theme
}


productionMilkCount <- function(){
  plot.df <- data %>%
    filter(grepl("Quantity_milk", X)) %>% 
    mutate(X = format_x_labels(X))
  
  ggplot(data = plot.df, aes(x = X, y = Mean)) +
    geom_bar(stat = "identity", fill = colours[1]) + 
    scale_y_continuous(labels = format_y_labels(plot.df$Mean)) +
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    labs(title = NULL)+
    xlab("Sub-group")+
    ylab("Milk (Litres)")+
    theme
}


valueManureCount <- function(){
  plot.df <- data %>%
    filter(grepl("Value_manure_", X)) %>% 
    mutate(X = format_x_labels(X))
  
  ggplot(data = plot.df, aes(x = X, y = Mean)) +
    geom_bar(stat = "identity", fill = colours[1]) + 
    scale_y_continuous(labels = label_dollar()) +  
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    labs(title = "Manure Value")+
    xlab("Sub-group")+
    ylab("Value (USD)")+
    theme
}


valueHideCount <- function(){
  plot.df <- data %>%
    filter(grepl("Value_hides_", X)) %>% 
    mutate(X = format_x_labels(X))
  
  ggplot(data = plot.df, aes(x = X, y = Mean)) +
    geom_bar(stat = "identity", fill = colours[1]) + 
    scale_y_continuous(labels = label_dollar()) +  
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    labs(title = "Hide Value")+
    xlab("Sub-group")+
    ylab("Value (USD)")+
    theme
}


valueOfftakeCount <- function(){
  plot.df <- data %>%
    filter(grepl("Value_offtake_", X)) %>% 
    mutate(X = format_x_labels(X))
  
  ggplot(data = plot.df, aes(x = X, y = Mean)) +
    geom_bar(stat = "identity", fill = colours[1]) + 
    scale_y_continuous(labels = label_dollar()) +  
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    labs(title = "Offtake Value")+
    xlab("Sub-group")+
    ylab("Value (USD)")+
    theme
}


costFeedCount <- function(){
  plot.df <- data %>%
    filter(grepl("Feed_cost_", X)) %>% 
    mutate(X = format_x_labels(X))
  
  ggplot(data = plot.df, aes(x = X, y = Mean)) +
    geom_bar(stat = "identity", fill = colours[1]) + 
    scale_y_continuous(labels = label_dollar()) +  
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    labs(title = NULL)+
    xlab("Sub-group")+
    ylab("Cost (USD)")+
    theme
}


costLabourCount <- function(){
  plot.df <- data %>%
    filter(grepl("Labour_cost_", X)) %>% 
    mutate(X = format_x_labels(X))
  
  ggplot(data = plot.df, aes(x = X, y = Mean)) +
    geom_bar(stat = "identity", fill = colours[1]) + 
    scale_y_continuous(labels = label_dollar()) +  
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    labs(title = NULL)+
    xlab("Sub-group")+
    ylab("Cost (USD)")+
    theme
}


costHealthCount <- function(){
  plot.df <- data %>%
    filter(grepl("Health_cost_", X)) %>% 
    mutate(X = format_x_labels(X))
  
  ggplot(data = plot.df, aes(x = X, y = Mean)) +
    geom_bar(stat = "identity", fill = colours[1]) + 
    scale_y_continuous(labels = label_dollar()) +  
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    labs(title = NULL)+
    xlab("Sub-group")+
    ylab("Cost (USD)")+
    theme
}


costInfrastructureCount <- function(){
  plot.df <- data %>%
    filter(grepl("Infrastructure_cost_", X)) %>% 
    mutate(X = format_x_labels(X))
  
  ggplot(data = plot.df, aes(x = X, y = Mean)) +
    geom_bar(stat = "identity", fill = colours[1]) + 
    scale_y_continuous(labels = label_dollar()) +  
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    labs(title = NULL)+
    xlab("Sub-group")+
    ylab("Cost (USD)")+
    theme
}


costCapitalCount <- function(){
  plot.df <- data %>%
    filter(grepl("Capital_cost_", X)) %>% 
    mutate(X = format_x_labels(X))
  
  ggplot(data = plot.df, aes(x = X, y = Mean)) +
    geom_bar(stat = "identity", fill = colours[1]) + 
    scale_y_continuous(labels = label_dollar()) +  
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    labs(title = NULL)+
    xlab("Sub-group")+
    ylab("Cost (USD)")+
    theme
}


costTotalCountAge <- function(){
  plot.df <- data %>%
    filter(grepl("Total_expenditure_", X)) %>% 
    mutate(X = format_x_labels(X))
  
  ggplot(data = plot.df, aes(x = X, y = Mean)) +
    geom_bar(stat = "identity", fill = colours[1]) + 
    scale_y_continuous(labels = label_dollar()) +  
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    labs(title = NULL)+
    xlab("Sub-group")+
    ylab("Cost (USD)")+
    theme
}

costTotalCountItem <- function(){
  plot.df <- data %>%
    filter(grepl("_cost$", X) & !grepl("^cost_", X)) %>% 
    mutate(X = format_x_labels(X))
  
  ggplot(data = plot.df, aes(x = X, y = Mean)) +
    geom_bar(stat = "identity", fill = colours[1]) + 
    scale_y_continuous(labels = label_dollar()) +  
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    labs(title = NULL)+
    xlab("Sub-group")+
    ylab("Cost (USD)")+
    theme
}


costTotalStackedAge <- function(){
  plot.df <- data %>%
    filter(grepl("_cost_", X)) %>% 
    mutate(
      CostType = gsub("_.*", "", X),
      AgeGroup = gsub(".*_cost_", "", X),
      AgeGroup = ifelse(grepl("_Ox$", X), "Oxen", AgeGroup),
      AgeGroup = format_x_labels(AgeGroup)
    ) %>%
    select(CostType, AgeGroup, Mean)
  
  ggplot(data = plot.df, aes(x = AgeGroup, y = Mean, fill = CostType)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = colours)+
    scale_y_continuous(labels = label_dollar()) +
    scale_x_discrete(labels = format_x_labels(plot.df$AgeGroup)) +
    labs(title = NULL) +
    xlab("Sub-group") +
    ylab("Cost (USD)") +
    theme 
}


costTotalStackedItem <- function(){
  plot.df <- data %>%
    filter(grepl("_cost_", X)) %>% 
    mutate(
      CostType = gsub("_.*", "", X),
      AgeGroup = gsub(".*_cost_", "", X),
      AgeGroup = ifelse(grepl("_Ox$", X), "Oxen", AgeGroup),
      AgeGroup = format_x_labels(AgeGroup)
    ) %>%
    select(CostType, AgeGroup, Mean)
  
  ggplot(data = plot.df, aes(x = CostType, y = Mean, fill = AgeGroup)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = colours)+
    scale_y_continuous(labels = label_dollar()) +
    labs(title = NULL) +
    xlab("Sub-group") +
    ylab("Cost (USD)") +
    theme 
}


grossMargin <- function(){
  plot.df <- data %>%
    filter(grepl("Gross_margin_", X)) %>% 
    mutate(X = format_x_labels(X))
  
  ggplot(data = plot.df, aes(x = X, y = Mean, fill = Mean < 0)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("FALSE" = colours[1], "TRUE" = colours[2])) +
    scale_y_continuous(labels = label_dollar()) +  
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    labs(title = "Total Gross Margin")+
    xlab("Sub-group")+
    ylab("Value (USD)")+
    theme + 
    theme(legend.position = "none")
}


netValue <- function(){
  plot.df <- data %>%
    filter(grepl("Production_value_herd_offtake_hide_manure_", X)) %>% 
    mutate(X = format_x_labels(X))
  
  ggplot(data = plot.df, aes(x = X, y = Mean, fill = Mean < 0)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("FALSE" = colours[1], "TRUE" = colours[2])) +
    scale_y_continuous(labels = label_dollar()) +  
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    labs(title = "Total Net Value")+
    xlab("Sub-group")+
    ylab("Value (USD)")+
    theme + 
    theme(legend.position = "none")
}


valueIncrease <- function(){
  plot.df <- data %>%
    filter(grepl("Total_value_increase_", X)) %>% 
    mutate(X = format_x_labels(X))
  
  ggplot(data = plot.df, aes(x = X, y = Mean, fill = Mean < 0)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("FALSE" = colours[1], "TRUE" = colours[2])) +
    scale_y_continuous(labels = label_dollar()) +  
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    labs(title = "Total Value Increase")+
    xlab("Sub-group")+
    ylab("Value (USD)")+
    theme + 
    theme(legend.position = "none")
}


valueHerdIncrease <- function(){
  plot.df <- data %>%
    filter(grepl("Value_herd_increase_", X)) %>% 
    mutate(X = format_x_labels(X))
  
  ggplot(data = plot.df, aes(x = X, y = Mean, fill = Mean < 0)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("FALSE" = colours[1], "TRUE" = colours[2])) +
    scale_y_continuous(labels = label_dollar()) +  
    scale_x_discrete(labels = format_x_labels(plot.df$X)) +
    labs(title = "Herd Increase Value")+
    xlab("Sub-group")+
    ylab("Value (USD)")+
    theme + 
    theme(legend.position = "none")
}


costValueWaterfall <- function(){
  waterfall.vars <- c(
    "Value_offtake",
    "Value_manure",
    "Value_hides",
    "Value_milk",
    "Value_herd_increase",
    "Feed_cost",
    "Labour_cost",
    "Health_cost",
    "Capital_cost",
    "Infrastructure_cost"
  )
  
  plot.df <- data %>%
    filter(X %in% waterfall.vars) %>%
    select(X, Mean) %>%
    mutate(
      Label = gsub("_", " ", X),
      Category = case_when(
        grepl("cost", X) ~ "Cost",
        TRUE ~ "Value"
      ),
      Value = ifelse(Category == "Cost", -Mean, Mean)
    )%>%
    mutate(Label = factor(Label, levels = Label)) %>%
    mutate(
      cumulative = cumsum(Value) - Value,
      end = cumulative + Value,
      type = ifelse(Value >= 0, "Increase", "Decrease")
    )
  
  ggplot(plot.df) +
    geom_rect(aes(xmin = as.numeric(Label) - 0.4,
                  xmax = as.numeric(Label) + 0.4,
                  ymin = cumulative,
                  ymax = end,
                  fill = type)) +
    scale_x_continuous(breaks = seq_along(plot.df$Label),
                       labels = plot.df$Label) +
    scale_y_continuous(labels = label_dollar()) +
    scale_fill_manual(values = c("Increase" = colours[1], "Decrease" = colours[2])) +
    labs(title = NULL,
         x = NULL,
         y = "USD") +
    theme + 
    theme(legend.position = "none")
}