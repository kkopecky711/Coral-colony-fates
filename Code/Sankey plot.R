#### Sankey plot to display colony fates over four years for a singel reef plot

library(tidyverse)
library(janitor)
library(ggalluvial)
library(gt)

## Create dataframes of colonies in each year

# 2017
plot18_sankey.2017 <- read_csv("Data/Plot 18/Plot18_even more updated/Updated_2025-02-12/Matches_2025-02-12_2017-2018.csv") %>%
  clean_names() %>% 
  filter(class == "Pocillopora",
    area1 > 20,
    split_fuse == "none") %>% 
  mutate(T1 = "Live") %>% 
  select(genet, T1)

# 2018
plot18_sankey.2018 <- read_csv("Data/Plot 18/Plot18_even more updated/Updated_2025-02-12/Matches_2025-02-12_2017-2018.csv") %>%
  clean_names() %>% 
  filter(#period == "2017-2018",
    class == "Pocillopora",
    action != "born",
    #area1 > 20,
    split_fuse == "none") %>% 
  mutate(action = case_when(action == "dead" ~ "Completely dead",
                            action == "grow" ~ "Live",
                            action == "same" ~ "Live",
                            action == "shrink" ~ "Partially dead"),
         #period = as.factor(period),
         genet = as.character(genet),
         T2 = as.factor(action)) %>% 
  select(genet, T2)

# 2019
plot18_sankey.2019 <- read_csv("Data/Plot 18/Plot18_even more updated/Updated_2025-02-12/Matches_2025-02-12_2017-2019.csv") %>%
  clean_names() %>% 
  filter(#period == "2019-2020",
    class == "Pocillopora",
    action != "born",
    area1 > 20,
    split_fuse == "none") %>% 
  mutate(action = case_when(action == "dead" ~ "Completely dead",
                            action == "grow" ~ "Live",
                            action == "same" ~ "Live",
                            action == "shrink" ~ "Partially dead"),
         #period = as.factor(period),
         genet = as.character(genet),
         T3 = as.factor(action)) %>% 
  select(genet, T3)

# 2020
plot18_sankey.2020 <- read_csv("Data/Plot 18/Plot18_even more updated/Updated_2025-02-12/Matches_2025-02-12_2017-2020.csv") %>%
  clean_names() %>% 
  filter(#period == "2019-2020",
    #class == "Pocillopora",
    action != "born",
    split_fuse == "none") %>% 
  mutate(action = case_when(action == "dead" ~ "Completely dead",
                            action == "grow" ~ "Live",
                            action == "same" ~ "Live",
                            action == "shrink" ~ "Partially dead"),
         #period = as.factor(period),
         genet = as.character(genet),
         T4 = as.factor(action)) %>% 
  select(genet, T4)

## Create merged dataframe
plot_18.all_years <- merge(plot18_sankey.2017, plot18_sankey.2018, by = "genet")
plot_18.all_years <- merge(plot_18.all_years, plot18_sankey.2019, all.x = TRUE)
plot_18.all_years <- merge(plot_18.all_years, plot18_sankey.2020, all.x = TRUE)

plot_18.all_years <- plot_18.all_years %>% 
  distinct(.keep_all = TRUE) %>% 
  replace_na(list(T2 = "Completely dead", T3 = "Completely dead", T4 = "Completely dead"))

# Track unique four-year trajectories
condition_levels <- c(
  "Completely dead",
  "Partially dead",
  "Live"
)

condition_cols <- c(
  "Completely dead" = "#4A5352",
  "Partially dead" = "#ECBD95",
  "Live" = "#D7C8C6"
)

year_steps <- c("T1", "T2", "T3", "T4")

year_labs <- c(
  T1 = "2017",
  T2 = "2018",
  T3 = "2019",
  T4 = "2020"
)

gap <- 25
bar_width <- 0.16
curve_strength <- 0.45

make_ribbon <- function(flow_id, x, next_x,
                        source_flow_ymin, source_flow_ymax,
                        target_flow_ymin, target_flow_ymax,
                        n_points = 60) {
  
  t <- seq(0, 1, length.out = n_points)
  
  x0 <- x + bar_width / 2
  x3 <- next_x - bar_width / 2
  x1 <- x0 + curve_strength * (x3 - x0)
  x2 <- x3 - curve_strength * (x3 - x0)
  
  bez <- function(p0, p1, p2, p3) {
    (1 - t)^3 * p0 +
      3 * (1 - t)^2 * t * p1 +
      3 * (1 - t) * t^2 * p2 +
      t^3 * p3
  }
  
  x_top <- bez(x0, x1, x2, x3)
  y_top <- bez(source_flow_ymax, source_flow_ymax, target_flow_ymax, target_flow_ymax)
  
  x_bottom <- bez(x0, x1, x2, x3)
  y_bottom <- bez(source_flow_ymin, source_flow_ymin, target_flow_ymin, target_flow_ymin)
  
  tibble(
    flow_id = flow_id,
    x = c(x_top, rev(x_bottom)),
    y = c(y_top, rev(y_bottom))
  )
}

paths <- plot_18.all_years %>%
  mutate(across(T1:T4, ~factor(.x, levels = condition_levels))) %>%
  count(T1, T2, T3, T4, name = "n") %>%
  filter(n >= 5) %>%   # <- filter here
  arrange(desc(n)) %>%
  mutate(
    path_id = row_number(),
    path_label = n
  )

path_long <- paths %>%
  pivot_longer(
    cols = T1:T4,
    names_to = "year_step",
    values_to = "condition"
  ) %>%
  mutate(
    year_step = factor(year_step, levels = year_steps),
    x = as.numeric(year_step)
  )

nodes <- path_long %>%
  group_by(year_step, x, condition) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  filter(n > 0) %>%
  group_by(year_step) %>%
  arrange(condition, .by_group = TRUE) %>%
  mutate(
    ymin = cumsum(lag(n, default = 0)) + gap * (row_number() - 1),
    ymax = ymin + n,
    ymid = (ymin + ymax) / 2
  ) %>%
  ungroup()

# Labels for each condition block
node_labels <- nodes %>%
  mutate(
    label = n,
    label_x = x,
    label_y = (ymin + ymax) / 2
  )

# Allocate each four-year path within each node
path_positions <- path_long %>%
  left_join(nodes %>% select(year_step, condition, node_ymin = ymin),
            by = c("year_step", "condition")) %>%
  group_by(year_step, condition) %>%
  arrange(path_id, .by_group = TRUE) %>%
  mutate(
    ymin = node_ymin + cumsum(lag(n, default = 0)),
    ymax = ymin + n,
    ymid = (ymin + ymax) / 2
  ) %>%
  ungroup()

flows <- path_positions %>%
  arrange(path_id, year_step) %>%
  group_by(path_id) %>%
  mutate(
    next_x = lead(x),
    target_ymin = lead(ymin),
    target_ymax = lead(ymax)
  ) %>%
  ungroup() %>%
  filter(!is.na(next_x)) %>%
  mutate(
    source_flow_ymin = ymin,
    source_flow_ymax = ymax,
    target_flow_ymin = target_ymin,
    target_flow_ymax = target_ymax,
    flow_id = paste(path_id, year_step, sep = "_")
  )

flow_polys <- pmap_dfr(
  flows %>%
    select(
      flow_id, x, next_x,
      source_flow_ymin, source_flow_ymax,
      target_flow_ymin, target_flow_ymax
    ),
  make_ribbon
)

ggplot() +
  
  # Flow ribbons
  geom_polygon(
    data = flow_polys,
    aes(
      x = x,
      y = y,
      group = flow_id
    ),
    fill = "grey60",
    color = "white",
    linewidth = 0.4,
    alpha = 0.85
  ) +
  
  # Condition boxes
  geom_rect(
    data = nodes,
    aes(
      xmin = x - bar_width / 1.5,
      xmax = x + bar_width / 1.5,
      ymin = ymin,
      ymax = ymax,
      fill = condition
    ),
    color = "black",
    linewidth = 0.8
  ) +
  
  # Counts for each condition block
  geom_text(
    data = node_labels,
    aes(
      x = label_x,
      y = label_y,
      label = label
    ),
    size = 4,
    fontface = "bold"
  ) +
  
  scale_fill_manual(
    values = condition_cols,
      breaks = c(
        "Live",
        "Partially dead",
        "Completely dead"
      ),
    name = "Colony condition"
    #name = expression(italic("Pocillopora") ~ "condition")
  ) +
  
  scale_x_continuous(
    breaks = 1:4,
    labels = c(
      "2017",
      "2018",
      "2019",
      "2020"
    ),
    position = "top",
    expand = expansion(mult = c(0.05, 0.25))
  ) +
  
  scale_y_continuous(
    breaks = NULL,
    labels = NULL
  ) +
  
  labs(
    x = "Year",
    y = "Number of colonies"
  ) +
  
  coord_cartesian(clip = "off") +
  
  theme_classic(base_size = 16) +
  theme(
  axis.line.x = element_blank(),
  axis.ticks.x = element_blank(),

  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.title.y = element_text(size = 16),

  legend.position = "right"
)

## Table of four-year fates
paths_table <- paths %>%
  arrange(path_id) %>%
  mutate(
    percent = round(100 * n / sum(n), 1)
  ) %>%
  select(-path_id, -path_label) %>% 
  rename(
    `2017` = T1,
    `2018` = T2,
    `2019` = T3,
    `2020` = T4,
    `Colonies` = n,
    `%` = percent
  )

paths_table_gt <- paths_table %>%
  gt() %>%
  fmt_number(
    columns = `%`,
    decimals = 1
  ) %>%
  cols_label(
    `%` = "Percent (%)"
  ) %>%
  tab_header(
    title = md("**Four-year colony fate trajectories**")
  )

paths_table_gt
