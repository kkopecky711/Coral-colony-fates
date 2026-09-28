#### Demographic outcomes as a function of colony size and taxon ####

library(tidyverse)
library(janitor)
library(nnet)
library(emmeans)

#### Fate probabilities ----
# Import master colony-matching dataset
colony_matches <- read_csv("Data/Matches_master.csv") %>% 
  clean_names() 

# Create one record per 2018 colony
colony_fates <- colony_matches %>%
  filter(
    class %in% c("Pocillopora", "Acropora"),
    action != "born"
  ) %>% 
  rename(
    id_2018 = blob1,
    id_2019 = blob2,
    area_2018 = area1,
    area_2019 = area2
  ) %>% 
  mutate(
    action = case_when(
      action == "dead"   ~ "Complete mortality",
      action == "grow"   ~ "Growth",
      action == "same"   ~ "Stasis",
      action == "shrink" ~ "Partial mortality"
    )
  ) %>% 
  select(-id_2019) %>% 
  group_by(plot, id_2018, class, action) %>% 
  summarize(
    area_2018 = mean(area_2018),
    area_2019 = sum(area_2019),
    .groups = "drop"
  ) %>%
  filter(area_2018 > 20) %>%
  mutate(
    class = factor(
      class,
      levels = c("Pocillopora", "Acropora")
    ),
    action = factor(
      action,
      levels = c(
        "Growth",
        "Stasis",
        "Partial mortality",
        "Complete mortality"
      )
    )
  )

## Multinomial fate model to test whether the probability of growth, stasis, partial mortality, or complete mortality varies with initial colony size and taxon.

fate.mod <- multinom(
  action ~ log(area_2018) * class,
  data = colony_fates,
  trace = FALSE
)

summary(fate.mod)

# Compare the size-by-taxon interaction model with an additive model to determine whether the relationship between colony size and demographic fate differs between taxa.

fate.mod.add <- multinom(
  action ~ log(area_2018) + class,
  data = colony_fates,
  trace = FALSE
)

# Compare models to see whether a significant interaction is detected and whether including the interaction imporves model fit

anova(fate.mod.add, fate.mod, test = "Chisq") # Significant interaction detected
AIC(fate.mod.add, fate.mod) # Interaction improves model fit --> include interaction in model structure

# Generate a table of values to report in text
pred_summary <- emmeans(
  fate.mod,
  ~ action | class * area_2018,
  at = list(
    area_2018 = c(50, 100, 500, 1000)
  ),
  type = "response"
) %>%
  as.data.frame()

pred_summary

# Generate table of full model output for supplement
# Extract model coefficients and standard errors
mod_summary <- summary(fate.mod)

coef_table <- as.data.frame(mod_summary$coefficients) %>%
  rownames_to_column("Fate") %>%
  pivot_longer(
    cols = -Fate,
    names_to = "Term",
    values_to = "Estimate"
  )

se_table <- as.data.frame(mod_summary$standard.errors) %>%
  rownames_to_column("Fate") %>%
  pivot_longer(
    cols = -Fate,
    names_to = "Term",
    values_to = "SE"
  )

# Combine and calculate Wald statistics and P-values
fate_model_table <- left_join(
  coef_table,
  se_table,
  by = c("Fate", "Term")
) %>%
  mutate(
    z = Estimate / SE,
    p = 2 * pnorm(abs(z), lower.tail = FALSE)
  )

fate_model_table


## Generate predicted probabilities and 95% confidence intervals for each demographic fate across the observed colony-size range of each taxon

# Determine observed size range for each taxon
poc_range <- range(
  colony_fates$area_2018[colony_fates$class == "Pocillopora"],
  na.rm = TRUE
)

acr_range <- range(
  colony_fates$area_2018[colony_fates$class == "Acropora"],
  na.rm = TRUE
)

# Generate log-spaced colony sizes across each observed range
poc_sizes <- exp(seq(
  log(poc_range[1]),
  log(poc_range[2]),
  length.out = 200
))

acr_sizes <- exp(seq(
  log(acr_range[1]),
  log(acr_range[2]),
  length.out = 200
))

# Predicted fate probabilities for Pocillopora
pred_poc <- emmeans(
  fate.mod,
  ~ action | area_2018,
  at = list(
    area_2018 = poc_sizes,
    class = "Pocillopora"
  ),
  type = "response"
) %>%
  as.data.frame() %>%
  mutate(class = "Pocillopora")

# Predicted fate probabilities for Acropora
pred_acr <- emmeans(
  fate.mod,
  ~ action | area_2018,
  at = list(
    area_2018 = acr_sizes,
    class = "Acropora"
  ),
  type = "response"
) %>%
  as.data.frame() %>%
  mutate(class = "Acropora")

# Combine predictions
pred_fates <- bind_rows(
  pred_poc,
  pred_acr
) %>%
  mutate(
    class = factor(
      class,
      levels = c("Pocillopora", "Acropora")
    ),
    action = factor(
      action,
      levels = c(
        "Growth",
        "Stasis",
        "Partial mortality",
        "Complete mortality"
      )
    )
  )

#### Colony fate visualizations (Figure 4) ----

fate_colors <- c(
  "Growth"             = "#83A552",
  "Stasis"             = "#7ACCD7",
  "Partial mortality"  = "#115896",
  "Complete mortality" = "#21282F"
)

# Fig. 4A: Predicted probabilities and 95% confidence intervals for each demographic fate as a function of initial colony size. Rugs show the distribution of observed initial colony sizes.

ggplot(
  pred_fates,
  aes(
    x = area_2018,
    y = prob,
    color = action,
    fill = action
  )
) +
  geom_ribbon(
    aes(
      ymin = lower.CL,
      ymax = upper.CL
    ),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(
    linewidth = 1
  ) +
  geom_rug(
    data = colony_fates,
    aes(x = area_2018),
    inherit.aes = FALSE,
    alpha = 0.15
  ) +
  facet_wrap(~class) +
  scale_x_log10() +
  scale_y_continuous(
    breaks = seq(0, 1, 0.25)
  ) +
  scale_color_manual(values = fate_colors) +
  scale_fill_manual(values = fate_colors) +
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  labs(
    x = expression("Initial colony size (cm"^2*")"),
    y = "Predicted probability",
    color = "Colony fate",
    fill = "Colony fate"
  ) +
  theme_classic(base_size = 12)

# Fig. 4B Scatter plot of change in colony size (log10) for surviving colonies
colony_change <- colony_fates %>%
  filter(action %in% c(
    "Growth",
    "Stasis",
    "Partial mortality"
  )) %>%
  mutate(
    log10_size_change = log10(area_2019 / area_2018))

ggplot(
  colony_change,
  aes(
    x = area_2018,
    y = log10_size_change,
    color = action
  )
) +
  # No change line
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray40"
  ) +
  # Lines for doubling and halving in colony size
  geom_hline(
    yintercept = c(log10(0.5), log10(2)),
    linetype = "dotted",
    color = "gray60"
  ) +
  geom_point(
    alpha = 0.35,
    size = 1.5
  ) +
  facet_wrap(~class) +
  scale_x_log10() +
  scale_color_manual(values = fate_colors) +
  labs(
    x = expression("Initial colony size (cm"^2*")"),
    y = expression(log[10]*"(final size / initial size)"),
    color = "Colony fate"
  ) +
  theme_classic(base_size = 12)

# Fig. 4C: size distribution of colonies that underwent complete mortality 
complete_mortality <- colony_fates %>%
  filter(action == "Complete mortality") %>% 
  mutate(class = factor(class, levels = c("Pocillopora", "Acropora")))

ggplot(
  complete_mortality,
  aes(x = area_2018)
) +
  geom_histogram(
    bins = 20,
    color = "lightgrey",
    fill = "#21282F"
  ) +
  facet_wrap(~class) +
  scale_x_log10() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 105)) +
  labs(
    x = expression("Initial colony size (cm"^2*")"),
    y = "Number of colonies"
  ) +
  theme_classic(base_size = 12) 
