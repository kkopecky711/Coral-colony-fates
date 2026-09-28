#### Colony mortality ~ size ####

library(tidyverse)
library(janitor)
library(DHARMa)
library(glmmTMB)
library(ggeffects)

# Master datafile for all plots 2018-2019
colony_matches <- read_csv("Data/Matches_master.csv") %>% 
  clean_names() 

# Numbers of colonies that fall into different fate categories from 2018-2019
colony_fates <- colony_matches %>%
  filter(class == "Pocillopora" | class == "Acropora",
         action != "born") %>% 
  rename(id_2018 = blob1,
         id_2019 = blob2,
         area_2018 = area1,
         area_2019 = area2) %>% 
  mutate(action = case_when(
    action == "dead" ~ "Complete mortality",
    action == "grow" ~ "Growth",
    action == "same" ~ "Stasis",
    action == "shrink" ~ "Partial mortality")) %>% 
  select(-id_2019) %>% 
  group_by(plot, id_2018, class, action) %>% 
  summarize(area_2018 = mean(area_2018),
            area_2019 = sum(area_2019)) 

#### Colony survival----
colony_survival <- colony_fates %>% 
  filter(class == "Pocillopora" | class == "Acropora",
         action != "born",
         area_2018 > 20) %>% 
  mutate(class = as.factor(class),
         prop_survival = area_2019/area_2018,
         prop_survival = case_when(action == "Complete mortality" ~ 0,
                                   action == "Growth" ~ 1,
                                   action == "Partial mortality" ~ prop_survival,
                                   action == "Stasis" ~ prop_survival),
         prop_survival =  if_else(prop_survival > 1, 1, prop_survival),
         prop_mortality = 1 - prop_survival,
         prop_mortality.beta = (prop_mortality * (nrow(colony_survival) - 1) + 0.5) / nrow(colony_survival))

colony_mortality <- colony_fates %>%
  ungroup() %>%
  filter(
    action %in% c("Partial mortality", "Complete mortality"),
    class %in% c("Pocillopora", "Acropora"),
    area_2018 > 20
  ) %>%
  mutate(
    class = factor(class),
    prop_mortality = case_when(
      action == "Complete mortality" ~ 1,
      action == "Partial mortality" ~
        1 - (area_2019 / area_2018)
    )
  ) %>%
  filter(prop_mortality >= 0)


## GLMM of mortality ~ initial size and taxa
mortality.glmm <- glmmTMB(
  prop_mortality ~ log(area_2018) * class + (1 | plot),
  data = colony_mortality,
  family = ordbeta(link = "logit")
)

summary(mortality.glmm)

# Diagnostics
sim <- simulateResiduals(fittedModel = mortality.glmm, plot = TRUE)

mortality.glmm.poly <- glmmTMB(
  prop_mortality ~ log(area_2018) +
    I(log(area_2018)^2) +
    class +
    log(area_2018):class +
    I(log(area_2018)^2):class +
    (1 | plot),
  data = colony_mortality,
  family = ordbeta(link = "logit")
)

AIC(mortality.glmm, mortality.glmm.poly)

anova(mortality.glmm, mortality.glmm.poly)

sim.mortality.poly <- simulateResiduals(
  mortality.glmm.poly,
  n = 1000
)

plot(sim.mortality.poly)

# mortality.glmm <- glmmTMB(
#   prop_mortality.beta ~ log(area_2018) * class + (1 | plot),
#   data = colony_survival,
#   family = beta_family(link = "logit")
# )
# 
# summary(mortality.glmm)


# # Plot model predictions
# predictions.mortality <- ggpredict(mortality.glmm, terms = ~area_2018*class)
# plot(predictions.mortality)
# 
# # Extract model predictions
# predictions.mortality <- as.data.frame(predictions.mortality)
# 
# # Ensure 'class' or 'group' is a factor with consistent ordering
# colony_survival$class <- factor(colony_survival$class, levels = c("Acropora", "Pocillopora"))
# predictions.mortality$group <- factor(predictions.mortality$group, levels = c("Acropora", "Pocillopora"))
# 
# 
# ## Plot model predictions and individual colony values
# ggplot(data = predictions.mortality, 
#        aes(x = x, y = predicted, color = group)) +
#   geom_point(data = colony_survival,
#              aes(x = area_2018, y = prop_mortality, color = class),
#              alpha = 0.5) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, 
#                   group = group, fill = group),
#               alpha = 0.7, color = NA) +
#   geom_line(aes(y = predicted)) +
#   labs(x = expression("Colony size before heat wave (cm"^2*")"),
#        y = "Colony mortality") +
#   scale_fill_manual(values = c("Acropora" = "#604A76", 
#                                "Pocillopora" = "#D7C8C6"),
#                     labels = c(expression(italic("Acropora")), 
#                                expression(italic("Pocillopora"))),
#                     name = "") +
#   scale_color_manual(values = c("Acropora" = "#604A76", 
#                                 "Pocillopora" = "#D7C8C6"),
#                      labels = c(expression(italic("Acropora")), 
#                                 expression(italic("Pocillopora"))),
#                      name = "") +
#   theme_classic(base_size = 12)



#### Colony growth ----

# Create dataframe for only colonies that remained stable in size (stasis) or grew 
colony_growth <- colony_fates %>% 
  filter(action == "Growth",
         class %in% c("Pocillopora", "Acropora"),
         area_2018 > 20) %>% 
  mutate(class = as.factor(class),
         raw_growth = area_2019 - area_2018,
         prop_growth = (area_2019 - area_2018) / area_2018)

# Check plot proportional growth ~ log(intiial size)
ggplot(colony_growth,
       aes(x = log(area_2018),
           y = prop_growth,
           color = class)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  theme_classic()

# GLMM of log colony growth as a function of initial size and taxon, plot as random effect
growth.glmm <- glmmTMB(
  prop_growth ~ log(area_2018) * class + (1 | plot),
  data = colony_growth,
  family = Gamma(link = "log")
)

summary(growth.glmm)

# Model diagnostics using DHARMa
sim <- simulateResiduals(fittedModel = growth.glmm, plot = TRUE)


# Get observed size ranges for each taxon
poc_range <- range(
  colony_growth$area_2018[colony_growth$class == "Pocillopora"],
  na.rm = TRUE
)

acro_range <- range(
  colony_growth$area_2018[colony_growth$class == "Acropora"],
  na.rm = TRUE
)

# Pocillopora predictions
pred_poc <- ggpredict(
  growth.glmm,
  terms = c(
    paste0("area_2018 [", poc_range[1], ":", poc_range[2], "]"),
    "class [Pocillopora]"
  )
)

# Acropora predictions
pred_acro <- ggpredict(
  growth.glmm,
  terms = c(
    paste0("area_2018 [", acro_range[1], ":", acro_range[2], "]"),
    "class [Acropora]"
  )
)

# Make factor ordering consistent
colony_growth$class <- factor(
  colony_growth$class,
  levels = c("Acropora", "Pocillopora")
)

# Create combined dataframe for predictions
pred_growth <- bind_rows(
  as.data.frame(pred_poc),
  as.data.frame(pred_acro)
)

pred_growth$group <- factor(
  pred_growth$group,
  levels = c("Acropora", "Pocillopora")
)

ggplot() +
  
  geom_point(
    data = colony_growth,
    aes(
      x = area_2018,
      y = prop_growth,
      color = class
    ),
    alpha = 0.5
  ) +
  
  geom_ribbon(
    data = pred_growth,
    aes(
      x = x,
      ymin = conf.low,
      ymax = conf.high,
      fill = group,
      group = group
    ),
    alpha = 0.25,
    color = NA
  ) +
  
  geom_line(
    data = pred_growth,
    aes(
      x = x,
      y = predicted,
      color = group
    ),
    linewidth = 1
  ) +
  
  scale_x_log10() +
  
  labs(
    x = expression("Initial colony size (cm"^2*")"),
    y = "Proportional increase in colony surface area"
  ) +
  
  scale_fill_manual(
    values = c(
      "Acropora" = "#604A76",
      "Pocillopora" = "#D7C8C6"
    ),
    labels = c(
      expression(italic("Acropora")),
      expression(italic("Pocillopora"))
    ),
    name = ""
  ) +
  
  scale_color_manual(
    values = c(
      "Acropora" = "#604A76",
      "Pocillopora" = "#D7C8C6"
    ),
    labels = c(
      expression(italic("Acropora")),
      expression(italic("Pocillopora"))
    ),
    name = ""
  ) +
  
  theme_classic(base_size = 12)
