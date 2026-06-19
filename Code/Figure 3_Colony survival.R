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
    action == "same" ~ "No change",
    action == "shrink" ~ "Partial mortality")) %>% 
  select(-id_2019) %>% 
  group_by(plot, id_2018, class, action) %>% 
  summarize(area_2018 = mean(area_2018),
            area_2019 = sum(area_2019)) 

# Colony survival 
colony_survival <- colony_fates %>% 
  filter(class == "Pocillopora" | class == "Acropora",
         action != "born",
         area_2018 > 20) %>% 
  mutate(class = as.factor(class),
         prop_survival = area_2019/area_2018,
         prop_survival = case_when(action == "Complete mortality" ~ 0,
                                   action == "Growth" ~ 1,
                                   action == "Partial mortality" ~ prop_survival,
                                   action == "No change" ~ prop_survival),
         prop_survival =  if_else(prop_survival > 1, 1, prop_survival),
         prop_mortality = 1 - prop_survival) %>% 
  mutate(survival = if_else(prop_survival >= 0.5, 1, 0),
         mortality = if_else(prop_survival < 0.5, 1, 0))


## GLMM of mortality ~ initial size and taxa

# Rescale survival data to be between 0 and 1 for beta distribution
colony_survival$prop_mortality <- (colony_survival$prop_mortality * (nrow(colony_survival) - 1) + 0.0001) / nrow(colony_survival)

# Check to ensure rescaling worked
summary(colony_survival$prop_mortality)
any(colony_survival$prop_mortality == 0 | colony_survival$prop_mortality == 1)

mortality.glmm <- glmmTMB(
  prop_mortality ~ area_2018 * class,
  data = colony_survival,
  family = beta_family(link = "logit")
)

summary(mortality.glmm)

# Check whether including zero-inflation increases model fit
mortality.glmm.zi <- glmmTMB(
  prop_mortality ~ area_2018 * class,
  data = colony_survival,
  family = beta_family(link = "logit"),
  ziformula = (~1)
)

AIC(mortality.glmm, mortality.glmm.zi) # Model without zero inflation has better fit

# Diagnostics
sim <- simulateResiduals(fittedModel = mortality.glmm, plot = TRUE)

# Plot model predictions
predictions.mortality <- ggpredict(mortality.glmm, terms = ~area_2018*class)
plot(predictions.mortality)

# Extract model predictions
predictions.mortality <- as.data.frame(predictions.mortality)

# Ensure 'class' or 'group' is a factor with consistent ordering
colony_survival$class <- factor(colony_survival$class, levels = c("Acropora", "Pocillopora"))
predictions.mortality$group <- factor(predictions.mortality$group, levels = c("Acropora", "Pocillopora"))


## Plot model predictions and individual colony values
ggplot(data = predictions.mortality, 
       aes(x = x, y = predicted, color = group)) +
  geom_point(data = colony_survival,
             aes(x = area_2018, y = prop_mortality, color = class),
             alpha = 0.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, 
                  group = group, fill = group),
              alpha = 0.7, color = NA) +
  geom_line(aes(y = predicted)) +
  labs(x = expression("Colony size before heat wave (cm"^2*")"),
       y = "Colony mortality") +
  scale_fill_manual(values = c("Acropora" = "#604A76", 
                               "Pocillopora" = "#D7C8C6"),
                    labels = c(expression(italic("Acropora")), 
                               expression(italic("Pocillopora"))),
                    name = "") +
  scale_color_manual(values = c("Acropora" = "#604A76", 
                                "Pocillopora" = "#D7C8C6"),
                     labels = c(expression(italic("Acropora")), 
                                expression(italic("Pocillopora"))),
                     name = "") +
  theme_classic(base_size = 12)
