#### Total raw cover of live and dead coral in single reef plot from 2017-2020

library(tidyverse)
library(janitor)

# Sum area for all colonies in each year and convert to square meters 
area.2017 <- read_csv("Data/Plot 18/Plot18_even more updated/Raw areas/2017.csv") %>% 
  clean_names() %>% 
  rename(size = tag_lab_surf_area) %>%
  group_by(tag_lab_class_name) %>% 
  summarize(area = sum(size)*0.0001) %>% 
  mutate(year = 2017) 

area.2018 <- read_csv("Data/Plot 18/Plot18_even more updated/Raw areas/2018.csv") %>% 
  clean_names() %>% 
  rename(size = tag_lab_surf_area) %>%
  group_by(tag_lab_class_name) %>% 
  summarize(area = sum(size)*0.0001) %>% 
  mutate(year = 2018)

area.2019 <- read_csv("Data/Plot 18/Plot18_even more updated/Raw areas/2019.csv") %>% 
  clean_names() %>% 
  rename(size = tag_lab_surf_area) %>%
  group_by(tag_lab_class_name) %>% 
  summarize(area = sum(size)*0.0001) %>% 
  mutate(year = 2019)

area.2020 <- read_csv("Data/Plot 18/Plot18_even more updated/Raw areas/2020.csv") %>% 
  clean_names() %>% 
  rename(size = tag_lab_surf_area) %>%
  group_by(tag_lab_class_name) %>% 
  summarize(area = sum(size)*0.0001) %>% 
  mutate(year = 2020) 

# Combine years
live_dead.area <- rbind(area.2017, area.2018, area.2019, area.2020)

# Combine Acropora and Pocillopora into a single category of live coral
live_dead.area <- live_dead.area %>% 
  mutate(coral_condition = case_when(tag_lab_class_name == "Pocillopora_dead" ~ "Dead",
                                     TRUE ~ "Live")) %>% 
  group_by(year, coral_condition) %>% 
  summarize(area = sum(area))

# Plot of raw surface areas over time
ggplot(live_dead.area, aes(x = year, y = area, fill = fct_rev(coral_condition))) +
  geom_col(color = "black") +
  labs(x = "Year",
       y = Surface~area~(m^2)) +
  scale_fill_manual(values = c("white", "#4A5352"),
                    name = "",
                    labels = c(Live~coral~(italic("Acropora + Pocillopora")), "Dead coral")) +
  scale_x_continuous(breaks = c(2017,2018,2019,2020)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 33)) +
  theme_classic(base_size = 14) +
  theme(legend.position = "top") 
