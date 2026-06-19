library(tidyverse)
library(janitor)

#### Colony numbers and absolute surface areas ####

# Master datafile for all plots 2018-2019
colony_matches <- read_csv("Data/Matches_master.csv") %>% 
  clean_names() 

# Numbers of colonies that fall into different fate categories from 2018-2019
colony_fates <- colony_matches %>%
  filter(class == "Pocillopora" | class == "Acropora",
         #split_fuse == "none",
         action != "born") %>% 
  rename(id_2018 = blob1,
         id_2019 = blob2,
         area_2018 = area1,
         area_2019 = area2) %>% 
  mutate(action = case_when(#action == "born" ~ "Growth"
    action == "dead" ~ "Complete mortality",
    action == "grow" ~ "Growth",
    action == "same" ~ "No change",
    action == "shrink" ~ "Partial mortality")) %>% 
  select(-id_2019) %>% 
  group_by(plot, id_2018, class, action) %>% 
  summarize(area_2018 = mean(area_2018),
            area_2019 = sum(area_2019)) 

# Total live colonies in 2018 (pre-disturbance)
pre_dist.col_num <- colony_fates %>%
  filter(area_2018 > 20) %>% 
  group_by(class) %>% 
  summarize(num_colonies = n())

ggplot(pre_dist.col_num, aes(x = fct_rev(class), y = num_colonies, fill = class)) +
  geom_col(width = 1,
           color = "black",
           position = "stack") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,3010)) +
  scale_fill_manual(name = "",
                    labels = c("", ""),
                    values = c("#604A76", "#D7C8C6")) +
  labs(x = "Coral taxa",
       y = "Number of colonies") +
  theme_classic(base_size = 12)

# Total live surface areas in 2018 (pre-disturbance)
pre_dist.surf_area <- colony_fates %>% 
  filter(area_2018 > 20) %>% 
  group_by(class) %>% 
  summarize(total_surf = sum(area_2018)*0.0001)

ggplot(pre_dist.surf_area, aes(x = fct_rev(class), y = total_surf, fill = class)) +
  geom_col(width = 1,
           color = "black",
           position = "stack") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,71)) +
  scale_fill_manual(name = "",
                    labels = c("", ""),
                    values = c("#604A76", "#D7C8C6")) +
  labs(x = "Coral taxa",
       y = Surface~area~(m^2)) +
  theme_classic(base_size = 12)


## Pocillopora
colony_numbers.poc <- colony_fates %>% 
  filter(class == "Pocillopora",
         area_2018 > 20) %>% 
  group_by(action) %>% 
  summarize(num_colonies = n())

colony_numbers.poc$action <- ordered(colony_numbers.poc$action, levels = c("Complete mortality", "Partial mortality", "No change", "Growth"))

ggplot(colony_numbers.poc, aes(x = action, y = num_colonies)) +
  geom_col(color = "black", 
           fill = "#D7C8C6",
           width = 0.6) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1230),
                     breaks = c(200,400,600,800,1000,1200)) +
  labs(x = "Type of change",
       y = "Number of colonies") +
  theme_classic(base_size = 12)

## Acropora
colony_numbers.acro <- colony_fates %>% 
  filter(class == "Acropora",
         area_2018 > 20) %>% 
  group_by(action) %>% 
  summarize(num_colonies = n())

colony_numbers.acro$action <- ordered(colony_numbers.acro$action, levels = c("Complete mortality", "Partial mortality", "No change", "Growth"))

ggplot(colony_numbers.acro, aes(x = action, y = num_colonies)) +
  geom_col(color = "black", 
           fill = "#604A76",
           width = 0.6) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,180),
                     breaks = c(25,50,75,100,125,150,175)) +
  labs(x = "Type of change",
       y = "Number of colonies") +
  theme_classic(base_size = 12)


#### Gains and losses in surface areas ----

## Pocillopora
gains_losses.poc <- colony_fates %>% 
  filter(class == "Pocillopora",
         area_2018 > 20) %>% 
  mutate(area_change = (area_2019 - area_2018)*0.0001) %>% 
  group_by(action) %>% 
  summarize(SA_change = sum(area_change))

gains_losses.poc$action <- ordered(gains_losses.poc$action, levels = c("Complete mortality", "Partial mortality", "No change", "Growth"))

ggplot(gains_losses.poc, aes(x = action, y = SA_change)) +
  geom_col(color = "black", 
           fill = "#D7C8C6",
           width = 0.6) +
  geom_hline(yintercept = 0, col = "black") +
  scale_y_continuous(limits = c(-35,10)) +
  labs(x = "Type of change",
       y = Surface~area~change~(m^2)) +
  theme_classic(base_size = 12)

## Acropora
gains_losses.acro <- colony_fates %>% 
  filter(class == "Acropora",
         area_2018 > 20) %>% 
  mutate(area_change = (area_2019 - area_2018)*0.0001) %>% 
  group_by(action) %>% 
  summarize(SA_change = sum(area_change)) 

gains_losses.acro$action <- ordered(gains_losses.acro$action, levels = c("Complete mortality", "Partial mortality", "No change", "Growth"))

ggplot(gains_losses.acro, aes(x = action, y = SA_change)) +
  geom_col(color = "black", 
           fill = "#604A76",
           width = 0.6) +
  geom_hline(yintercept = 0, col = "black") +
  labs(x = "Type of change",
       y = Surface~area~change~(m^2)) +
  scale_y_continuous(limits = c(-4,1)) +
  theme_classic(base_size = 12)
