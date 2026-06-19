#### Size distributions of Pocillopora by year for single plot

# Create dataframes of colony sizes for each year
sizes.2017 <- read_csv("Data/Plot 18/Plot18_even more updated/Updated_2025-02-12/Matches_2025-02-12_2017-2018.csv") %>% 
  clean_names() %>% 
  filter(class == "Pocillopora",
         action != "born",
         area1 > 20,
         split_fuse == "none") %>% 
  rename(size = area1) %>% 
  mutate(year = 2017) %>% 
  select(genet, year, size, action)

sizes.2018 <- read_csv("Data/Plot 18/Plot18_even more updated/Updated_2025-02-12/Matches_2025-02-12_2017-2018.csv") %>% 
  clean_names() %>% 
  filter(class == "Pocillopora",
         action != "born",
         area2 > 20,
         split_fuse == "none") %>% 
  rename(size = area2) %>% 
  mutate(year = 2018) %>% 
  select(genet, year, size, action)

sizes.2018.check <- sizes.2018 %>% 
  filter(action == "born")

sizes.2019 <- read_csv("Data/Plot 18/Plot18_even more updated/Updated_2025-02-12/Matches_2025-02-12_2017-2019.csv") %>% 
  clean_names() %>% 
  filter(class == "Pocillopora",
         action != "born",
         area2 > 20,
         split_fuse == "none") %>% 
  rename(size = area2) %>% 
  mutate(year = 2019) %>% 
  select(genet, year, size, action)

sizes.2020 <- read_csv("Data/Plot 18/Plot18_even more updated/Updated_2025-02-12/Matches_2025-02-12_2017-2020.csv") %>% 
  clean_names() %>% 
  filter(#period == "2019-2020",
    class == "Pocillopora",
    area2 > 20,
    action != "born",
    split_fuse == "none") %>% 
  rename(size = area2) %>% 
  mutate(year = 2020) %>% 
  select(genet, year, size, action)

# Combine into a single dataframe
sizes.all_years <- rbind(sizes.2017, sizes.2018, sizes.2019, sizes.2020)
sizes.all_years <- sizes.all_years %>% 
  mutate(action = case_when(action == "shrink" ~ "Shrink",
                            TRUE ~ "Grow"))

sizes.all_years <- sizes.all_years %>% 
  mutate(fill_color = if_else(year == 2017, "None", action))

## Histograms of live colony size distributions in each year
ggplot(sizes.all_years, aes(x = size)) +
  geom_histogram(aes(fill = fill_color),
                 color = "black",
                 binwidth = 100) +
  facet_wrap(~year, ncol = 4) +
  labs(x = Colony~size~(cm^2),
       y = "No. live colonies") +
  scale_x_continuous(breaks = c(50, 500, 1000)) +
  scale_y_continuous(limits = c(0,200)) +
  scale_fill_manual(name = "",
                    labels = c("Size increase", "Initial size", "Size decrease"),
                    values = c("#D7C8C6", "white", "#ECBD95")) +
  theme_minimal(base_size = 14) #+
  #theme(legend.position = "none")

## Create dataframes for sizes of dead colonies in each year
dead_sizes.2017 <- read_csv("Data/Plot 18/Plot18_even more updated/Updated_2025-02-12/Matches_2025-02-12_2017-2018.csv") %>% 
  clean_names() %>% 
  select(-area2) %>% 
  filter(class == "Pocillopora_dead", 
         area1 > 20,
         split_fuse == "none") %>% 
  rename(size = area1) %>% 
  mutate(year = 2017) %>% 
  select(year, size)

dead_sizes.2018 <- read_csv("Data/Plot 18/Plot18_even more updated/Updated_2025-02-12/Matches_2025-02-12_2017-2018.csv") %>% 
  clean_names() %>% 
  filter(class == "Pocillopora",
         area1 > 20,
         action == "dead",
         split_fuse == "none") %>% 
  rename(size = area1) %>% 
  mutate(year = 2018) %>% 
  select(year, size)

dead_sizes.2019 <- read_csv("Data/Plot 18/Plot18_even more updated/Updated_2025-02-12/Matches_2025-02-12_2017-2019.csv") %>% 
  clean_names() %>% 
  filter(class == "Pocillopora",
         action == "dead",
         area1 > 20,
         split_fuse == "none") %>% 
  rename(size = area1) %>% 
  mutate(year = 2019) %>% 
  select(year, size)

dead_sizes.2020 <- read_csv("Data/Plot 18/Plot18_even more updated/Updated_2025-02-12/Matches_2025-02-12_2017-2020.csv") %>% 
  clean_names() %>% 
  filter(class == "Pocillopora",
         action == "dead",
         area1 > 20,
         split_fuse == "none") %>% 
  rename(size = area1) %>% 
  mutate(year = 2020) %>% 
  select(year, size)

dead_sizes.all_years <- rbind(dead_sizes.2017, dead_sizes.2018, dead_sizes.2019, dead_sizes.2020)

# Histogram of dead colonies
ggplot(dead_sizes.all_years, aes(x = size)) +
  geom_histogram(color = "black",
                 binwidth = 100) +
  facet_wrap(~year, ncol = 4) +
  labs(x = Colony~size~(cm^2),
       y = "No. dead colonies") +
  scale_x_continuous(breaks = c(0, 500, 1000)) +
  scale_y_continuous(limits = c(0,200)) +
  scale_fill_manual(name = "",
                    labels = "Dead",
                    values = "#4A5352") +
  expand_limits(x = 1200) +
  theme_minimal(base_size = 14)
