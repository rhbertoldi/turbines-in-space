library(tidyverse)
library(sf)
library(readxl)
library(snakecase)
library(tidycensus)
rm(list = ls())
setwd("C:/Users/User02/Desktop/Harris/spatial_reg/turbines-in-space")

turbines <- read_sf(
  "C:/Users/User02/Desktop/Harris/spatial_reg/final_project/turbines/uswtdb_v3_0_20200417.shp")

census_api_key("375b79c384981482079e44ee2ac6f45f8f2df381")
v18 <- load_variables(2018, "acs5", cache = TRUE)

vars <- c(
  median_income = "B19013_001",
  white = "B02001_002",
  black = "B02001_003",
  amer_ind = "B02001_004",
  asian = "B02001_005",
  na_pi = "B02001_006",
  hisp = "B03001_003"
)

census_shp <- get_acs(
  geography = "state",
  variables = vars,
  year = 2018,
  geometry = TRUE) %>%
  set_names(to_snake_case(colnames(.))) %>%
  filter(name %in% state.name) %>%
  select(name, variable, estimate) %>%
  spread(variable, estimate)

states <- census_shp %>%
  select(name)

turbine_summary <- turbines %>%
  st_set_geometry(NULL) %>%
  filter(p_year != "-9999",
         p_year <= 2018) %>%
  group_by(t_state) %>%
  summarize(t_count = n()) %>%
  mutate(name = state.name[match(t_state, state.abb)])

merged_counts <- left_join(census_shp, turbine_summary, by = "name") %>%
  mutate(t_count = ifelse(is.na(t_count), 0, t_count)) %>%
  select(everything(), geometry) %>%
  select(-t_state) %>%
  filter(name != "Alaska",
         name != "Hawaii")

state_areas <- read_csv("data/land/state_areas.csv") %>%
  set_names(to_snake_case(colnames(.))) %>%
  rename(name = state)

wind <- read_csv("data/wind/wtk_site_metadata.csv") %>%
  set_names(to_snake_case(colnames(.))) %>%
  filter(power_curve != "offshore") %>%
  group_by(state) %>%
  summarise(ave_wind_speed = mean(wind_speed)) %>%
  filter(state %in% state.name) %>%
  rename("name" = "state")

temp_merge <- left_join(state_areas, wind,
                 by = "name", type = "left") %>%
  filter(name != "Alaska",
         name != "Hawaii")
final_merge <- left_join(merged_counts, temp_merge, by = "name")
write_rds(final_merge, "data/merged/final_merge.rds")
