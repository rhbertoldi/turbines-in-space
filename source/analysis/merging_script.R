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
         t_cap != "-9999",
         p_year <= 2018) %>%
  group_by(t_state) %>%
  summarize(t_count = n(),
            t_cap = sum(t_cap)) %>%
  mutate(name = state.name[match(t_state, state.abb)]) %>%
  mutate(t_cap_mwh = t_cap/1000) %>%
  na.omit()

merged_counts <- left_join(census_shp, turbine_summary, by = "name") %>%
  mutate(t_count = ifelse(is.na(t_count), 0, t_count),
         t_cap = ifelse(is.na(t_cap), 0, t_cap),
         t_cap_mwh = ifelse(is.na(t_cap_mwh), 0, t_cap_mwh)) %>%
  select(everything(), geometry) %>%
  select(-t_state)

state_areas <- read_csv("data/land/state_areas.csv") %>%
  set_names(to_snake_case(colnames(.))) %>%
  rename(name = state)

merged <- left_join(merged_counts, state_areas, by = "name")
write_rds(merged, "data/merged/turbine_census.rds")

ggplot(merged) +
  geom_sf(aes(fill = t_count)) +
  coord_sf(xlim = c(-175, -65)) +
  theme(legend.position = "none",
        line = element_blank(),
        rect = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_line(colour = "transparent"))
ggsave("source/build/t_map.png", plot = last_plot())

ggplot(merged, aes(x = median_income, y = t_count)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Median Income",
       y = "Turbine Count") +
  theme_minimal()
ggsave("source/build/inc_t.png", plot = last_plot())

ggplot(merged, aes(x = log(median_income), y = log(t_count))) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Median Income",
       y = "Turbine Count") +
  theme_minimal()
ggsave("source/build/log_inc_t.png", plot = last_plot())

ggplot(merged, aes(x = white, y = t_count)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Population - White",
       y = "Turbine Count") +
  theme_minimal()
ggsave("source/build/white_t.png", plot = last_plot())

merged %>%
  mutate(ratio_t_turbine = t_count / square_miles_land_area) %>%
  ggplot(aes(x = log(median_income), y = ratio_t_turbine)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Median Income",
       y = "Number of Turbines Over State Land Area") +
  theme_minimal()
ggsave("source/build/ratio_inc_t.png", plot = last_plot())

# Hoen, B.D., Diffendorfer, J.E., Rand, J.T., Kramer, L.A., Garrity, C.P., and Hunt, H.E., 2018,
# United States Wind Turbine Database (ver. 3.0, April 2020): U.S. Geological Survey, American Wind Energy Association,
# and Lawrence Berkeley National Laboratory data release, https://doi.org/10.5066/F7TX3DN0.
