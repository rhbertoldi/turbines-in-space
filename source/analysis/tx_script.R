library(tidyverse)
library(sf)
library(readxl)
library(snakecase)
library(tidycensus)
library(spdep)
library(spatialreg)
library(spatialprobit)
rm(list = ls())
setwd("C:/Users/User02/Desktop/Harris/spatial_reg/turbines-in-space")

turbines <- read_sf(
  "C:/Users/User02/Desktop/Harris/spatial_reg/final_project/turbines/uswtdb_v3_0_20200417.shp"
) %>%
  filter(t_state == "TX")

vars <- c(
  median_income = "B19013_001",
  white = "B02001_002",
  black = "B02001_003",
  amer_ind = "B02001_004",
  asian = "B02001_005",
  na_pi = "B02001_006",
  hisp = "B03001_003"
)

census_shp_tx <- get_acs(
  geography = "county",
  variables = vars,
  state = "48",
  year = 2018,
  geometry = TRUE
) %>%
  set_names(to_snake_case(colnames(.))) %>%
  select(name, variable, estimate) %>%
  spread(variable, estimate) %>%
  separate(name, into = c("t_county", "temp"), sep = ",") %>%
  select(-temp)

turbine_summary_tx <- turbines %>%
  st_set_geometry(NULL) %>%
  filter(p_year != "-9999",
         p_year <= 2018) %>%
  group_by(t_county) %>%
  summarize(t_count = n()) %>%
  na.omit()

elevation <- read_csv("data/texas/tx_elevation.csv") %>%
  set_names(to_snake_case(colnames(.))) %>%
  mutate(name = paste(name, "County")) %>%
  rename(t_county = name)

land_area <- read_csv("data/texas/tx_land_area.csv") %>%
  mutate(county = paste(county, "County")) %>%
  rename(t_county = county)

wind_speed <- read_csv("data/texas/tx_wind_speed.csv") %>%
  set_names(to_snake_case(colnames(.))) %>%
  separate(county_population,
           sep = ",",
           into = c("t_county", "temp")) %>%
  separate(average_wind_speed,
           sep = " ",
           into = c("ave_wind_speed", "temp")) %>%
  select(-temp) %>%
  mutate(
    t_county = paste(t_county, "County"),
    ave_wind_speed = as.numeric(ave_wind_speed)
  ) %>%
  mutate(t_county =
           ifelse(
             t_county == "Mcculloch County",
             "McCulloch County",
             ifelse(
               t_county == "Mcmullen County",
               "McMullen County",
               ifelse(
                 t_county == "Mcmullen County",
                 "McMullen County",
                 ifelse(
                   t_county == "Mclennan County",
                   "McLennan County",
                   ifelse(t_county == "De Witt County", "DeWitt County",
                          t_county)
                 )
               )
             )
           )
         )

temp_merge <- plyr::join_all(
  list(elevation,
       land_area,
       wind_speed,
       turbine_summary_tx),
  by = "t_county",
  type = "left"
)

merged_tx <-
  left_join(census_shp_tx, temp_merge, by = "t_county") %>%
  mutate(t_count = ifelse(is.na(t_count), 0, t_count)) %>%
  st_transform(2163) %>%
  mutate(count_dummy = ifelse(t_count == 0, 0, 1))
write_rds(merged_tx, "data/merged/final_merge_tx.rds")
