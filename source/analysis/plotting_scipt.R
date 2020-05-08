library(tidyverse)
library(sf)
rm(list = ls())
setwd("C:/Users/User02/Desktop/Harris/spatial_reg/turbines-in-space")

merged <- read_rds("data/merged/final_merge.rds")

ggplot(merged) +
  geom_sf(aes(fill = t_count)) +
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
