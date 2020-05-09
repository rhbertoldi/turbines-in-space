library(tidyverse)
library(sf)
library(spdep)
rm(list = ls())
setwd("C:/Users/User02/Desktop/Harris/spatial_reg/turbines-in-space")

merged <- read_rds("data/merged/final_merge.rds") %>%
  st_transform(2163)

# kNN
coords <- st_centroid(st_geometry(merged))
neighbors_knn <- knearneigh(coords, k=4)
plot(st_geometry(merged), border='grey')
plot(knn2nb(neighbors_knn), coords, add=TRUE)

# Queen
neighbors_queen <- poly2nb(merged)
list_queen <- nb2listw(neighbors_queen)
plot(st_geometry(merged), border='grey')
plot(neighbors_queen, coords, add = TRUE)
hist(card(neighbors_queen),
     col = "grey",
     main = "Connectivity Histogram",
     breaks = seq(-0.5, 9.5, by = 1))

# reg analysis
reg_vars <- merged %>%
  st_set_geometry(NULL) %>%
  select(
    -c(name, t_count, t_cap, t_cap_mwh, net_generation, ave_cap_factor)
    ) %>%
  names() %>%
  paste(collapse = " + ")

reg_ols <- lm(as.formula(paste0("t_count ~ ", reg_vars)), data = merged)
lm.morantest(reg_ols, list_queen)
lm.LMtests(reg_ols, list_queen, test = "all")

summary(reg_ols)
