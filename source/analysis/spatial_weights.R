library(tidyverse)
library(sf)
library(spdep)
rm(list = ls())
setwd("C:/Users/User02/Desktop/Harris/spatial_reg/turbines-in-space")

merged <- read_rds("data/merged/final_merge.rds") %>%
  st_transform(2163)

# nc_sp <- as(merged, 'Spatial')
# neighbors <- poly2nb(nc_sp)
# neighbors_sf <- as(nb2lines(neighbors, coords = coordinates(nc_sp)), 'sf')
# neighbors_sf <- st_set_crs(neighbors_sf, st_crs(merged))
#
# ggplot(merged) +
#   geom_sf(fill = 'salmon', color = 'white') +
#   geom_sf(data = neighbors_sf) +
#   theme(line = element_blank(),
#         rect = element_blank(),
#         axis.text = element_blank(),
#         axis.title = element_blank(),
#         panel.grid.major = element_line(colour = "transparent"))

# kNN
coords <- st_centroid(st_geometry(merged))
neighbors_knn <- knearneigh(coords, k=5)
plot(st_geometry(merged), border='grey')
plot(knn2nb(neighbors_knn), coords, add=TRUE)

# Queen
neighbors_queen <- poly2nb(merged)
plot(st_geometry(merged), border='grey')
plot(neighbors_queen, coords, add = TRUE)
hist(card(neighbors_queen),
     col = "grey",
     main = "Connectivity Histogram",
     breaks = seq(-0.5, 9.5, by = 1))


