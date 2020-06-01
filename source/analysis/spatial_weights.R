library(tidyverse)
library(sf)
library(spdep)
library(spatialreg)
rm(list = ls())
setwd("C:/Users/User02/Desktop/Harris/spatial_reg/turbines-in-space")

merged <- read_rds("data/merged/final_merge.rds") %>%
  st_transform(2163) %>%
  mutate(median_income2 = median_income*median_income)

# kNN
coords <- st_centroid(st_geometry(merged), of_largest_polygon = TRUE)
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
    -c(name, t_count, amer_ind, asian, black, hisp, na_pi, white)
    ) %>%
  names() %>%
  paste(collapse = " + ")

reg_ols <- lm(as.formula(paste0("t_count ~ ", reg_vars)), data = merged)
summary(reg_ols)
lm.morantest(reg_ols, list_queen)
lm.LMtests(reg_ols, list_queen, test = "all")

lm.morantest(reg_ols, nb2listw(knn2nb(neighbors_knn)))
lm.LMtests(reg_ols, nb2listw(knn2nb(neighbors_knn)), test = "all")

shapiro.test(merged$t_count)
# normality
bptest(reg_ols)
# fail to reject the null of homoskedasticity
vif(reg_ols)
# how much the other regressors can explain the rest of the regressors
# variable is correlated with the others?


