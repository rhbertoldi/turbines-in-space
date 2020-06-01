library(tidyverse)
library(spdep)
library(spatialreg)
library(spatialprobit)
rm(list = ls())
setwd("C:/Users/User02/Desktop/Harris/spatial_reg/turbines-in-space")

merged_tx <- read_rds("data/merged/final_merge_tx.rds")

ggplot(merged_tx) +
  geom_sf(aes(fill = t_count)) +
  theme(legend.position = "none",
        line = element_blank(),
        rect = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_line(colour = "transparent"))
ggsave("source/build/tx_count.png", plot = last_plot())

reg_vars <- merged_tx %>%
  st_set_geometry(NULL) %>%
  select(
    -c(count_dummy, t_county, t_count, amer_ind,
       asian, black, hisp, na_pi, white)
  ) %>%
  names() %>%
  paste(collapse = " + ")

coords <- st_centroid(st_geometry(merged_tx), of_largest_polygon = TRUE)
neighbors_queen <- poly2nb(merged_tx)
list_queen <- nb2listw(neighbors_queen)
plot(st_geometry(merged_tx), border='grey')
plot(neighbors_queen, coords, add = TRUE)
hist(card(neighbors_queen),
     col = "grey",
     main = "Connectivity Histogram",
     breaks = seq(-0.5, 9.5, by = 1))

neighbors_knn <- knearneigh(coords, k=4)
list_knn <- nb2listw(knn2nb(neighbors_knn))

w <- as(as_dgRMatrix_listw(list_queen), "CsparseMatrix")

reg_glm <- glm(as.formula(paste0("count_dummy ~ ", reg_vars)),
               data = merged_tx,
               family = binomial(link = "probit"))
summary(temp_glm)

lag_probit <- sarprobit(temp_glm, w, showProgress = TRUE, ndraw = 1000)
impacts(lag_probit)
plot(lag_probit)
summary(lag_probit)

error_probit <- semprobit(temp_glm, w, showProgress = TRUE, ndraw = 1000)
summary(error_probit)
