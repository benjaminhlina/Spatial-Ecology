
# load packages ----

library(dplyr)
library(forcats)
library(ggplot2)
library(here)
library(lemon)
library(lubridate)
library(tidyr)
library(readr)
library(sf)
library(stringr)

# bring in clean downloaded ----

ri <- read_rds(here("Saved Data", 
                    "daily_ri_roi_lkt.rds"))

ri <- ri %>% 
  mutate(fish_basin = factor(str_remove(fish_basin, " Basin"), 
                      levels = c("East", "West", "North")), 
         season = fct_relevel(season, "Spring", "Summer", "Fall", 
                              "Winter"), 
         receiver_basin = factor(str_remove(receiver_basin, " Basin"), 
                                 levels = c("East", "West", "North")), 
  )

# ---- Bring in shapefile ------
p_map <- st_read(dsn = here::here("Shapefiles",
                                  "."),
                 layer = "plake_edit_wo_link")


glimpse(ri)
ri_sum_rb <- ri %>% 
  group_by(fish_basin, receiver_basin,  season) %>% 
  summarise(
    mean_ri = mean(ri_w), 
    mean_long = mean(rec_mean_long),
    mean_lat = mean(rec_mean_lat), 
  ) %>% 
  ungroup() %>% 
  st_as_sf(., coords = c("mean_long", "mean_lat"), 
           crs = st_crs(p_map))

ri_sum_rg <- ri %>% 
  group_by(fish_basin, rec_group, receiver_basin,  season) %>% 
  summarise(
    mean_ri = mean(ri_w), 
    mean_long = mean(rec_mean_long),
    mean_lat = mean(rec_mean_lat), 
  ) %>% 
  ungroup() %>% 
  st_as_sf(., coords = c("mean_long", "mean_lat"), 
           crs = st_crs(p_map))

ri_sum_rb
ri_sum_rg


p <- ggplot() + 
  geom_sf(data = p_map, fill = NA) + 
  geom_sf(data = ri_sum_rb, aes(size = mean_ri, 
                                fill = receiver_basin),
          shape = 21) +
  scale_fill_viridis_d(
    begin = 0.35, end = 0.85,
    option = "G", name = "Receiver Basin") +
  facet_grid(fish_basin ~ season) + 
  scale_size(name = "Mean Weighted\nResidency Index") +
  theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank(), 
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 90), 
    legend.text.align = 0.5
  ) + 
  labs(x = "Longitude", 
       y = "Latitude")
p
ggsave(here("plots", 
            "residency index",
            "map_bubble_residency_season_basin.png"), 
       height = 11, width = 8.5, plot = p)


p1 <- ggplot() + 
  geom_sf(data = p_map, fill = NA) + 
  geom_sf(data = ri_sum_rg, aes(size = mean_ri)) + 
  facet_grid(fish_basin ~ season) + 
  scale_size(name = "Mean Weighted\nResidency Index") +
  theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank(), 
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 90), 
    legend.text.align = 0.5
  ) + 
  labs(x = "Longitude", 
       y = "Latitude")
p1

ggsave(here("plots", 
            "residency index",
            "map_bubble_residency_season_basin_rec_group.png"), 
       height = 11, width = 8.5, plot = p1)

p2 <- ggplot() + 
  geom_sf(data = p_map, fill = NA) + 
  geom_sf(data = ri_sum_rb, aes(size = mean_ri, 
                                fill = receiver_basin),
          shape = 21) +
  scale_fill_viridis_d(
    begin = 0.35, end = 0.85,
    option = "G", name = "Receiver Basin") +
  facet_grid(fish_basin ~ season) + 
  scale_size(name = "Mean Weighted\nResidency Index") +
  theme_bw(base_size = 15) + 
  guides(fill = guide_legend(override.aes = list(size = 5))) + 
  theme(
    panel.grid = element_blank(), 
    strip.background = element_blank(),
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title = element_blank()) + 
  labs(x = "Longitude", 
       y = "Latitude")
# p2
ggsave(here("plots", 
            "residency index",
            "map_bubble_residency_season_basin_presentation.png"), 
       height = 11, width = 8.5, plot = p2)
