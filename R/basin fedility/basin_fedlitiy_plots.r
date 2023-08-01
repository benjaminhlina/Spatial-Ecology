# load packages ----
library(dplyr)
library(car)
library(fitdistrplus)
library(forcats)
library(ggplot2)
library(lemon)
library(here)
library(lemon)
library(lubridate)
library(readr)
library(sf)
library(sp)




# bring in clean downloaded ----

ri <- read_rds(here("fish and tagging data", 
                    "Receiver Downloads", 
                    "Residency Index Data", 
                    "season_ri_lt.rds"))


p_map <- st_read(dsn = here::here("Papineau reciever locations and maps", 
                                  "Papineau Lake Shapefile",
                                  "."),
                 layer = "lake")
glimpse(ri)


rec_basins <- ri %>% 
  group_by(fish_basin, receiver_basin) %>% 
  summarise(mean_ri = mean(ri),
            mean_ri_w = mean(ri_w), 
            mean_ri_w_f = mean(ri_w_f), 
            mean_roi_w = mean(roi_w),
            mean_roi_w_f = mean(roi_w_f), 
            mean_w = n(), 
            fish_n = n_distinct(floy_tag)) %>% 
  ungroup()

rec_basins

rec_basins_season <- ri %>% 
  group_by(fish_basin, receiver_basin, season) %>% 
  summarise(mean_ri = mean(ri),
            mean_ri_w = mean(ri_w), 
            mean_ri_w_f = mean(ri_w_f), 
            mean_roi_w = mean(roi_w),
            mean_roi_w_f = mean(roi_w_f), 
            mean_w = n(), 
            fish_n = n_distinct(floy_tag)) %>% 
  ungroup()

rec_basins_season






ts <- ri %>% 
  group_by(fish_basin) %>% 
  summarise(fish_tot = n_distinct(floy_tag))

ts
rec_basins <- rec_basins %>% 
  left_join(ts, by = "fish_basin")


rec_basins_season <- rec_basins_season %>% 
  left_join(ts, by = "fish_basin")


rec_basins <- rec_basins %>% 
  mutate(basin_long = case_when(receiver_basin %in% "North Basin" ~ -74.775472143829, 
                                receiver_basin %in% "West Basin" ~ -74.7824596373354, 
                                receiver_basin %in% "East Basin" ~ -74.7620209408326, 
                                # TRUE ~ TRUE
  ), 
  basin_lat = case_when(receiver_basin %in% "North Basin" ~ 45.8574007988883, 
                        receiver_basin %in% "West Basin" ~ 45.8173827009556, 
                        receiver_basin %in% "East Basin" ~ 45.81102621018126, 
                        # TRUE ~ TRUE
  )
  ) 

rec_basins




rec_basins_season <- rec_basins_season %>% 
  mutate(basin_long = case_when(receiver_basin %in% "North Basin" ~ -74.775472143829, 
                                receiver_basin %in% "West Basin" ~ -74.7824596373354, 
                                receiver_basin %in% "East Basin" ~ -74.7620209408326, 
                                # TRUE ~ TRUE
  ), 
  basin_lat = case_when(receiver_basin %in% "North Basin" ~ 45.8574007988883, 
                        receiver_basin %in% "West Basin" ~ 45.8173827009556, 
                        receiver_basin %in% "East Basin" ~ 45.81102621018126, 
                        # TRUE ~ TRUE
  )
  ) 

rec_basins <- rec_basins %>% 
  mutate(fish_prop = (fish_n / fish_tot) * 100)



rec_basins_season <- rec_basins_season %>% 
  mutate(fish_prop = (fish_n / fish_tot) * 100)

glimpse(rec_basins)




bf <- rec_basins %>% 
  dplyr::select(fish_basin, receiver_basin, fish_n, fish_tot)



bf


rec_basins


ggplot() + 
  # geom_sf(data = p_map) +
  geom_point(data = rec_basins,
             aes(x = basin_long,
                 y = basin_lat, size = fish_n))



id <- unique(rec_basins$fish_basin)

unique(rec_basins$fish_basin)

rec_basins$fish_basin <- rec_basins$fish_basin %>% 
  fct_relevel("East Basin", "West Basin", "North Basin")




# plot per tagging basin site fedility ------

p <- ggplot() + 
  geom_sf(data = p_map) +
  facet_wrap(. ~ fish_basin, ncol = 3) +
  geom_point(data = rec_basins,
             aes(x = basin_long,
                 y = basin_lat, size = fish_prop, fill = fish_basin), 
             shape = 21, stroke = 1.15
  ) + 
  
  scale_size_continuous(name = "Proportion of \n Individuals (%)",
                        limits = c(0, 100),
                        breaks = seq(0, 100, 25),
                        range = c(1, 8)
                        ) +
  scale_fill_viridis_d(begin = 0.15, end = 0.75, option = "B", name = "Capture Basin") + 
  coord_sf() +
  theme_bw(
    base_size = 15
           ) +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.grid = element_blank(), 
        axis.text = element_text(colour = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  guides(fill = guide_legend(override.aes = list(size = 8))) +
  labs(x = "Longitude", 
       y = "Latitude")

# p



# rec_basins_north <- rec_basins_season %>% 
#   filter(fish_basin == "North Basin")


# p1 <- ggplot() + 
#   geom_sf(data = p_map) +
#   facet_wrap(. ~ season, ncol = 4) +
#   geom_point(data = rec_basins_north,
#              aes(x = basin_long,
#                  y = basin_lat, size = fish_prop, fill = fish_basin), 
#              shape = 21, stroke = 1.15
#   ) + 
#   
#   scale_size_continuous(name = "Proportion of \n Individuals (%)",
#                         limits = c(0, 100),
#                         breaks = seq(0, 100, 25),
#                         range = c(1, )
#   ) +
#   scale_fill_viridis_d(begin = 0.15, end = 0.75, option = "B", name = "Capture Basin") + 
#   coord_sf() +
#   theme_bw(
#     base_size = 15
#   ) +
#   theme(plot.title = element_text(hjust = 0.5), 
#         panel.grid = element_blank(), 
#         axis.text = element_text(colour = "black"), 
#         axis.text.x = element_text(angle = 45)) + 
#   guides(fill = guide_legend(override.aes = list(size = 8))) +
#   labs(x = "Longitude", 
#        y = "Latitude")
# 
# p1





















ggsave(p, filename = here("Fish and tagging data", 
                          "Receiver Downloads", 
                          "residency index plots", 
                          "basin_fedility_bubble.png"), 
       # height = 4.37, width = 8.34 * 3
       width = 11, height = 8.5, units = c("in")
       )
