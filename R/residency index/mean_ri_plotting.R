
# load packages ----
# library(brms) # beta mixed mobels
library(dplyr)
library(ggplot2)
library(ggspatial)
library(lemon)
library(forcats)
library(here)
library(readr)
library(rgdal)
library(sf)
library(sp)




# bring in clean downloaded ----

ri <- read_rds(here("fish and tagging data", 
                    "Receiver Downloads", 
                    "Residency Index Data", 
                    "season_ri_lt.rds"))


glimpse(ri)

# bring in shapefile -----

p_map <- st_read(dsn = here::here("Papineau reciever locations and maps", 
                                  "Papineau Lake Shapefile",
                                  "."),
                 layer = "lake")

# create mean dataframe -----
glimpse(ri)
ri_means <- ri %>% 
  group_by(fish_basin, receiver_basin, rec_group,
           rec_mean_lat, rec_mean_long, season) %>% 
  summarise(n = n(),
            n_id = n_distinct(floy_tag),
            mean_ri = mean(ri),
            mean_ri_w = mean(ri_w), 
            sd_ri_w = sd(ri_w), 
            sem_ri_w = (sd(ri_w) / sqrt(n())), 
            mean_roi_w = mean(roi_w), 
            mean_roi = mean(roi),
            sd_roi_f = sd(roi_w), 
            sem_roi_f = (sd(roi_w_f) / sqrt(n()))
  ) %>% 
  ungroup()


ri_means  
min(ri_means$mean_ri)
max(ri_means$mean_ri)



ri_means$fish_basin <- ri_means$fish_basin %>% 
  fct_relevel("East Basin", "West Basin", "North Basin") 

glimpse(ri_means)

View(ri_means)


# plot ----
# number of ri's weighted per number of rec not num of fish ----
ri_weighted <- ggplot() + 
  geom_sf(data = p_map) + 
  facet_grid(season ~ fish_basin) +
  geom_point(data = ri_means,
             aes(x = rec_mean_long,
                 y = rec_mean_lat, size = mean_ri_w, fill = fish_basin), 
             shape = 21, stroke = 1.15
  ) + 
  
  scale_size_continuous(name = "Residency Index",
                        limits = c(0, 1),
                        breaks = seq(0, 1, 0.25),
                        range = c(1, 10)
  ) +
  scale_fill_viridis_d(begin = 0.15, end = 0.75, option = "B", name = "Capture Basin") + 
  coord_sf() +
  # fixed_plot_aspect(ratio = 1.5) + # changes plot size 
  theme_bw(
    base_size = 18
  ) +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.grid = element_blank(), 
        axis.text = element_text(colour = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        # strip.text.y = element_text(angle = 0),
  ) + 
  guides(fill = guide_legend(override.aes = list(size = 8))) + 
  labs(x = "Longitude", 
       y = "Latitude")

ri_weighted


# # ggsave(ri_weighted, filename = here("Fish and tagging data", 
#                                     "Receiver Downloads", 
#                                     "residency index plots", 
#                                     "mean_ri_per_weighted.png"), 
#        # scale = 10,
#        # height = 4.37, width = 8.34 * 3
#        width = 8.5, height = 14, units = c("in")
# )
# number of roi weighted per number of rec not num of fish ----


roi_weighted <- ggplot() + 
  geom_sf(data = p_map) + 
  facet_grid(season ~ fish_basin) +
  geom_point(data = ri_means,
             aes(x = rec_mean_long,
                 y = rec_mean_lat, size = mean_roi_w, fill = fish_basin), 
             shape = 21, stroke = 1.15
  ) + 
  
  scale_size_continuous(name = "Roam Index",
                        limits = c(0, 1),
                        breaks = seq(0, 1, 0.25),
                        range = c(1, 8)
  ) +
  scale_fill_viridis_d(begin = 0.15, end = 0.75, option = "B", 
                       name = "Capture Basin") + 
  coord_sf() +
  theme_bw(
    base_size = 15
  ) +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.grid = element_blank(), 
        axis.text = element_text(colour = "black"), 
        axis.text.x = element_text(angle = 90), 
        strip.text.y = element_text(angle = 0)) + 
  guides(fill = guide_legend(override.aes = list(size = 8))) + 
  labs(x = "Longitude", 
       y = "Latitude")



roi_weighted



# ggsave(ri_weighted, filename = here("Fish and tagging data", 
#                                     "Receiver Downloads", 
#                                     "residency index plots", 
#                                     "mean_ri_per_weighted.png"), 
#        # scale = 10,
#        # height = 4.37, width = 8.34 * 3
#        width = 8.5 , height = 11, units = c("in")
# )


max(ri_means$n_id)

ri_means <- ri_means %>% 
  mutate(n_bins = case_when(n_id %in% seq(1, 4, 1) ~ "1-4", 
                            n_id %in% seq(5, 8, 1) ~ "5-8",
                            n_id %in% seq(9, 12, 1) ~ "9-12",
                            n_id %in% seq(13, 18, 1) ~ "13-18")
  )

glimpse(ri_means)
max(ri_means$mean_ri_w)
summary(ri_means$mean_ri_w)


ri_means$n_bins <- ri_means$n_bins %>% 
  as.factor() %>% 
  fct_relevel("1-4", 
              "5-8",
              "9-12",
              "13-18")

ri_n <- ggplot() + 
  geom_sf(data = p_map) + 
  facet_grid(season ~ fish_basin) +
  geom_point(data = ri_means,
             aes(x = rec_mean_long,
                 y = rec_mean_lat, size = n_bins, fill = mean_ri_w), 
             shape = 21,
             # shape = 16,
             # stroke = 1.15
  ) + 
  
  scale_size_discrete(name = "Number of Individuals",
                      # limits = c(0, 1),
                      # breaks = seq(0, 1, 0.25),
                      range = c(3, 8) 
  ) +
  # scale_fill_viridis_d(begin = 0.15, end = 0.75, option = "B", name = "Capture Basin") + 
  scale_fill_viridis_c("Residency Index",
                       limits = c(0, 1),
                       breaks = seq(0, 1, 0.2), 
                       # option = "B"
  ) +
  scale_x_continuous(breaks = c(-74.80, -74.78, -74.76, -74.74)) +
  
  coord_sf() +
  # fixed_plot_aspect(ratio = 1.5) + # changes plot size 
  theme_bw(
    # base_size = 18
  ) +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.grid = element_blank(), 
        axis.text = element_text(colour = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        # strip.text.y = element_text(angle = 0),
  ) + 
  guides(fill = guide_legend(override.aes = list(size = 8))) + 
  labs(x = "Longitude", 
       y = "Latitude")

ri_n



ggsave(ri_n, filename = here("Fish and tagging data",
                             "Receiver Downloads",
                             "residency index plots",
                             "mean_ri_per_bin_n.png"),
       # scale = 10,
       # height = 4.37, width = 8.34 * 3
       width = 8.5 , height = 11, units = c("in")
)





summary(ri_means$mean_roi_w)


roi_n <- ggplot() + 
  geom_sf(data = p_map) + 
  facet_grid(season ~ fish_basin) +
  geom_point(data = ri_means,
             aes(x = rec_mean_long,
                 y = rec_mean_lat, size = n_bins, fill = mean_roi), 
             shape = 21, stroke = 1.15
  ) + 
  
  scale_size_discrete(name = "Number of Individuals",
                      # limits = c(0, 1),
                      # breaks = seq(0, 1, 0.25),
                      range = c(3, 8) 
  ) +
  # scale_fill_viridis_d(begin = 0.15, end = 0.75, option = "B", name = "Capture Basin") + 
  scale_fill_viridis_c("Roaming Index",
                       limits = c(0, 1),
                       breaks = seq(0, 1, 0.2)
  ) +
  scale_x_continuous(breaks = c(-74.80, -74.78, -74.76, -74.74)) +
  
  coord_sf() +
  # fixed_plot_aspect(ratio = 1.5) + # changes plot size 
  theme_bw(
    # base_size = 18
  ) +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.grid = element_blank(), 
        axis.text = element_text(colour = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        # strip.text.y = element_text(angle = 0),
  ) + 
  guides(fill = guide_legend(override.aes = list(size = 8))) + 
  labs(x = "Longitude", 
       y = "Latitude")

roi_n







glimpse(ri_means)


sum_roi <- ri %>% 
  group_by(fish_basin, receiver_basin, season) %>% 
  summarise(mean_roi = mean(roi), 
            n_id = n_distinct(floy_tag)) %>% 
  ungroup()






sum_roi




sum_roi <- sum_roi %>% 
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

glimpse(sum_roi)
summary(sum_roi)


sum_roi <- sum_roi %>% 
  mutate(n_bins = factor(case_when(n_id %in% seq(1, 4, 1) ~ "1-4", 
                                   n_id %in% seq(5, 8, 1) ~ "5-8",
                                   n_id %in% seq(9, 12, 1) ~ "9-12",
                                   n_id %in% seq(13, 18, 1) ~ "13-18"),
                         levels = c("1-4", 
                                    "5-8", 
                                    "9-12", 
                                    "13-18")
                         ) 
         )




glimpse(sum_roi)
levels(sum_roi$n_bins)

sum_roi <- sum_roi %>% 
  mutate(fish_basin = fct_relevel(.f = .$fish_basin, 
                                  "East Basin", 
                                  "West Basin", 
                                  "North Basin")
  )






roi_n_basin <- ggplot() + 
  geom_sf(data = p_map) + 
  facet_grid(season ~ fish_basin) +
  geom_point(data = sum_roi,
             aes(x = basin_long,
                 y = basin_lat, size = n_bins, fill = mean_roi), 
             shape = 21, stroke = 1.15
  ) + 
  
  scale_size_discrete(name = "Number of Individuals",
                      # limits = c(0, 1),
                      # breaks = seq(0, 1, 0.25),
                      range = c(3, 8) 
  ) +
  # scale_fill_viridis_d(begin = 0.15, end = 0.75, option = "B", name = "Capture Basin") + 
  scale_fill_viridis_c("Roaming Index",
                       limits = c(0, 1),
                       breaks = seq(0, 1, 0.2)
  ) +
  scale_x_continuous(breaks = c(-74.80, -74.78, -74.76, -74.74)) +
  
  coord_sf() +
  # fixed_plot_aspect(ratio = 1.5) + # changes plot size 
  theme_bw(
    # base_size = 18
  ) +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.grid = element_blank(), 
        # axis.text = element_text(colour = "black"), 
        # axis.text.x = element_text(angle = 45, hjust = 1), 
        # strip.text.y = element_text(angle = 0),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
  ) + 
  guides(fill = guide_legend(override.aes = list(size = 8)))
  # labs(x = "Longitude", 
  #      y = "Latitude")


















ggsave(roi_n, filename = here("Fish and tagging data",
                              "Receiver Downloads",
                              "residency index plots",
                              "mean_roi_per_bin_n.png"),
       # scale = 10,
       # height = 4.37, width = 8.34 * 3
       width = 8.5 , height = 11, units = c("in")
)


ggsave(roi_n, filename = here("Fish and tagging data",
                              "Receiver Downloads",
                              "residency index plots",
                              "mean_roi_per_bin_n_wt_basin.png"),
       # scale = 10,
       # height = 4.37, width = 8.34 * 3
       width = 8.5 , height = 11, units = c("in")
)


ggsave(roi_n_basin, filename = here("Fish and tagging data",
                              "Receiver Downloads",
                              "residency index plots",
                              "mean_roi_per_bin_n_basin.png"),
       # scale = 10,
       # height = 4.37, width = 8.34 * 3
       width = 8.5 , height = 11, units = c("in")
)
