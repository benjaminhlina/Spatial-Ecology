

# load packages ----
{
library(broom.mixed)
library(dplyr)
library(fitdistrplus)
library(ggplot2)
library(gganimate)
library(ggh4x)
library(gamm4)
library(gratia)
library(here)
library(itsadug)
library(lubridate)
library(lemon)
library(janitor)
library(lme4)
library(mgcv)
library(multcomp)
library(openxlsx)
library(patchwork)
library(purrr)
library(sf)
library(readr)
library(tibble)
library(tidymv)
library(tidyr)
library(visreg)
  source(here("R", 
              "julian_date_reorder.r"))
}



# bring in depth data 



ful <- read_rds(here("Saved Data", 
                     "lkt_depth.rds"))

glimpse(ful)


p_map <- st_read(dsn = here("Shapefiles",
                            "."),
                 layer = "plake_edit_wo_link")

##### SUBSET OUT ONLY TEMP DATA and determine daily temp FOR 2017 - 2021-------



ful_depth <- ful %>% 
  mutate(hour = hour(time_bins)) %>% 
  group_by(floy_tag, fish_basin, rec_group, 
           hour, date, week, month, season, year,
           sensor_unit) %>% 
  summarise(
    mean_long = mean(long_mean), 
    mean_lat = mean(lat_mean), 
    mean_depth = mean(sensor_value)) %>% 
  ungroup() %>% 
  mutate(date_2 = as.numeric(date), 
         floy_tag = factor(floy_tag)
  ) %>%
  filter(date_2 <= 18559) %>% 
  mutate(fish_basin = factor(stringr::str_replace(as.character(fish_basin), " Basin",
                                                  ""), 
                             levels = c("East", "West", "North")), 
         doy = yday(date), 
         month = factor(month, 
                        levels = c("May", "June", "July", 
                                   "August", "September", "October",
                                   "Novemeber", "December", "January",
                                   "February", "March", "April")
         )
  ) %>% 
  arrange(month) %>%
  mutate(doy_id = days(date)) %>% 
  arrange(date)

glimpse(ful_depth)

unique(ful_depth$year)
unique(ful_depth$mean_lat)


knots <- list(doy = c(0.5, 366.5))
# --------------------------crreat big bammmmmm ---------------------
m <- bam(mean_depth ~ 
           # s(hour, k = 10, by = fish_basin, bs = "cc") + 
           s(doy, k = 15, by = fish_basin, bs = "cc") +
           s(floy_tag, year, bs = c("re", "re"),
             by = fish_basin) +
           s(mean_long, mean_lat, k = 100, bs = "ds", m = c(1, 0.5)) +  
         # ti(doy, year, bs = c("cc", "re"), k = c(12, 4)) +
         # ti(mean_long, mean_lat, hour, d = c(2, 1), bs = c("ds", "cc"),
         #    m = list(c(1, 0.5), NA), k = c(20, 10)) + 
         ti(mean_long, mean_lat, doy, d = c(2, 1), bs = c("ds", "cc"),
            m = list(c(1, 0.5), NA), k = c(25, 15)),
         # ti(mean_long, mean_lat, year, d = c(2, 1), bs = c("ds", "re"),
         #    m = list(c(1, 0.5), NA), k = c(25, 4)),
         
         
         data = ful_depth, 
         method = "fREML", 
         knots = knots, 
         nthreads = c(4, 1), 
         discrete = TRUE
)
beepr::beep()


par(mfrow = c(2, 2))
gam.check(m)
par(mfrow = c(1, 1))

m_simp <- bam(mean_depth ~ 
                s(hour, k = 10, by = fish_basin, bs = "cc") + 
                s(doy, k = 12, by = fish_basin, bs = "cc") +
                s(year, bs = "re", by = fish_basin) +
                s(mean_long, mean_lat, k = 100, bs = "ds", m = c(1, 0.5)) +
                ti(doy, year, bs = c("cc", "tp"), k = c(12, 4)), 
              data = ful_depth, 
              method = "fREML", 
              knots = knots, 
              nthreads = c(4, 1), 
              discrete = TRUE
)
AIC(m, m_simp)
anova(m, m_simp, test = "F")

summary(m)

plot(m, pages = 1, shade = TRUE)

draw(m, scales = "free")

unique(ful_depth$floy_tag)

glimpse(ful_depth)
# 
depth_raster_smooth <- crossing(
  # tibble(date = unique(df$daily)),
   tibble(floy_tag = "a"),
   tibble(fish_basin = unique(ful_depth$fish_basin)),
   tibble(doy = unique(ful_depth$doy)),
  tibble(year = 0),
  # depths can now be any value
  tibble(mean_long = seq(min(ful_depth$mean_long),
                         max(ful_depth$mean_long), length = 100),
         mean_lat = seq(min(ful_depth$mean_lat),
                        max(ful_depth$mean_lat), length = 100))

) %>% 
  expand(floy_tag, fish_basin, doy, year, mean_long, mean_lat) %>% 

  mutate(
    depth = predict.gam(
      m, exclude =  c("s(floy_tag, year)"),
      nthreads = 16, 
      newdata = tibble(
        # date = date,
        doy = doy,
        # day_in_year,
        floy_tag = floy_tag,
        year = year,
        fish_basin = fish_basin,
        mean_long = mean_long,
        mean_lat = mean_lat,
      )
    )
  )
beepr::beep()


write_rds(depth_raster_smooth, 
          here("Saved Data", 
               "predicted_daily_depth_location_GAMM.rds"))
glimpse(depth_raster_smooth)

depth_raster_smooth <- read_rds(here("Saved Data", 
                                     "predicted_daily_depth_location_GAMM.rds"))
# ind <- exclude.too.far(pdata$mean_long, pdata$mean_long, 
#                        lts_1$mean_long, lts_1$mean_lat, dist = 0.1)
# 
# 
# fit[ind] <- NA




predicts_sf <- st_as_sf(depth_raster_smooth, coords = c("mean_long", "mean_lat"), 
                        crs =  st_crs(p_map))




tests <- st_intersection(predicts_sf, p_map)
glimpse(tests)
tests <- tests %>% 
  dplyr::select(floy_tag:depth, geometry)
glimpse(tests)


tests <- tests %>% 
  mutate(
    doy = as.integer(doy), 
    date = as.Date(doy, origin = "2017-12-31"), 
    day_of_year = as.Date(format(date,"%m-%d-%y"))
  )






# ggplot() + 
#   geom_sf(data = p_map) +
#   geom_sf(data = test_2, aes(colour = depth)) + 
#   scale_colour_viridis_c(name = "Depth (m)", 
#                          trans = "reverse") + 
#   theme_bw(base_size = 15) + 
#   facet_wrap(. ~ fish_basin) + 
#   theme(panel.grid = element_blank())


ggplot() + 
  geom_sf(data = p_map) +
  geom_sf(data = tests, aes(colour = depth)) + 
  scale_colour_viridis_c(name = "Depth (m)", 
                         trans = "reverse") + 
  theme_bw(base_size = 15) + 
  facet_wrap(. ~ fish_basin) + 
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5), 
        axis.text = element_text(colour = "black")) + 
  labs(
    title = 'Day of Year: {frame_time}',
       x = "Longitude", y = "Latitude") + 
  transition_time(date) +
  ease_aes() -> p
# gc()
# rm(p)
  
# animate(p, height = 12,
#           width = 12, units = "in", nframes = 365, res = 300)
# 
# anim_save(animation = animate(p, height = 12,
#                               width = 12, units = "in", 
#                               nframes = 365, res = 300, 
#                               renderer = av_renderer("depth_gam_interpolate.mkv")), 
#           path = here("Plots"), filename =  c("depth_gam_interpolate.mkv"))
# 

# animate(p)
beepr::beep()





glimpse(tests)

tests_1 <- tests %>% 
  dplyr::select(floy_tag:depth, date, day_of_year) %>% 
  bind_cols(., st_coordinates(tests[,1])) %>% 
  rename(mean_long = X, 
         mean_lat = Y)


glimpse(test_1)

test_1 <- st_drop_geometry(test_1)



doy_id <- unique(tests$day_of_year)

tests <- tests %>% 
  filter(depth > 0)











# plot each day 
for(i in 1:length(doy_id)) {
  
  tests %>%
    filter(day_of_year %in% doy_id[i]) %>% 
    ggplot() + 
    geom_sf(data = p_map) +
    geom_sf(aes(colour = depth),size = 1.5) + 
    scale_colour_viridis_c(name = "Depth (m)", 
                           trans = "reverse", 
                           breaks = rev(seq(0, 50, 10)),
                           limits = rev(c(0, 50))) + 
    theme_bw(base_size = 15) + 
    facet_wrap(. ~ fish_basin) + 
    theme(panel.grid = element_blank(), 
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5), 
          axis.text = element_text(colour = "black")) + 
    labs(title = doy_id[i], 
      x = "Longitude", y = "Latitude") -> p 
  # p
  ggsave(plot = p, filename = here::here("Plots",
                                         "depth_heat_map_static",
                                         paste("lkt_", doy_id[i], "_depth.png", 
                                               sep = "")), 
         width = 12, height = 12)
  
}

dev.off()
