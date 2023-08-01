{
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(ggh4x)
  library(here)
  library(purrr)
  library(readr)
  library(tidyr)
  source(here::here("R", 
                    "Functions", 
                    "leadlag_function.R"))
}

# ----- bring in data -----
lkt <- read_rds(here::here("Saved data", 
                           "kenauk lake trout 2017 - 2020.rds")) %>% 
  select(floy_tag, fish_basin, tl, detection_timestamp_utc, 
         detection_timestamp_est, name, lat, long, sensor_value, 
         sensor_unit, 
         day_night, season, year)

# ---- select only temp and depth ----
dat_td <- lkt %>% 
  filter(sensor_unit %in% c("°C", "m"))

# ----- split temp and depth up to match properly ---- 
dat_temp <- dat_td %>% 
  pivot_wider(names_from = sensor_unit, 
              values_from = sensor_value, 
              values_fn = as.numeric
  ) %>% 
  janitor::clean_names() %>% 
  rename(temp = c, 
         depth = m) %>% 
  mutate(id = row_number(),) %>% 
  select(id, floy_tag:long, depth, temp, day_night:year) %>% 
  setDT() %>% 
  mutate(event = if_else(temp > 15, "start", NA))


# -- filter every time fish goes above 15 temp grab previous and above -----
dat_temps <- dat_temp %>%
  group_by(floy_tag) %>%
  filter(leadlag(temp > 15, bef = 1, aft = 1)) %>%
  ungroup()

# check 
dat_temps %>%
  as_tibble() %>%
  print(n = 46)


# ---- make back into long format ------
dat_temps_long <- dat_temps %>% 
  select(id:long, day_night:event, depth, temp) %>% 
  pivot_longer(col = !id:event, 
    names_to = "sensor", values_to = "value"
  ) %>% 
  filter(!is.na(value)) %>% 
  mutate(
    labels = case_when(
      sensor == "temp" ~ "Temperature (°C)", 
      sensor == "depth" ~ "Depth (m)"
    )
  )


# ---- create reverse scales -----
scales <- list(
  scale_y_reverse(), 
  scale_y_continuous()
  
)


# ------ plot ------
ggplot(dat_temps_long, aes(x = detection_timestamp_est, 
                           y = value)) + 
  geom_point(size = 3, aes(colour = value), alpha = 0.5) +
  facet_wrap(. ~ labels, scales = "free_y", strip.position = "left") + 
  scale_colour_viridis_c(end = 0.8, begin = 0.2, name = "") +
  facetted_pos_scales(y = scales) + 
  theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank(), 
    axis.text = element_text(colour = "black"), 
    strip.text.y.left = element_text(), 
    strip.placement = "outside", 
    strip.background = element_blank()
  ) + 
  labs(x = "Date", 
       y = "") -> p 
p

beepr::beep()
# 
# 

# ggplot(dat_temps_long, aes(x = detection_timestamp_est, 
#                            y = value)) + 
#   geom_point(size = 3, aes(colour = labels), alpha = 0.5) +
#   geom_line(linewidth = 1, alpha = 0.5) +
#   
#   scale_colour_viridis_d(end = 0.65, begin = 0.25, name = "") +
#   theme_classic(base_size = 15) + 
#   labs(x = "Date", 
#        y = "Depth or Temperature")
  
  ggsave(here("Plots", 
                    "warm water excursions", 
              "depth_and_temp_scatter_plot.png"), plot = p,
         width = 11, height = 7)


