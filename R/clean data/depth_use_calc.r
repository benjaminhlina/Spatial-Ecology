# load packages ----

library(dplyr)
library(ggplot2)
library(ggh4x)
library(gamm4)
library(here)
library(lubridate)
library(janitor)
library(readr)
library(tibble)
library(tidymv)
library(tidyr)

# bring in clean downloaded ----

lt <- read_rds(here::here("Saved data", 
                          "kenauk lake trout 2017 - 2020.rds"))


glimpse(lt)

# bring in metadata for tagged fish -----
fish_tag_data <- read_csv(here::here("Data", 
                                     "all fish tagged kenauk.csv")) %>%
  clean_names()

glimpse(fish_tag_data)

fish_tag_data <- fish_tag_data %>% 
  mutate(tag_date = dmy(tag_date), 
         year = year(tag_date))

#  grab just lake trout to add weights -----

fish_acel <- fish_tag_data %>%
  filter(species %in% "LT" 
         # & vemco_type %in% "Acc" 
  ) %>% 
  rename(floy_tag = gray_floy_tag_number) %>% 
  dplyr::select(floy_tag, weight, tl, fl, girth,
                vemco_type, basin)

# select just weights and floy tag 
fish_acel_w <- fish_acel %>% 
  dplyr::select(floy_tag, weight) 

glimpse(fish_acel_w)

# join weights to lt 
lt <- lt %>% 
  left_join(fish_acel_w, by = "floy_tag")

# weight correction -----
lt <- lt %>% 
  mutate(weight = if_else(is.na(weight), true = 
                            round((((girth * girth) * tl) / 30000), 
                                  digits = 0),
                          false = weight)
  )

# weights per basin 
wt <- lt %>% 
  group_by(fish_basin) %>% 
  summarise(mean_wt = round(mean(weight), digits = 0)) %>% 
  ungroup()

wt


############## create full dataset for bioE froim May 2019 - May 2020 ---------
ful <- lt %>% 
  filter(sensor_value > 0 & sensor_unit %in% c("m")) %>%
  mutate(
    time_bins = floor_date(detection_timestamp_utc, unit = "1 hours")
  )

# determined numbered heard 
number_heard <- lt %>% 
  filter(sensor_unit %in% c("m")) %>% 
  group_by(fish_basin) %>% 
  summarise(n = n_distinct(floy_tag)) %>% 
  ungroup()


number_heard
# mean lab weights from Cruz-font et al. 2016 ------

glimpse(ful)
# create summarized bioE per day----
# mean daily temp, depth, and accel 
ful_1 <- ful %>% 
  dplyr::select(floy_tag, fish_basin, detection_timestamp_utc, name, 
                long_mean, 
                lat_mean, time_bins, date, 
                week, month, season, year, tl, fl, girth, weight,
                sensor_unit, sensor_value) 
    
glimpse(ful_1)


ful_1 <- ful_1 %>% 
  mutate(
    rec_group = case_when(name %in% c(3, 4, 11, 19, 23)  ~ "Central East-Basin",
                          name %in% 12 ~ "Black Bay",
                          name %in% c(2, 18) ~ "South East-Basin",
                          name %in% c(1, 9, 10) ~ "North East-Basin",
                          name %in% c(5, 6) ~ "Central West-Basin",
                          name %in% c(21, 22) ~ "Monaco Bay",
                          name %in% c(7, 20) ~ "Sucker Creek",
                          name %in% c(8, 17) ~ "North West-Basin",
                          name %in% 16 ~ "Hidden Bay",
                          name %in% 15 ~ "Central North-Basin",
                          name %in% c(13, 14) ~ "North North-Basin")
  )

glimpse(ful_1)


ful_1 <- ful_1 %>% 
  dplyr::select(floy_tag:detection_timestamp_utc,
                long_mean, lat_mean, rec_group, time_bins:sensor_value)




# ful %>% 
#   filter(sensor_unit %in% ("°C"), 
# !(floy_tag %in% "07478")) %>%
#   group_by(floy_tag, fish_basin, date) %>% 
#   summarise(mean_smr = mean(smr)) %>% 
#   ggplot(aes(x = date, y = mean_smr)) + 
#   geom_point(aes(colour = floy_tag), size = 3, alpha = 0.45)

# write bioE dataframe as rds ----- 
write_rds(x = ful_1, here("Saved Data", 
                          "lkt_depth.rds"))

