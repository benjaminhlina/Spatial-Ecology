# ----load packages ----
library(data.table)
library(dplyr)
library(forcats)
library(glatos)
library(ggplot2)
library(here)
library(lubridate)
library(magrittr)
library(readr)
library(stringr)
library(suncalc)
library(sf)

# ---- bring in clean downloaded ----

lt <- read_rds(here::here("Saved data", 
                          "kenauk lake trout 2017 - 2020.rds")) %>% 
  as_tibble()






smb <- read_rds(here::here("Saved data",
                           "kenauk smallmouth bass 2018 - 2020.rds")) %>% 
  as_tibble()

p_map <- st_read(dsn = here("Shapefiles",
                            "."),
                 layer = "plake_edit_wo_link")


lt$species <- "lt"
smb$species <- "smb"

dat <- rbind(lt, smb)

rm(lt)
rm(smb)

glimpse(dat)
dat <- dat %>% 
  select(floy_tag, detection_timestamp_utc, 
         sensor_value, sensor_unit, name, letter_name, 
         lat_mean, long_mean, fish_basin, receiver_basin, 
         tl, fl, girth, passed_filter, species)


dat <- dat %>% 
  filter(passed_filter == 1) %>% 
  arrange(floy_tag, detection_timestamp_utc) %>% 
  group_by(floy_tag) %>% 
  mutate(time_bin = floor_date(detection_timestamp_utc, unit = "2 hours")) %>% 
  arrange(floy_tag, time_bin) %>% 
  ungroup() 



results <- dat %>% 
  group_by(species, 
           floy_tag, time_bin, fish_basin, tl, fl, girth, sensor_unit) %>% 
  summarise(
    mean_lat = mean(lat_mean),
    mean_long = mean(long_mean), 
    rec = n_distinct(name), 
    num_hits = n(),
    mean_sensor = mean(sensor_value),
    median_sensor = median(sensor_value), 
    random_senor = sample(sensor_value, size = 1),
    sensor_sd = sd(sensor_value),
    sensor_sem = sd(sensor_value) / sqrt(n()), 
  ) %>% 
  ungroup() %>% 
  arrange(floy_tag, time_bin)
beepr::beep()

glimpse(results)

unique(results$rec)
unique(results$num_hits)

rd <- results %>% 
  filter(sensor_unit == "m")
rd




glimpse(coa)
coas <- st_as_sf(results, 
                 coords = c("mean_long", "mean_lat"), 
                 crs = st_crs(p_map))
glimpse(coas)
ggplot() + 
  geom_sf(data = p_map, fill = NA) + 
  geom_sf(data = coas, aes(colour = floy_tag, shape = species))





tests <- st_intersection(coas, p_map)
beepr::beep()


glimpse(tests)

glimpse(tests)
tests <- tests %>% 
  select(species:sensor_sem, geometry)
tests




tests1 <- tests
tests1



# add date, month, month_number, year to dataframe -----
tests1 <- tests1  %>% 
  mutate(date = date(time_bin),
         jdate = yday(time_bin),
         hour = hour(time_bin),
         month = month(x = time_bin, label = TRUE, abbr = FALSE),
         month_number = month(time_bin), 
         year = year(time_bin), 
         month_year = as.character(floor_date(time_bin, unit = "month")))


glimpse(tests1)




# add season -----
season <- tibble(month_number = c(9, 10, 11, 
                                  12, 1, 2, 
                                  3, 4, 5, 
                                  6, 7, 8), 
                 season = factor(c(rep("Fall", 3), 
                                   rep("Winter", 3), 
                                   rep("Spring", 3),
                                   rep("Summer", 3)),
                                 levels = c("Fall", "Winter", 
                                            "Spring", "Summer")))


glimpse(season)


tests1 <- merge(tests1, season, by = "month_number")




# paste season and year togeteher ------


tests1 <- tests1 %>% 
  mutate(season_year = paste(season, year))


glimpse(tests1)


# issue arrises with december as december's year is the previous year
# so when you paste season and year together you get winter 2017 when it really 
# is winter 2018


# fix using case_when which is like ifelse statments 
tests1 <- tests1 %>% 
  mutate(season_year = case_when(month_year == "2017-12-01" ~ "Winter 2018",
                                 month_year == "2018-12-01" ~ "Winter 2019",
                                 month_year == "2019-12-01" ~ "Winter 2020",
                                 month_year == "2020-12-01" ~ "Winter 2021",
                                 TRUE ~ season_year))



unique(tests1$season_year)
# convert season_year into factor 
tests1$season_year <- tests1$season_year %>% 
  as.factor() %>% 
  fct_relevel("Fall 2017", "Winter 2018", "Spring 2018", "Summer 2018", 
              "Fall 2018", "Winter 2019", "Spring 2019", "Summer 2019", 
              "Fall 2019", "Winter 2020", "Spring 2020", "Summer 2020",
              "Fall 2020", "Winter 2021", "Spring 2021", "Summer 2021")



# reconvert month_year into posixc  

tests1$month_year <- as.POSIXct(tests1$month_year, tz = "UTC")




# take timebins and convert into new column that is EST -----


tests1$time_bins_est <- with_tz(tests1$time_bin,
                                        tzone = "America/New_York")



start_date <- as.Date(min(tests1$time_bins_est))
end_date <- as.Date(max(tests1$time_bins_est))

plake_sunset_sunrise <- getSunlightTimes(date = seq.Date(from = start_date,
                                                         to =  end_date,
                                                         by = 1),
                                         keep = c("sunrise", "sunset", 
                                                  "dawn", "dusk", "night", 
                                                  "nadir", "nightEnd", 
                                                  "goldenHour", "goldenHourEnd"),
                                         lat = 45.81525,
                                         lon = 	-74.77110,
                                         tz = "EST")

glimpse(plake_sunset_sunrise)

# rename lat and long in plake sunset and sunrise

plake_sunset_sunrise <- plake_sunset_sunrise %>% 
  rename(lake_lat = lat,
         lake_long = lon)

# merge sunrise dataframe to detection dataframe ----



tests1 <- merge(tests1, plake_sunset_sunrise, by = "date")



# create day and night column based on whether detections were heard before sunrise or sunset ----

tests1$day_night <- ifelse(tests1$time_bins_est >
                                     tests1$sunrise &
                                     tests1$time_bins_est <
                                     tests1$sunset,
                                   "day", "night")


glimpse(tests1)


tests11 <- tests1 %>% 
  select(species, floy_tag, fish_basin:girth, time_bin,
         date, month_number, sensor_unit:sensor_sem, 
         jdate:day_night, geometry)



glimpse(tests11)
tests11

write_rds(tests11, here("Saved data", 
                      "Seasonal_COA_lt_smb_2h.rds"))


