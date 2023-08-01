# load packages ----
# library(brms) # beta mixed mobels
library(dplyr)
library(forcats)
library(ggplot2)
library(lemon)
library(glmmTMB) # beta mixed models
library(glatos)
library(here)
library(lemon)
library(lubridate)
library(moments)
library(readr)
library(sf)
library(sp)




# bring in clean downloaded ----

lt <- read_rds(here::here("Saved data", 
                          "kenauk lake trout 2017 - 2020.rds")) %>% 
  as_tibble()


glimpse(lt)

# unique(lt$passed_filter)

p_map <- st_read(dsn = here::here("Shapefiles",
                                  "."),
                 layer = "plake_edit_wo_link")
# filter out double detections from temperature ------
lt_filter <- lt %>% 
  filter(sensor_unit %in% c("m/s²", "m", NA) & passed_filter == 1)


unique(lt_filter$name)

unique(lt$receiver_basin)
# fid <- lt %>% 
#   group_by(floy_tag, fish_basin) %>% 
#   summarise(n = n())
# 
# 
# fid
# lts <- lt %>% 
#   sample_n(25000)
# 
# lts <- lt %>% 
#   filter(floy_tag %in% c(1152, 1153, 1654))
# write_rds(lts, path = "D:/Dropbox/CU/Data/RI Workshop/data/ri_sample_data.rds")


# make receiver groups based on location in the lake -----
lt_filter <- lt_filter %>% 
  mutate(rec_group = case_when(name %in% c(3, 4, 11, 19, 23)  ~ "Central East-Basin", 
                               name %in% 12 ~ "Black Bay", 
                               name %in% c(2, 18) ~ "South East-Basin", 
                               name %in% c(1, 9, 10) ~ "North East-Basin", 
                               name %in% c(5, 6) ~ "Central West-Basin",
                               name %in% c(21, 22) ~ "Monaco Bay", 
                               name %in% c(7, 20) ~ "Sucker Creek", 
                               name %in% c(8, 17) ~ "North West-Basin",
                               name %in% 16 ~ "Hidden Bay",
                               name %in% 15 ~ "Central North-Basin", 
                               name %in% c(13, 14) ~ "North North-Basin"))


lt_filter <- lt_filter %>% 
  mutate(rec_group = as.factor(rec_group))

levels(lt_filter$rec_group)



levels(lt_filter$season_year)

# okay that's fine except any additions will not be correct

# grab the first detection heard on each receiver 
rec_deploy <- lt_filter %>% 
  group_by(name) %>% 
  arrange(detection_timestamp_utc) %>%
  filter(row_number() == 1) %>% 
  dplyr::select(date, name, rec_group) %>% 
  arrange(rec_group) %>% 
  ungroup()

rec_deploy
# View(rec_deploy)

# use the first heard detection on added receviers to change rec_num for specic groups
# those groups are CMB, SMB, WWB and MB
# 
# 
# add rec_group_number

glimpse(lt_filter)

lt_filter <- lt_filter %>% 
  mutate(rec_group_num = case_when(
    rec_group %in% "Black Bay" ~ 1,
    rec_group %in% "Central East-Basin" ~ case_when(date <= ymd("2018-07-04")  ~ 3,
                                                    date >= ymd("2018-07-05") & 
                                                      date <= ymd("2019-11-05") ~ 4,
                                                    date >= ymd("2019-11-06") ~ 5),
    rec_group %in% "Central North-Basin" ~ 1,
    rec_group %in% "Central West-Basin" ~ 2, 
    rec_group %in% "Hidden Bay" ~ 1, 
    rec_group %in% "Monaco Bay" ~ case_when(date <= ymd("2019-06-14")  ~ 1, 
                                            date >= ymd("2019-06-15") & 
                                              date <= ymd("2019-11-05") ~ 2, 
                                            date >= ymd("2019-11-06") ~ 1), 
    rec_group %in% "North East-Basin" ~ 3, 
    rec_group %in% "North North-Basin" ~ 2, 
    rec_group %in% "North West-Basin" ~ 2, 
    rec_group %in% "South East-Basin" ~ if_else(date > ymd("2018-07-05"), 2, 1), 
    rec_group %in% "Sucker Creek" ~ if_else(date > ymd("2018-07-05"), 2, 1),
  )
  )



lt_filter <- lt_filter %>% 
  mutate(rec_basin_num = case_when(
    receiver_basin %in% "East Basin" ~ case_when(date <= ymd("2018-07-04")  ~ 8,
                                                 date >= ymd("2018-07-05") & 
                                                   date <= ymd("2019-11-05") ~ 10,
                                                 date >= ymd("2019-11-06") ~ 11),
    receiver_basin %in% "West Basin" ~ case_when(date <= ymd("2018-07-04")  ~ 5, 
                                                 date >= ymd("2018-07-05") & 
                                                   date <= ymd("2018-07-19") ~ 6,
                                                 date >= ymd("2018-07-20") & 
                                                   date <= ymd("2019-06-13") ~ 7,
                                                 date >= ymd("2019-06-14") & 
                                                   date <= ymd("2019-11-05") ~ 8,
                                                 date >= ymd("2019-11-06") ~ 7),
    receiver_basin %in% "North Basin" ~ 4
  )
  )


glimpse(lt_filter)


rec_group_sum <- lt_filter %>% 
  group_by(rec_group, season_year,rec_group_num) %>% 
  summarise(n = n()) %>% 
  arrange(rec_group, season_year)


rec_group_sum

# determine how many detections per rec group 
rec_gr_a <- lt_filter %>% 
  group_by(rec_group, rec_group_num) %>% 
  summarise(n = n()) %>% 
  ungroup()




# created weighted n value ----
rec_gr_a <- rec_gr_a %>% 
  mutate(n_w = round(n / rec_group_num, digits = 0))
rec_gr_a


# filter out just rec goups that we added recs to ----
cmb <- rec_gr_a %>%  
  filter(rec_group %in% c("Central East-Basin")) %>% 
  dplyr::select(rec_group_num, n_w)
cmb

smbs <- rec_gr_a %>%  
  filter(rec_group %in% c("South East-Basin")) %>% 
  dplyr::select(rec_group_num, n_w)


sc <- rec_gr_a %>%  
  filter(rec_group %in% c("Sucker Creek")) %>% 
  dplyr::select(rec_group_num, n_w)


# chi square tests for indpences for number of rec per group added -----

chisq.test(cmb)
chisq.test(smbs)
chisq.test(sc)


# make rec_group lat and long means from mean lat and long per receiver ----
lt_filter <- lt_filter %>% 
  group_by(rec_group) %>% 
  mutate(rec_mean_lat = mean(lat_mean), 
         rec_mean_long = mean(long_mean)) %>% 
  ungroup()

# create time step for 2 hours which is in seconds
time_step <- 86400 

lt_filter <- lt_filter %>% 
  mutate(time_bin = floor_date(detection_timestamp_utc, 
                               unit = "day") + (time_step / 2)) 




lt_filter <- lt_filter %>% 
  group_by(season_year) %>% 
  mutate(season_length = n_distinct(time_bin)) %>% 
  ungroup()

glimpse(lt_filter)




glimpse(lt_filter)
lt_filter <- lt_filter %>% 
  mutate(fish_id = as.numeric(floy_tag))

###################################### for daily residences ------
# create summarized dataframe of for S whcih is per time_bin the number -----
# of unique detections per rec_group -----
S <- lt_filter %>% 
  group_by(floy_tag, fish_basin, receiver_basin,
           rec_basin_num,
           rec_group,
           rec_mean_lat, rec_mean_long, 
           season_year, season, year, time_bin, 
           season_length, rec_group_num
  ) %>% 
  summarise(S = n(),
            rec_group_det = n_distinct(name)
  ) %>% 
  ungroup %>% 
  # complete(floy_tag, 
  #          nesting(
  #            fish_basin, 
  #            receiver_basin,
  #            rec_basin_num,
  #            rec_group,
  #            rec_mean_lat, 
  #            rec_mean_long, 
  #            season_year, 
  #            time_bin, 
  #            season_length, 
  #            rec_group_num
  #          ), 
  #          fill = list(S = 0, 
  #                      rec_group_det = 0)
  # ) %>% 
  filter(!S < 10) %>%
  arrange(floy_tag, time_bin)

glimpse(S)
# create t which is the nunber of unique times a fish was heard ------
# anywhere per time_bin ----

t <-  lt_filter %>% 
  group_by(floy_tag, season_year, season_length, season, time_bin, year) %>% 
  summarise(t = n()) %>% 
  ungroup() %>% 
  arrange(floy_tag, time_bin)
# %>% 
  # complete(floy_tag, nesting(season_year, season_length, time_bin))

glimpse(S)
glimpse(t)


# combine S and T datdframes to calculate residence index -----
ri_k <- S %>% 
  left_join(t, by = c("floy_tag", "season_year", "season_length" , "season", "year",  
                      "time_bin"))


# ri_k[is.na(ri_k)] <- 0 


glimpse(ri_k)  
ri_k <- ri_k %>% 
  mutate(rec_group_num = case_when(floy_tag == "07025" 
                                   & 
                                     time_bin == ymd_hms("2019-11-06 12:00:00") 
                                   & 
                                     rec_group == "Monaco Bay" ~ 2, 
                                   floy_tag == "1161"
                                   &
                                     time_bin == ymd_hms("2018-07-05 12:00:00")
                                   & 
                                     rec_group == "South East-Basin" ~ 2, 
                                   TRUE ~ rec_group_num))

ri_k <- ri_k %>% 
  mutate(rec_basin_num = case_when(floy_tag == "07025" 
                                   & 
                                     time_bin == ymd_hms("2019-11-06 12:00:00")
                                   ~ 8,
                                   TRUE ~ rec_basin_num)
  )
# clacualte residence index by taking s / t -----
glimpse(ri_k)


ri_k <- ri_k %>% 
  mutate(ri = S / t, 
         roi = rec_group_det / rec_group_num) 
ri_k$ri[is.nan(ri_k$ri)] <- 0

ri_k <- ri_k %>% 
  mutate(ri_w = ri / rec_group_num, 
         roi_w = roi / rec_group_num)

ri_k <- arrange(ri_k, floy_tag, time_bin, rec_group)



# er <- ri_k %>% 
#   mutate(rec_group_num = case_when(floy_tag == "07025" 
#                                    & 
#                                      time_bin == ymd_hms("2019-11-06 12:00:00") 
#                                    & 
#                                      rec_group == "Monaco Bay" ~ 2, 
#                                    floy_tag == "1161"
#                                    &
#                                      time_bin == ymd_hms("2018-07-05 12:00:00")
#                                    & 
#                                      rec_group == "South East-Basin" ~ 2, 
#                                    TRUE ~ rec_group_num))


glimpse(ri_k)




ggplot(data = ri_k, aes(x = ri_w)) +  
  geom_histogram()

fitdistrplus::descdist(ri_k$ri_w)





# plot residence indexs 
# ggplot(data = ri_k) +
#   facet_wrap(. ~ season_year, ncol = 4, scale = "free_x") + 
#   geom_point(aes(x = time_bin, y = ri, colour = rec_group, 
#                  shape = fish_basin), size = 1.2) + 
#   theme_classic()


# seasonal bases ------


glimpse(ri_k)
ri_k <- ri_k %>% 
  mutate(date = floor_date(time_bin, unit = "day"))



glimpse(ri_k)

write_rds(ri_k, here("Saved Data", 
                     "daily_ri_roi_lkt.rds"))

# on a seasonal time frame -----

ri_k <- ri_k  %>%
  group_by(floy_tag, rec_group, date) %>% 
  mutate(s_day = n()) %>% 
  ungroup()



ri_s <- ri_k %>%  
  group_by(floy_tag, fish_basin, receiver_basin, 
           rec_basin_num,
           rec_group, rec_group_num, 
           rec_mean_lat,
           rec_mean_long, 
           season_year, season_length, season, 
           year) %>% 
  summarise(s = sum(s_day),
            rn = n_distinct(rec_group_det)) %>% 
  ungroup()
ri_s


glimpse(ri_k)
glimpse(ri_s)

# calculate seasonal RI and ROI -------
ri_s <- ri_s %>% 
  mutate(ri =  s / season_length, 
         roi = rn / rec_group_num)

glimpse(ri_s)

# WEIGHTED PER REC ------
ri_s <- ri_s %>% 
  mutate(ri_w = ri / rec_group_num, 
         roi_w = roi / rec_basin_num)

glimpse(ri_s)

# addd in unique number of fish heard per season -----
ri_s <- ri_s %>% 
  group_by(fish_basin, rec_group, rec_mean_lat, rec_mean_long, season_year, 
           season, year) %>%
  mutate(fish_unique = n_distinct(floy_tag)) %>% 
  ungroup()


ri_s <- ri_s %>% 
  mutate(ri_w_f = ri_w / fish_unique, 
         roi_w_f = roi_w / fish_unique)

# save ri dataframe -------
write_rds(ri_s, file = here("Saved Data", 
                            "season_ri_lt.rds"))
# Add depth into  ------

glimpse(lt)
lt_depth <- lt_filter %>% 
  filter(sensor_unit == "m" & sensor_value >= 0)



# add depth bins -----
lt_depth <- lt_depth %>% 
  mutate(depth_group = case_when(
    between(sensor_value, 00, 05)  ~ "5 m", 
    between(sensor_value, 05, 10) ~ "10 m",
    between(sensor_value, 10, 15) ~ "15 m", 
    between(sensor_value, 15, 20) ~ "20 m", 
    between(sensor_value, 20, 25) ~ "25 m", 
    between(sensor_value, 25, 30) ~ "30 m", 
    between(sensor_value, 30, 35) ~ "35 m", 
    between(sensor_value, 35, 40) ~ "40 m", 
    between(sensor_value, 40, 45) ~ "45 m", 
    between(sensor_value, 45, 50) ~ "50 m", 
    between(sensor_value, 50, 55) ~ "55 m", 
    between(sensor_value, 55, 60) ~ "60 m", 
    between(sensor_value, 60, 65) ~ "65 m", 
    between(sensor_value, 65, 70) ~ "70 m"
  )
  ) 

ri_d <- lt_depth %>% 
  mutate(depth = as.numeric(stringr::str_remove(depth_group, " m")))

lt_depth <- lt_depth %>% 
  mutate(depth_group = as.factor(depth_group))

lt_depth$depth_group <- lt_depth$depth_group %>% 
  fct_relevel("5 m", "10 m", "15 m", "20 m", 
              "25 m", "30 m", "35 m", "40 m", 
              "45 m", "50 m", "55 m", "60 m", 
              "65 m", "70 m")

unique(lt_depth$depth_group)


# calcualte S with depth invovled ------

s_d <- lt_depth %>% 
  group_by(floy_tag, fish_basin, depth_group, rec_group, receiver_basin, 
           rec_mean_lat, rec_mean_long, season_year, time_bin) %>% 
  summarise(S = n()) %>% 
  ungroup() %>% 
  arrange(time_bin, floy_tag)
s_d

# calcualte t with depth invovled ----
t_d <- lt_depth %>% 
  group_by(floy_tag, season_year, time_bin) %>% 
  summarise(t = n()) %>% 
  ungroup()

t_d

# lt_m <- lt_depth %>% 
#   dplyr::select(floy_tag, time_bin, rec_group, sensor_value, depth_group)
# 
# lt_m

# rm(lt)
# rm(lt_depth)
# rm(lt_filter)
# rm(S)
# rm(s_d)
# rm(S_basin)
# rm(t)
# rm(t_d)
# 
# gc()

ri_d <- s_d %>% 
  left_join(t_d, by = c("floy_tag", "season_year", "time_bin")) %>% 
  arrange(time_bin, floy_tag)


ri_d$ri <- ri_d$S / ri_d$t

glimpse(ri_d)
unique(ri_d$depth_group)
ri_d

ggplot(data = ri_d, aes(x = time_bin, y = ri, fill = rec_group)) + 
  geom_bar(stat = "identity", position = "stack") 
  # scale_y_reverse()

# fitdistrplus::descdist(ri_d$ri)
# fitdistrplus::descdist(ri_k$ri)
# levels(ri_d$depth_group)
# glimpse(ri_k)

# m <- glmmTMB(ri ~ rec_group + date * season_year + (1|floy_tag),
#              data = ri_k, 
#              ziformula = ~rec_group,
#              verbose = TRUE, 
#              family = beta_family())
# 
# beepr::beep()
# glimpse(ri_k)
# ri_ks <- ri_k %>% 
#   filter(season_year %in% c("Summer 2018", "Winter 2019"))

# m1 <- brm(ri ~ rec_group * depth_group + season_year + fish_basin + (1|floy_tag),
#           data = ri_d,
#           family = zero_one_inflated_beta(),
#           cores = 9)


# summary(m1)
# write_rds(m1, here::here("Fish and tagging data", 
#                          "Receiver Downloads", 
#                          "Model Results", 
#                          "RI_bme_basin.rds"))
# summary(m1)
# 
# 
# glimpse(m1)
# ggplot(data = ri_d, aes(x = ri)) + 
#   geom_histogram()
# ri_d <- ri_d %>% 
#   mutate(date = floor_date(time_bin, unit = "day")) %>% 
#   arrange(time_bin)
# glimpse(ri_d)
# ri_d
# ggplot(data = ri_d, aes(x = depth_group, y = ri)) + 
#   facet_wrap(. ~ season_year, scale = "free_x") + 
#   geom_boxplot(aes(fill = depth_group)) 
# 
# glimpse(ri_k)
# ggplot(data = ri_k, aes(x = date, y = ri)) + 
#   geom_boxplot(aes(group = date)) + 
#   facet_wrap(. ~ season_year, scale = "free_x")
#   
#   
# 
# ggplot(data = ri_d, aes(x = time_bin, y = ri)) + 
#   facet_wrap(.~ season_year, scale = "free_x") + 
#   geom_point(aes(colour = depth_group, group = depth_group))