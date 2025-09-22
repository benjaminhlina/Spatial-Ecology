
# load packages ----
library(brms) # beta mixed mobels
library(data.table)
library(dplyr)
library(fitdistrplus)
library(forcats)
library(ggplot2)
library(lemon)
# library(glmmTMB) # beta mixed models
library(glatos)
library(here)
library(lme4)
library(lubridate)
library(magrittr)
library(readr)
library(sandwich)
library(stringr)
library(suncalc)
library(sf)
library(sp)



# bring in clean downloaded ----

lt <- read_rds(here::here("Saved data", 
                          "kenauk lake trout 2017 - 2020.rds")) %>% 
  as_tibble()



glimpse(lt)

p_map <- st_read(dsn = here::here("Papineau reciever locations and maps", 
                                  "Papineau Lake Shapefile",
                                  "."),
                 layer = "lake")
# filter out double detections from temperturee ------
lt_filter <- lt %>% 
  filter(sensor_unit %in% c("m/s²", "m", NA))
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
  mutate(rec_group = case_when(name %in% c(3, 4, 11, 19)  ~ "Central Main Basin", 
                               name %in% 12 ~ "Black Bay", 
                               name %in% c(2, 18) ~ "South Main Basin", 
                               name %in% c(1, 9, 10) ~ "North Main Basin", 
                               name %in% c(5, 6) ~ "Central West Basin",
                               name %in% c(21, 22) ~ "Monaco Bay", 
                               name %in% c(7, 20) ~ "Sucker Creek", 
                               name %in% c(8, 17) ~ "North West Basin",
                               name %in% 16 ~ "Hidden Bay",
                               name %in% 15 ~ "Central North Basin", 
                               name %in% c(13, 14) ~ "North North Basin"))


lt_filter <- lt_filter %>% 
  mutate(rec_group = as.factor(rec_group))

levels(lt_filter$rec_group)





# okay that's fine except any additions will not be correct

# grab the first detection heard on each receiver 
rec_deploy <- lt_filter %>% 
  group_by(name) %>% 
  arrange(detection_timestamp_utc) %>%
  filter(row_number() == 1) %>% 
  dplyr::select(date, name, rec_group) %>% 
  arrange(rec_group) %>% 
  ungroup()

# View(rec_deploy)

# use the first heard detection on added receviers to change rec_num for specic groups
# those groups are CMB, SMB, WWB and MB
# 
# 
# add rec_group_number

lt_filter <- lt_filter %>% 
  mutate(rec_group_num = case_when(
    rec_group %in% "Black Bay" ~ 1,
    rec_group %in% "Central Main Basin" ~ if_else(date > ymd("2018-07-04"), 4, 3),
    rec_group %in% "Central North Basin" ~ 1,
    rec_group %in% "Central West Basin" ~ 2, 
    rec_group %in% "Hidden Bay" ~ 1, 
    rec_group %in% "Monaco Bay" ~ if_else(date > ymd("2019-06-14"), 2, 1), 
    rec_group %in% "North Main Basin" ~ 3, 
    rec_group %in% "North North Basin" ~ 2, 
    rec_group %in% "North West Basin" ~ 2, 
    rec_group %in% "South Main Basin" ~ if_else(date > ymd("2018-07-04"), 2, 1), 
    rec_group %in% "Sucker Creek" ~ if_else(date > ymd("2018-07-04"), 2, 1), 
    
  )
  )

rec_group_sum <- lt_filter %>% 
  group_by(rec_group, season_year,rec_group_num) %>% 
  summarise(n = n())


levels(lt_filter$receiver_basin)

lt_filter$receiver_basin <- as.character(lt_filter$receiver_basin)

lt_filter <- lt_filter %>% 
  mutate(receiver_basin = 
           case_when(name == 17 ~ "West Basin", 
                     TRUE ~ receiver_basin))




lt_filter$receiver_basin <- as.factor(lt_filter$receiver_basin)

lt_filter <- lt_filter %>% 
  group_by(receiver_basin) %>% 
  mutate(rec_basin_num = n_distinct(name)) %>%
  ungroup()


glimpse(lt_filter)
unique(lt_filter$rec_basin_num)
# View(rec_group_sum)
# remove summer and fall 2019 as they are incomplete
lt_wo_sf <- lt_filter %>% 
  filter(!(season_year %in% c("Summer 2019", "Fall 2019")))


# determine how many detections per rec group 
rec_gr_a <- lt_wo_sf %>% 
  group_by(rec_group, rec_group_num) %>% 
  summarise(n = n()) %>% 
  ungroup()


rec_gr_a

# created weighted n value ----
rec_gr_a <- rec_gr_a %>% 
  mutate(n_w = round(n / rec_group_num, digits = 0))
rec_gr_a


# filter out just rec goups that we added recs to ----
cmb <- rec_gr_a %>%  
  filter(rec_group %in% c("Central Main Basin")) %>% 
  dplyr::select(rec_group_num, n_w)


smbs <- rec_gr_a %>%  
  filter(rec_group %in% c("South Main Basin")) %>% 
  dplyr::select(rec_group_num, n_w)


sc <- rec_gr_a %>%  
  filter(rec_group %in% c("Sucker Creek")) %>% 
  dplyr::select(rec_group_num, n_w)


# chi square tests for indpences for number of rec per group added -----

chisq.test(cmb)
chisq.test(smbs)
chisq.test(sc)



# ggplot(data = rec_gr_a, aes(x = rec_group_num, y = n_w)) + 
#   facet_wrap(.~ rec_group, scale = "free_y") +
#   stat_smooth(method = "glm", method.args = list(family = poisson), 
#               aes(group = rec_group, colour = "A"), size = 1.25, 
#               se = TRUE,  fullrange = TRUE) + 
#   
#   stat_smooth(method = "lm", aes(group = rec_group, colour = "B"), size = 1.25, 
#               se = TRUE, show.legend = TRUE, linetype = "dashed", fullrange = TRUE) +
#   scale_y_continuous(limits = c(0, 150000),
#                      breaks = seq(0, 150000, 30000)) + 
#   geom_point(size = 3) +
#   scale_colour_manual(name = "Model Type", labels = c("Poisson GLM", 
#                                                       "Linear Model"), 
#                       values = c("black", "red")) + 
#   theme_classic(base_size = 18) + 
#   theme(strip.text = element_text(colour = "black"), 
#         axis.text = element_text(colour = "black")) + 
#   labs(x = "Number of Receivers per Receiver Group", 
#        y = "Number of Detections")

dd <- lt_filter %>% 
  group_by(date) %>% 
  summarise(n = n())

ggplot(data = dd, aes(x = date, y = n)) + 
  geom_line()

# make rec_group lat and long means from mean lat and long per receiver ----
lt_filter <- lt_filter %>% 
  group_by(rec_group) %>% 
  mutate(rec_mean_lat = mean(lat_mean), 
         rec_mean_long = mean(long_mean)) %>% 
  ungroup()

# create time step for 2 hours which is in secondds 
time_step <- 86400 

lt_filter <- lt_filter %>% 
  mutate(time_bin = floor_date(detection_timestamp_utc, 
                               unit = "day") + (time_step / 2)) 



lt_filter <- lt_filter %>% 
  group_by(season_year) %>% 
  mutate(season_length = n_distinct(time_bin)) %>% 
  ungroup()

glimpse(lt_filter)

# determing S 

S <- lt_filter %>% 
  group_by(floy_tag, fish_basin, receiver_basin, rec_basin_num, rec_group,
           rec_mean_lat, rec_mean_long, 
           season_year, time_bin, 
           season_length, rec_group_num) %>% 
  summarise(
    rec_group_det = n_distinct(name), 
    S = n()
  ) %>% 
  ungroup %>% 
  filter(!S < 10) %>% 
  arrange(floy_tag, time_bin)

S

# create t which is the nunber of unique times a fish was herad ------
# anywhere per time_bin ----

t <-  lt_filter %>% 
  group_by(floy_tag, season_year, season_length, time_bin) %>% 
  summarise(t = n()) %>% 
  ungroup()

t

# combine S and T datdframes to calculate residence index -----
ri_k <- S %>% 
  left_join(t, by = c("floy_tag", "season_year", "season_length",
                      "time_bin"))


ri_k  
glimpse(ri_k)

# clacualte residence index by taking s / t ------



ri_k <- ri_k %>% 
  mutate(roam_index = rec_group_det / rec_group_num)





ri_k <- ri_k %>% 
  mutate(roam_index_w = roam_index / rec_group_num)


huh <- ri_k %>% 
  filter(roam_index > 1)

glimpse(huh)
ggplot(data = ri_k, aes(x = roam_index_w)) + 
  geom_histogram()



descdist(ri_k$roam_index_w)


dist.norm <- fitdist(ri_k$roam_index_w, distr = "norm")

plot(dist.norm)

# plot 



fish_id <- unique(ri_k$floy_tag)

ri_s <- ri_k %>% 
  arrange(fish_basin, floy_tag)


pdf(here::here("Fish and tagging data", 
               "Receiver Downloads", 
               "residency index plots",  
               "Seaonal_roaming_index_per_fish.pdf"), 
    width = 12, height = 10)

for (i in 1:length(fish_id)) {
  df <- ri_k %>% 
    filter(floy_tag %in% fish_id[i])
  
  ggplot() +
    geom_sf(data = p_map) + 
    facet_wrap(. ~ season_year, ncol = 4) + 
    geom_point(data = df, 
               aes(x = rec_mean_long, 
                   y = rec_mean_lat, 
                   size = roi_w)) +
    scale_size_continuous(name = "Roaming Index", 
                          limits = c(0, 1), 
                          breaks = seq(0, 1, 0.25)) + 
    coord_sf() +
    theme_void(base_size = 18) + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    labs(title = paste(fish_id[i], "-", df$fish_basin,
                       sep = "")) -> p
  
  print(p)
  
}

dev.off()
