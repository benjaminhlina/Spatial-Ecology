# load packages ----

library(dplyr)
library(data.table)
library(forcats)
library(glatos)
library(ggplot2)
library(here)
library(lubridate)
library(janitor)
library(lemon)
library(purrr)
library(readr)
library(tibble)
library(tidyr)

# bring in clean downloaded ----

lt <- read_rds(here::here("Saved data", 
                          "kenauk lake trout 2017 - 2020.rds"))
glimpse(lt)

# add in rec groups ------
lt <- lt %>% 
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




glimpse(lt)

# arrange and select only the needed columns -----
lt <- lt %>% 
  filter(sensor_unit %in% c( "m/s²", NA, "m")) %>% 
  arrange(floy_tag, detection_timestamp_utc) %>%
  dplyr::select(floy_tag, fish_basin, 
                tl, girth,
                name, 
                lat_mean, long_mean,
                rec_group,  receiver_basin, 
                detection_timestamp_utc, 
                # dates, 
                # week, month, months, month_number, 
                season, year, season_year
  ) %>% 
  rename(rec_basin = receiver_basin) %>% 
  
  mutate(
    across(ends_with("basin"), ~ stringr::str_replace(as.character(.x),  
                                                      " Basin", "")
    ), 
    across(ends_with("basin"), ~ fct_relevel(factor(.x), 
                                             "East",
                                             "West",
                                             "North")
    )
  )

# determine every time name switches ------
lt <- lt %>% 
  arrange(floy_tag, detection_timestamp_utc) %>% 
  group_by(floy_tag) %>% 
  mutate(change_rec = if_else(name != lag(name) | 
                                is.na(lag(name)), 
                              1, 0)) %>% 
  ungroup()

glimpse(lt)


# create arrival column and departure column by labeling first and last det ----
lt_st <- lt %>% 
  arrange(floy_tag, detection_timestamp_utc) %>% 
  group_by(floy_tag) %>% 
  mutate(arrive = if_else(name != lag(name) | 
                            is.na(lag(name)), 
                          1, 0), 
         depart = if_else(name != lead(name) | 
                            is.na(lead(name)), 
                          1, 0))  %>% 
  ungroup() %>%
  arrange(floy_tag, detection_timestamp_utc) %>%
  mutate(
    date = floor_date(detection_timestamp_utc, unit = "day"), 
  )


glimpse(lt_st)



# clean this up by keeping only when arrive and depart both equal 1 
lt_clean <- lt_st %>% 
  filter(arrive == 1 | depart == 1) %>% 
  arrange(floy_tag, detection_timestamp_utc) %>% 
  rownames_to_column("event_id")



# add when season starts and ends -----
lt_clean <- lt_clean %>% 
  mutate(
    season_start = case_when(
      season_year %in% "Fall 2017" ~ ymd_hms("2017-09-01 00:00:00"), 
      season_year %in% "Winter 2018" ~ ymd_hms("2017-12-01 00:00:00"), 
      season_year %in% "Spring 2018" ~ ymd_hms("2018-03-01 00:00:00"), 
      season_year %in% "Summer 2018" ~ ymd_hms("2018-06-01 00:00:00"), 
      season_year %in% "Fall 2018"~ ymd_hms("2018-09-01 00:00:00"), 
      season_year %in% "Winter 2019" ~ ymd_hms("2018-12-01 00:00:00"), 
      season_year %in% "Spring 2019" ~ ymd_hms("2019-03-01 00:00:00"), 
      season_year %in% "Summer 2019" ~ ymd_hms("2019-06-01 00:00:00"), 
      season_year %in% "Fall 2019" ~ ymd_hms("2019-09-01 00:00:00"), 
      season_year %in% "Winter 2020" ~ ymd_hms("2019-12-01 00:00:00"), 
      season_year %in% "Spring 2020" ~ ymd_hms("2020-03-01 00:00:00"), 
      season_year %in% "Summer 2020" ~ ymd_hms("2020-06-01 00:00:00"), 
      season_year %in% "Fall 2020" ~ ymd_hms("2020-09-01 00:00:00"),  
      season_year %in% "Winter 2021" ~ ymd_hms("2020-12-01 00:00:00"),
      season_year %in% "Spring 2021" ~ ymd_hms("2021-03-01 00:00:00"), 
      season_year %in% "Summer 2021" ~ ymd_hms("2021-06-01 00:00:00")
    ), 
    season_end = case_when(
      season_year %in% "Fall 2017" ~ ymd_hms("2017-11-30 23:59:59"), 
      season_year %in% "Winter 2018" ~ ymd_hms("2018-02-28 23:59:59"), 
      season_year %in% "Spring 2018" ~ ymd_hms("2018-05-31 23:59:59"), 
      season_year %in% "Summer 2018" ~ ymd_hms("2018-08-31 23:59:59"), 
      season_year %in% "Fall 2018"~ ymd_hms("2018-11-30 23:59:59"), 
      season_year %in% "Winter 2019" ~ ymd_hms("2019-02-28 23:59:59"), 
      season_year %in% "Spring 2019" ~ ymd_hms("2019-05-31 23:59:59"), 
      season_year %in% "Summer 2019" ~ ymd_hms("2019-08-31 23:59:59"), 
      season_year %in% "Fall 2019" ~ ymd_hms("2019-11-30 23:59:59"), 
      season_year %in% "Winter 2020" ~ ymd_hms("2020-02-29 23:59:59"), 
      season_year %in% "Spring 2020" ~ ymd_hms("2020-05-31 23:59:59"), 
      season_year %in% "Summer 2020" ~ ymd_hms("2020-08-31 23:59:59"), 
      season_year %in% "Fall 2020" ~ ymd_hms("2020-11-30 23:59:59"),  
      season_year %in% "Winter 2021" ~ ymd_hms("2021-02-28 23:59:59"),
      season_year %in% "Spring 2021" ~ ymd_hms("2021-05-31 23:59:59"), 
      season_year %in% "Summer 2021" ~ ymd_hms("2021-08-31 23:59:59"), 
    )
    
  )



glimpse(lt_clean)



# ----- Determine time spent by taking diff between arrive & depart times ------
lt_clean_1 <- lt_clean %>% 
  group_by(floy_tag) %>%
  arrange(floy_tag, detection_timestamp_utc) %>% 
  
  mutate(
    time_spent = as.numeric(detection_timestamp_utc - 
                              lag(detection_timestamp_utc,
                                  default = first(detection_timestamp_utc))
    ), 
    season_length = round(as.numeric(season_end - season_start))
  ) %>% 
  ungroup()


glimpse(lt_clean_1)



lt_clean_1 <- lt_clean_1 %>% 
  mutate(
    time_spent_min = time_spent / 60, 
    time_spent_h = time_spent_min / 60, 
    time_spent_d = time_spent_h / 24
  )


glimpse(lt_clean_1)
summary(lt_clean_1$time_spent_d)

lt_clean_1 %>% 
  filter(time_spent_d > season_length)

# ----- fix when days spent exceeds season_length 
# lt_clean_2 <- bind_rows(
#   lt_clean_1,
#   lt_clean_1 %>%
#     filter(
#         lead(time_spent_d > season_length & lag(arrive) == 1)
#     ) %>%
#     # filter(season != lag(season) &
#     #        name == name) %>%
#     mutate(
#       detection_timestamp_est = season_end,
#       depart = 1,
#       arrive = 0,
#     ),
#   # lt_clean_1 %>%
#   #   # group_by(season_year) %>%
#   #   filter(
#   #     (time_spent_d > season_length & lag(arrive) == 0) |
#   #       lead(time_spent_d > season_length & lag(arrive) == 0)
#   #   ) %>%
#   #   # filter(season != lead(season) &
#   #   #          name == name) %>%
#   #   mutate(
#   #     detection_timestamp_est = season_end,
#   #     depart = 1,
#   #     arrive = 0),
#   #
# ) %>%
#   arrange(floy_tag, detection_timestamp_est) %>%
#   mutate(
#     time_spent = as.numeric(detection_timestamp_est -
#                               lag(detection_timestamp_est,
#                                   default = first(detection_timestamp_est))),
#     time_spent_min = time_spent / 60,
#     time_spent_h = time_spent_min / 60,
#     time_spent_d = time_spent_h / 24,
#     time_spent_d = case_when(time_spent_d < 0 ~ 0,
#                              time_spent_d == 0 ~ 0,
#                              time_spent_d > 0 ~ time_spent_d)
#   ) %>%
# 
#   dplyr::select(floy_tag:detection_timestamp_est, date, season:season_year,
#                 season_start, season_end,
#                 change_rec:depart, time_spent:time_spent_d) %>%
#   rownames_to_column("event_id") %>%
#   mutate(event_id = as.numeric(event_id))
# 



glimpse(lt_clean_2)
lt_clean_1 %>%
  dplyr::select(event_id:fish_basin, name, rec_group, detection_timestamp_utc,
                arrive, depart,
                time_spent_d)
lt_clean_1 %>%
  filter((time_spent_d > season_length & lag(arrive) == 1) |
           lead(time_spent_d > season_length & lag(arrive) == 1)
  ) %>% 
  dplyr::select(event_id:fish_basin, name, rec_group, detection_timestamp_utc,
                arrive, depart,
                time_spent_d) %>% 
  print(n = 34)
glimpse(lt_clean_1)
# lt_clean_1 %>%
#   filter(event_id == 265188) %>%
#   dplyr::select(event_id:fish_basin, name, rec_group, detection_timestamp_est,
#                 arrive, depart,
#                 time_spent_d)
bind_rows(
  lt_clean_1 %>%
    # group_by(season_year) %>% 
    filter((time_spent_d > season_length & lag(arrive) == 1) |
             lead(time_spent_d > season_length & lag(arrive) == 1)
    ) %>%
    dplyr::select(floy_tag:fish_basin, name, rec_group, detection_timestamp_utc,
                  arrive, depart, season_year, season_start, season_end,
                  time_spent_d), 
  lt_clean_1 %>%
    # group_by(season_year) %>% 
    filter((time_spent_d > season_length & lag(arrive) == 1) |
             lead(time_spent_d > season_length & lag(arrive) == 1)
    ) %>%
    dplyr::select(floy_tag:fish_basin, name, rec_group, detection_timestamp_utc,
                  arrive, depart, season_year, season_start, season_end, 
                  time_spent_d) %>% 
    mutate(
      detection_timestamp_est = season_end,
      depart = 1,
      arrive = 0
    )
  
) %>% 
  ungroup() %>% 
  arrange(floy_tag, detection_timestamp_est) %>% 
  rownames_to_column("event_id") %>% 
  dplyr::select(event_id:fish_basin, name, rec_group, detection_timestamp_est, 
                arrive, depart, 
                time_spent_d) %>% 
  print(n = 68)

# lt_clean_2 %>%
#   filter(floy_tag == "05781" & date == ymd("2019-06-06")) %>%
#   dplyr::select(event_id:fish_basin, name, rec_group, detection_timestamp_est,
#                 arrive, depart,
#                 time_spent_d)
# summary(lt_clean_2$time_spent_d)
# 
# lt_clean_2 %>%
#   filter((time_spent_d > season_length & lag(arrive) == 1) |
#   lead(time_spent_d > season_length & lag(arrive) == 1)) %>%
#   dplyr::select(event_id:fish_basin, name, rec_group, detection_timestamp_est,
#                 arrive, depart,
#                 time_spent_d)
# lt_clean_2 %>%
#   filter((time_spent_d > season_length & lag(arrive) == 1) |
#   lead(time_spent_d > season_length & lead(arrive) == 1)) %>%
#   dplyr::select(event_id:fish_basin, name, rec_group, detection_timestamp_est,
#                 arrive, depart,
#                 time_spent_d)

# toString(sprintf("'%s'",sort(unique(lt_clean_1$season_year))



lt_time_spent <- lt_clean_1 %>% 
  filter(depart == 1 & time_spent_d < season_length)

lt_clean_1 %>% 
  filter(time_spent_d > season_length)


# lt_clean_1 <- lt_clean_1 %>% 
#   group_by(floy_tag) %>% 
#   mutate(
#     event_start = if_else(
#       season != lag(season), 
#       as.numeric(season_end - as.Date(date)),  
#       0
#     ),
#     event_end = if_else(
#       season != lag(season), 
#       as.numeric(as.Date(date) - season_start), 
#       0)
#   ) %>% 
#   ungroup()
# 
# 
# glimpse(lt_clean_1)

# lt_clean_2 <- lt_clean_1 %>% 
#   ungroup() %>% 
#   group_split(event_id) %>% 
#   map_dfr(~ .x %>% 
#             if_else(.$time_spent_d > .$season_length,
#             add_row(event_id = first(.$event_id), 
#                     type = 'F', 
#                     `other-col` = 'F'), 
#             .$time_spent_d))  %>%
#   slice(-n())
# mutate(
#   time_spent_d_2 = if_else(time_spent_d > season_length,  
#                          time_spent_d
#                            # TRUE ~ time_spent_d
#                            )) %>% 
# ungroup()



glimpse(lt_clean_1)

# 
# lt_c <- lt_clean_2 %>% 
#   filter(floy_tag == "05781" & season == "Fall" 
#          & year %in% c(2019) & rec_group %in% "Monaco Bay")
# 
# 
# bind_rows(
#   lt_clean_1 %>%   
#     filter(event_id %in% c(256219:256223)),
#   lt_clean_1 %>% 
#     filter(event_id %in% c(256219:256223)
#            &  season != lag(season)) %>% 
#     mutate(detection_timestamp_est = season_start, 
#            depart = 0,
#            arrive = 1,
#     )
# )%>% 
#   dplyr::select(floy_tag:event_end) %>% 
#   arrange(floy_tag, name, detection_timestamp_est)
# glimpse(lt_c)
# glimpse(lt)
# 
# lts2 <- lt %>% 
#   group_by(season, year) %>% 
#   mutate(
#     doy = yday(detection_timestamp_est)
#   ) %>% 
#   summarise(length = n_distinct(doy)) %>% 
#   ungroup()
# 
# lts2
# 
# 
# lt_clean_2 %>%   
#   filter(event_id %in% c(265970:265980)) %>% 
#   View()
# 

# lt_clean_1 %>% 
#   filter(event_id %in% seq(2055, 2068, 1))
# 
# 
# 
# lt_clean_1 %>% 
#   dplyr::select(floy_tag,name, detection_timestamp_est, arrive, depart, 
#                 time_spent:time_spent_d)
# 
# lt_clean_1 %>% 
#   filter(time_spent == 0) %>% 
# 
# ggplot(aes(x = detection_timestamp_est, y = factor(name))) +
#   geom_point(aes(colour = floy_tag))


lt_summarize <- lt_time_spent %>% 
  group_by(floy_tag,fish_basin, name, rec_group, rec_basin, season, year, 
           season_length) %>% 
  summarise(
    total_time_h = sum(time_spent_h), 
    total_time_d = sum(time_spent_d)
  ) %>% 
  ungroup() %>% 
  filter(total_time_d < season_length) 



lt_summarize

summary(lt_summarize$total_time_d)



write_rds(lt_clean_1, here("Saved Data", 
                           "arrive_and_depart_rec.rds"))

write_rds(lt_summarize, here("Saved Data", 
                             "time_spent_rec_season.rds"))



ggplot(data = lt_summarize, aes(x = rec_group,
                                # y = season
                                y = total_time_d
)) + 
  geom_jitter(
    size = 3, 
    aes(
      shape = factor(year), 
      # size = total_time_d, 
      colour = fish_basin), 
    width = 0.2) + 
  scale_colour_viridis_d(end = 0.85, name = "Capture Basin") + 
  scale_shape(name = "Year") + 
  facet_wrap(. ~ season, nrow = 2) +
  theme_bw(base_size = 15) + 
  
  theme(panel.grid = element_blank(), 
        # legend.position = c(0.05, 0.93),
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "Receiver Group", 
       y = "Total Time (days)")

ggplot(data = lt_summarize, aes(x = rec_group,
                                # y = season
                                y = total_time_d
)) + 
  geom_jitter(
    size = 3, 
    aes(
      shape = factor(year), 
      # size = total_time_d, 
      colour = fish_basin), 
    width = 0.2) + 
  scale_colour_viridis_d(end = 0.85, name = "Capture Basin") + 
  scale_shape(name = "Year") + 
  facet_wrap(. ~ season, nrow = 2) +
  theme_bw(base_size = 15) + 
  
  theme(panel.grid = element_blank(), 
        # legend.position = c(0.05, 0.93),
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "Receiver Group", 
       y = "Total Time (days)")







lt_sums <- lt_clean_1 %>% 
  group_by(floy_tag, fish_basin, rec_basin, season, year, season_length) %>% 
  summarise(
    total_time_h = sum(time_spent_h), 
    total_time_d = sum(time_spent_d)
  ) %>% 
  ungroup() %>% 
  filter(total_time_d < season_length) 

lt_sums


ggplot(data = lt_sums, aes(x = rec_basin,
                           # y = season
                           y = total_time_d
)) + 
  geom_jitter(
    size = 3, 
    aes(
      shape = factor(year), 
      # size = total_time_d, 
      colour = fish_basin), 
    width = 0.2) + 
  scale_colour_viridis_d(end = 0.85, name = "Capture Basin") + 
  scale_shape(name = "Year") + 
  facet_rep_wrap(. ~ season, nrow = 2) +
  theme_bw(base_size = 15) + 
  
  theme(panel.grid = element_blank(), 
        # legend.position = c(0.05, 0.93),
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "Receiver Group", 
       y = "Total Time (days)")






lt_sum <- lt_clean %>% 
  group_by(floy_tag, name) %>% 
  mutate(time_num = as.numeric(detection_timestamp_utc), 
         time_spent = time_num - lag(time_num, default = first(time_num))) %>% 
  ungroup()



lt_sum


# make arrive and depart long format -----
lt_long <- lt_clean %>%
  select(event_id:detection_timestamp_est, arrive, depart) %>% 
  pivot_longer(!event_id:detection_timestamp_est,
               names_to = "event", values_to = "occured") %>% 
  filter(occured != 0) %>%
  select(-occured)


lt_long

# convert detections into seconds and then use lag to determing time difference
# between arrive and deaprt -----
lt_sum <- lt_long %>% 
  group_by(floy_tag, name) %>% 
  mutate(time_num = as.numeric(detection_timestamp_est), 
         time_spent = time_num - lag(time_num, default = first(time_num))) %>% 
  ungroup() %>% 
  mutate(week = floor_date(detection_timestamp_est, unit = "1 week"), 
         month = month(detection_timestamp_est, label = TRUE), 
         month_num = lubridate::month(detection_timestamp_est), 
         year = year(detection_timestamp_est))


# create timme bins based on duration ------
lt_sums <- lt_sum %>%
  mutate(min = time_spent / 60, 
         hr = min / 60, 
         days = hr / 24, 
         weeks = days / 7)

lt_sums


lt_sum_hr <- lt_sums %>% 
  group_by(floy_tag, name, month, year) %>% 
  summarise(sum_hr = sum(hr)) %>% 
  ungroup() %>% 
  arrange(floy_tag, year, month)



lt_sum_hr
# boxplot time spent per hour regardless of fish per month ----
ggplot(data = lt_sum_hr, aes(x = as.factor(name), y = sum_hr)) + 
  geom_boxplot() + 
  facet_rep_wrap(. ~ month, repeat.tick.labels = TRUE) +
  theme_classic() + 
  labs(x = "Receiver Name", 
       y = "Hours")


ggplot(data = lt_summarize, 
       aes(x = rec_group, y = total_time_d)) + 
  geom_boxplot(aes(fill = fish_basin)) + 
  scale_fill_viridis_d(end = 0.85, name = "Capture Basin") +  
  facet_wrap(. ~ season, nrow = 2) +
  theme_bw(base_size = 15) + 
  
  theme(panel.grid = element_blank(), 
        # legend.position = c(0.05, 0.93),
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "Receiver Group", 
       y = "Total Time (days)")
  


