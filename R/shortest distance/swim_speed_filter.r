
# load packages ----

library(dplyr)
library(ggplot2)
library(ggraph)
library(gdistance)
library(here)
library(lubridate)
library(igraph)
library(network)
library(networkD3)
library(purrr)
library(rgdal)
library(raster)
library(rgeos)
library(readr)
library(sf)
library(sp)
library(tibble)
library(tidygraph)
library(tidyr)
library(visNetwork)


# bring in clean downloaded ----

cd <- read_rds(here::here("Saved data", 
                          "cost_distance_per_rec_sf.rds"))

lt <- read_rds(here::here("Saved data", 
                          "kenauk lake trout 2017 - 2020.rds"))


lt <- lt %>% 
  filter(sensor_unit %in% c("m", "°C", NA))

cd <- cd %>% 
  st_drop_geometry() %>% 
  as_tibble()



cd


cd$cost_dist[is.na(cd$cost_dist)] <- 0


# cd1 <- cd
# cd1 <- cd1[!duplicated(cd$cost_dist), ]


#stewart et al. 1983 and cruz-font et al. 2019 Fekdnab abd savutz 
# higbs abd niffut 2004# consevative esitalmes 
cd <- cd %>% 
  mutate(time_to_dest_s = (cost_dist / 0.8),
        time_to_dest_min = ((cost_dist / 0.8) / 60))


unique(lt$sensor_unit)


# lt_filter <- lt %>% 
  # filter()


glimpse(lt)

lt <- lt %>% 
  arrange(floy_tag, detection_timestamp_utc) %>% 
  group_by(floy_tag) %>% 
  mutate(change_rec = if_else(name != lag(name) | 
                                is.na(lag(name)), 
                              1, 0)) %>% 
  ungroup()


glimpse(lt)
lt <- lt %>% 
  mutate(rec_dest = name, 
         rec_dest_lat = lat_mean, 
         rec_dest_long = long_mean)

glimpse(lt)



# create where fish are coming from 
lt <- lt %>% 
  arrange(floy_tag, detection_timestamp_utc) %>% 
  group_by(floy_tag) %>% 
  mutate(rec_source = lag(rec_dest), 
         rec_source_lat = lag(rec_dest_lat), 
         rec_source_long = lag(rec_dest_long)) %>% 
  ungroup()

# ts <- lt_switch %>%
#   group_by(rec_source_ln) %>%
#   summarise(n = n()) %>%
#   ungroup()
# 
# View(ts)

glimpse(lt)


# remove the first time its heard which is NA
lt <- lt %>% 
  filter(!(rec_source == is.na(rec_source)))


lt <- lt %>%
  mutate(rec_source_to_dest = paste(rec_source, rec_dest, sep = "-"))



glimpse(lt)




lt <- lt %>%
  group_by(floy_tag) %>% 
  mutate(diftime = as.numeric(difftime(detection_timestamp_utc, 
                            lag(detection_timestamp_utc)))) %>% 
  ungroup()



glimpse(lt)
lt <- lt %>% 
  filter(!(diftime == is.na(diftime)))




# lt_s <- lt %>% 
#   filter(dates == ymd("2019-07-12"))

# View(lt_s)


lt_s <- lt %>% 
  dplyr::select(floy_tag, detection_timestamp_utc, name, rec_source_to_dest, diftime)



glimpse(lt_s)
View(lt_s)
lt_filter_ <- lt_s %>%
  group_by(floy_tag) %>%
  filter(case_when(rec_source_to_dest %in% cd$from_to ~ 
                     diftime < cd$time_to_dest_s,
                   TRUE ~ diftime < cd$time_to_dest_s
                   )) %>% 
  ungroup()



lt_filter_error <- lt_s %>%
  group_by(floy_tag) %>%
  filter(case_when(rec_source_to_dest %in% cd$from_to ~ 
                     diftime > cd$time_to_dest_s,
                   TRUE ~ diftime > cd$time_to_dest_s
  )) %>% 
  ungroup() %>% 
  arrange(floy_tag, rec_source_to_dest)







glimpse(lt_filter_error)

glimpse(lt_filter)
unique(lt_filter$rec_source_to_dest)


m <- lt_filter %>% 
  group_by(rec_source_to_dest, diftime) %>% 
  summarise(n = n_distinct(diftime))



View(m)


nrow(lt) - nrow(lt_filter)


rec_sum <- lt %>% 
  group_by(floy_tag, rec_source_to_dest, day_night) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  rename(from_to = rec_source_to_dest)


rec_sum



rec_sum <- rec_sum %>% 
  left_join(cd, by = "from_to")


rec_sum <- rec_sum %>% 
  select(floy_tag:n, cost_dist:time_to_dest_min)

rec_sum <- rec_sum %>% 
  mutate(dis_trav = n * cost_dist)


rec_sum


ggplot(data = rec_sum, aes(x = dis_trav)) + 
  geom_histogram() + 
  facet_wrap(.~ day_night)
