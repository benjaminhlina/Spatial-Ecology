
# load packages ----

library(data.table)
library(dplyr)
library(forcats)
library(ggplot2)
library(ggraph)
library(here)
library(igraph)
library(lubridate)
library(janitor)
library(network)
library(networkD3)
library(purrr)
library(rgdal)
library(rgeos)
library(RSP)
library(readr)
library(sf)
library(sp)
library(stringr)
library(sfnetworks)
library(tibble)
library(tidygraph)
library(visNetwork)


# bring in clean downloaded ----

lt <- read_rds(here::here("Saved data", 
                          "kenauk lake trout 2017 - 2020.rds"))

# bring in cost_distance analsysis per recevier group --------
cd <- read_rds(here::here("Saved data", 
                          "cost_distance_per_rec_group_sf.rds"))


# convert to tibble ----
# cd <- cd %>% 
#   st_drop_geometry() %>% 
#   as_tibble()
# 
# # replace any NA value with 0 
# cd$cost_dist[is.na(cd$cost_dist)] <- 0



rl <- read_csv(here::here("Papineau reciever locations and maps", 
                          "Recevier Locations", 
                          "overall_rec_loc.csv")) %>%
  clean_names()


rl <- rl %>% 
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


rl_sum <- rl %>% 
  filter(!download_group %in% "error") %>% 
  group_by(rec_group, basin) %>% 
  summarise(lat = mean(lat), 
            long = mean(long)) %>% 
  ungroup()






p_map <- st_read(dsn = here::here("Papineau reciever locations and maps",
                                  "Papineau Lake Shapefile",
                                  "."),
                 layer = "lake")


p_map_spd <- as_Spatial(p_map)




# p_map <-readOGR(dsn = here::here("Papineau reciever locations and maps",
#                                   "Papineau Lake Shapefile",
#                                   "."),
#                  layer = "lake")




rl_sum_sf <- st_as_sf(rl_sum, coords = c("long", "lat"), 
                      crs = st_crs(p_map))


ggplot() + 
  geom_sf(data = p_map) + 
  geom_sf(data = rl_sum_sf)




# add species column to each dataframe -----

lt$species <- "lt"

glimpse(lt)


lt <- lt %>% 
  filter(passed_filter == 1)

# lts <- lt %>%
#   filter(sensor_unit %in% c("m/s²", "m", NA) & passed_filter == 1)




# make receiver groups based on location in the lake -----
lt <- lt %>%
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



lt <- lt %>% 
  group_by(rec_group) %>% 
  mutate(rec_group_lat = mean(lat_mean),
         rec_group_long = mean(long_mean)) %>% 
  ungroup()

# lt %>% 
#   group_by(rec_group_long, 
#            rec_group_lat) %>%
#   summarise(n = n()) %>% 
#   ggplot() + 
#   geom_sf(data = p_map) + 
#   geom_point(data = dfs, aes(x = rec_group_long, 
#                              y = rec_group_lat, size = n)) 
# 
# 
# 


rl_sum_sf <- lt %>% 
  group_by(rec_group, receiver_basin, ) %>% 
  summarise(rec_group_lat = unique(rec_group_lat),
            rec_group_long = unique(rec_group_long)) %>% 
  ungroup() %>% 
  st_as_sf(coords = c("rec_group_long", 
                      "rec_group_lat"), 
           crs = st_crs(p_map)) 




# determine everytime letter_name switches ------
lt <- lt %>% 
  arrange(floy_tag, detection_timestamp_utc) %>% 
  group_by(floy_tag) %>% 
  mutate(change_rec = if_else(rec_group != lag(rec_group) | 
                                is.na(lag(rec_group)), 
                              1, 0)) %>% 
  ungroup()


glimpse(lt)



# ls <- lt %>% 
#   dplyr::select(floy_tag, detection_timestamp_utc, name, change_rec)

# View(ls)

# create duplicate colum for destiation 
lt <- lt %>% 
  mutate(rec_dest_group = rec_group, 
         rec_dest_lat = rec_group_lat, 
         rec_dest_long = rec_group_long)

glimpse(lt)



# create where fish are coming from 
lt <- lt %>% 
  arrange(floy_tag, detection_timestamp_utc) %>% 
  group_by(floy_tag) %>% 
  mutate(rec_source_group = lag(rec_dest_group), 
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
  filter(!(rec_source_group == is.na(rec_source_group)))


lt <- lt %>%
  mutate(rec_source_to_dest = paste(rec_source_group, rec_dest_group, sep = "-"))


glimpse(lt)


# create node list ------
sources <- lt %>%
  distinct(rec_source_group, 
           # rec_source_lat, 
           # rec_source_long
  ) %>%
  rename(label = rec_source_group)

destinations <- lt %>%
  distinct(rec_dest_group, 
           # rec_dest_lat, 
           # rec_dest_long
  ) %>%
  rename(label = rec_dest_group)

nodes <- full_join(sources, destinations, by = "label")
nodes

nodes <- nodes %>%
  rowid_to_column("id")
nodes






# nodes <- tibble(
#   id = as.integer(unique(lt$name_order)),
#   label = as.character(unique(lt$letter_name))
# ) %>%
#   arrange(id)



# create edge list -----
glimpse(lt)

per_route_id <- lt %>% 
  group_by(floy_tag, fish_basin, receiver_basin, 
           rec_source_group, rec_source_lat, 
           rec_source_long,
           rec_dest_group, 
           rec_dest_lat, rec_dest_long, 
           season) %>% 
  summarise(weight = n()) %>% 
  ungroup() %>% 
  arrange(weight, rec_source_group, floy_tag)




glimpse(edges)

edges <- per_route_id %>% 
  left_join(nodes, by = c("rec_source_group" = "label")) %>% 
  rename(from = id)


edges <- edges %>% 
  left_join(nodes, by = c("rec_dest_group" = "label")) %>% 
  rename(to = id) %>% 
  arrange(floy_tag)


edges <- edges %>% 
  mutate(from = as.integer(from), 
         to = as.integer(to))
edges