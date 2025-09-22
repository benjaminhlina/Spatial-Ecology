
# load packages ----

library(data.table)
library(dplyr)
library(forcats)
library(fitdistrplus)
library(glatos)
library(ggplot2)
library(here)
library(lubridate)
library(janitor)
library(lme4)
library(network)
library(rgdal)
library(sf)
library(sp)
library(readr)
library(stringr)
library(tibble)
library(igraph)
library(tidygraph)
library(purrr)
library(tidyr)
library(ggraph)
library(visNetwork)
library(networkD3)

# bring in clean downloaded ----

lt <- read_rds(here::here("Saved data", 
                          "kenauk lake trout 2017 - 2020.rds"))


# remove temp double detections from 
lt <- lt %>% 
  filter(sensor_unit %in% c("m", "°C", NA))

# bring in shapefile ------
p_map <- st_read(dsn = here::here("Papineau reciever locations and maps",
                                  "Papineau Lake Shapefile",
                                  "."),
                 layer = "lake")
# bring in receiver locations ------
rl <- read_csv(here::here("Papineau reciever locations and maps", 
                          "Recevier Locations", 
                          "overall_rec_loc.csv")) %>%
  clean_names()


# add in receiver groupings  per receiver 
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


# remove donwload group an dcreate mean lat and long per receiver 
rl_sum <- rl %>% 
  filter(!download_group %in% "error") %>% 
  group_by(rec_group, basin) %>% 
  summarise(lat = mean(lat), 
            long = mean(long)) %>% 
  ungroup()



# bring in cost_distance analsysis per recevier group --------
cd <- read_rds(here::here("Saved data", 
                          "cost_distance_per_rec_group_sf.rds"))


# convert to tibble ----
cd <- cd %>% 
  st_drop_geometry() %>% 
  as_tibble()

# replace any NA value with 0 
cd$cost_dist[is.na(cd$cost_dist)] <- 0






# create sf object for receiver locations ------
rl_sum_sf <- st_as_sf(rl_sum, coords = c("long", "lat"), 
                      crs = st_crs(p_map))


ggplot() + 
  geom_sf(data = p_map) + 
  geom_sf(data = rl_sum_sf)


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

# create duplicate colum for destination 
lt <- lt %>% 
  mutate(rec_dest_group = rec_group)

glimpse(lt)


# create where fish are coming from 
lt <- lt %>% 
  arrange(floy_tag, detection_timestamp_utc) %>% 
  group_by(floy_tag) %>% 
  mutate(rec_source_group = lag(rec_dest_group)) %>% 
  ungroup()

# ts <- lt_switch %>%
#   group_by(rec_source_ln) %>%
#   summarise(n = n()) %>%
#   ungroup()
# 
# View(ts)


# remove the first time its heard which is NA
lt <- lt %>% 
  filter(!(rec_source_group == is.na(rec_source_group)))

# paste to and from toegether 
lt <- lt %>%
  mutate(rec_source_to_dest = paste(rec_source_group, rec_dest_group, sep = "-"))


glimpse(lt)



lts <- lt %>% 
  select(date, detection_timestamp_utc, floy_tag, 
         sensor_value, sensor_unit, name, rec_group, fish_basin, 
         season, rec_group, change_rec, rec_dest_group, 
         rec_source_group, rec_source_to_dest)



# create node list ------
sources <- lt %>%
  distinct(rec_source_group) %>%
  rename(label = rec_source_group)

destinations <- lt %>%
  distinct(rec_dest_group) %>%
  rename(label = rec_dest_group)

nodes <- full_join(sources, destinations, by = "label")
nodes

nodes <- nodes %>%
  rowid_to_column("id")
nodes

# reorder nodes to appear from west to east, north to south -----
nodes <- nodes %>% 
  mutate(label = factor(label, levels = c("Sucker Creek",
                                          "Monaco Bay",
                                          "Central West-Basin",
                                          "North West-Basin",
                                          "Hidden Bay",         
                                          "Central North-Basin",
                                          "North North-Basin",
                                          "Black Bay",
                                          "North East-Basin",
                                          "Central East-Basin",
                                          "South East-Basin")))






# nodes <- tibble(
#   id = as.integer(unique(lt$name_order)),
#   label = as.character(unique(lt$letter_name))
# ) %>%
#   arrange(id)



# create edge list -----

# seasonal edges list 
per_route_id <- lt %>% 
  group_by(floy_tag, fish_basin, receiver_basin, 
           rec_source_group, rec_dest_group, season) %>% 
  summarise(weight = n()) %>% 
  ungroup() %>% 
  arrange(weight, rec_source_group, floy_tag)


# day night edge list 
per_route_id_dn <- lt %>% 
  group_by(floy_tag, fish_basin, receiver_basin, 
           rec_source_group, rec_dest_group, day_night, season) %>% 
  summarise(weight = n()) %>% 
  ungroup() %>% 
  arrange(weight, rec_source_group, floy_tag)


# create to cobined column of to and from 
per_route_id <- per_route_id %>%
  mutate(from_to = paste(rec_source_group, rec_dest_group, sep = "-"))
# dplyr::select(from_to, weight)



# join nodes and per_route_id -----
# this creates nummerical colum from 
edges <- per_route_id %>% 
  left_join(nodes, by = c("rec_source_group" = "label")) %>% 
  rename(from = id)


# creates numerical column to 
edges <- edges %>% 
  left_join(nodes, by = c("rec_dest_group" = "label")) %>% 
  rename(to = id) %>% 
  arrange(floy_tag)


# m <- edges %>% 
#   group_by(weight) %>% 
#   summarise(n = n())
# 
# 
# m
# edges <- edges %>% 
#   dplyr::select(floy_tag, fish_basin, rec_source_group, rec_dest_group,
#                 from, to, weight) %>% 
#   arrange(floy_tag)

edges


# remove id - to in cd 
cd <- cd %>% 
  select(-id:-to)

# join cost distance to edges ------
edges <- edges %>% 
  left_join(cd, by = "from_to")


glimpse(edges)

# view histogram 
ggplot(data = edges, aes(x = weight)) + 
  geom_histogram() 



sum_dist <- edges %>% 
  group_by(floy_tag, from_to, season) %>% 
  summarise(total_dist = (cost_dist * weight) / 1000) %>% 
  ungroup()

sum_dist




sum_dist_avg <- sum_dist %>% 
  group_by(from_to, season) %>% 
  summarise(avg_dist = mean(total_dist), 
            std = sd(total_dist), 
            sem = sd(total_dist) / sqrt(n())) %>% 
  ungroup()



# sum_dist_0 <- sum_dist %>% 
#   filter(total_dist > 0)


ggplot(data = sum_dist, aes(x = total_dist)) + 
  geom_histogram() + 
  facet_grid(. ~ season)


sum_dist_small <- sum_dist %>% 
  filter(total_dist < 750)


ggplot(data = sum_dist_small, aes(x = from_to, y = total_dist, group = from_to)) + 
  stat_summary(geom = "errorbar",
               aes(group = from_to), colour = "black", width = 0.15, 
               fun.data = mean_se) +
  stat_summary(geom = "point", 
               aes(group = from_to), colour = "blue", 
               fun = mean,
               size = 3) +
  # geom_line(aes(group = floy_tag), colour = "black",
  #           size = 1) +
  facet_wrap(.~ season, nrow = 1) + 
  theme(axis.text.x = element_text(angle = 90))

# -------------------------------------------------------------------------------
# create networks for both season and fish -----



networks <- edges %>% 
  group_by(floy_tag, season) %>% 
  nest() %>% 
  rename(edges = data) %>% 
  mutate(
    graph = map(edges, 
                ~tbl_graph(edges = ., 
                           nodes = nodes, directed = TRUE)
    )
  )


networks$graph[[2]]

networks$graph

