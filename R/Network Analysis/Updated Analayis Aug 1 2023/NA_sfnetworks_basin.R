# ---- load packages ----

library(data.table)
library(dplyr)
library(forcats)
library(ggplot2)
library(ggraph)
library(ggspatial)
library(here)
library(igraph)
library(lubridate)
library(janitor)
library(network)
library(networkD3)
library(purrr)
library(rgdal)
library(rgeos)
library(readr)
library(sf)
library(stringr)
library(sfnetworks)
library(tibble)
library(tidygraph)
library(visNetwork)


# ---- bring in clean downloaded ----

lt <- read_rds(here::here("Saved data", 
                          "kenauk lake trout 2017 - 2020.rds"))

# ---- Bring in shapefile ------
p_map <- st_read(dsn = here::here("Shapefiles",
                                  "."),
                 layer = "plake_edit_wo_link")
# ---- bring in cost_distance analsysis per recevier group --------
cd <- read_rds(here::here("Saved data", 
                          "cost_distance_per_rec_group_sf.rds"))
# replace any NA value with 0 
cd$cost_dist[is.na(cd$cost_dist)] <- 0


# ---- make receiver groups based on location in the lake -----
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

# plot lt rec groups
lt %>%
  group_by(rec_group_long,
           rec_group_lat) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  ggplot() +
  geom_sf(data = p_map) +
  geom_point(aes(x = rec_group_long,
                 y = rec_group_lat, size = n)) + 
  theme_void()


# bring in receiver locations ------
rl <- read_csv(here::here("Data", 
                          "overall_rec_loc.csv")) %>%
  clean_names()


# add rec group 
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

# 
# # remove error and summarisze mean lat and longs ------
rl_sum <- rl %>%
  filter(!download_group %in% "error") %>%
  group_by(rec_group, basin) %>%
  summarise(lat = mean(lat),
            long = mean(long)) %>%
  ungroup()


# # create rec sf -----

rl_sum_sf <- st_as_sf(rl_sum, coords = c("long", "lat"),
                      crs = st_crs(p_map))




# ---- determine every time rec_group switches  ------
lt <- lt %>% 
  arrange(floy_tag, detection_timestamp_utc) %>% 
  group_by(floy_tag) %>% 
  mutate(change_rec = if_else(rec_group != lag(rec_group) | 
                                is.na(lag(rec_group)), 
                              1, 0)) %>% 
  ungroup()


glimpse(lt)



# ----- create duplicate colummn for destination ----
lt <- lt %>% 
  mutate(rec_dest_group = rec_group, 
         rec_dest_lat = rec_group_lat, 
         rec_dest_long = rec_group_long)

glimpse(lt)



# ---- create where fish are coming from ----
lt <- lt %>% 
  arrange(floy_tag, detection_timestamp_utc) %>% 
  group_by(floy_tag) %>% 
  mutate(rec_source_group = lag(rec_dest_group), 
         rec_source_lat = lag(rec_dest_lat), 
         rec_source_long = lag(rec_dest_long)) %>% 
  ungroup()


glimpse(lt)


# remove the first time its heard which is NA 
lt <- lt %>% 
  filter(!(rec_source_group == is.na(rec_source_group)))

# ---- paste from and to together -----
lt <- lt %>%
  mutate(rec_source_to_dest = paste(rec_source_group, rec_dest_group, sep = "-"))


glimpse(lt)
# ---- create node list based on source and destiatnion -----
sources <- lt %>%
  distinct(rec_source_group) %>%
  rename(label = rec_source_group)

destinations <- lt %>%
  distinct(rec_dest_group) %>%
  rename(label = rec_dest_group)

nodes <- full_join(sources, destinations, by = "label")

nodes <- nodes %>%
  arrange(label) %>% 
  rowid_to_column("id")
nodes

# ---- create edge list -----
glimpse(lt)

# determine the number of times a fish moved between two locations
per_route_id <- lt %>% 
  group_by(floy_tag, fish_basin, receiver_basin, 
           rec_source_group, rec_source_lat, 
           rec_source_long,
           rec_dest_group, 
           rec_dest_lat, rec_dest_long, 
           # month, # can turn off mon, yr, if need be
           season, 
           # year
  ) %>% 
  summarise(weight = n()) %>% 
  ungroup() %>% 
  arrange(weight, rec_source_group, floy_tag)

glimpse(per_route_id)
# create where the fish went from 
edges <- per_route_id %>% 
  left_join(nodes, by = c("rec_source_group" = "label")) %>% 
  rename(from = id)

# create where the fish went to 
edges <- edges %>% 
  left_join(nodes, by = c("rec_dest_group" = "label")) %>% 
  rename(to = id) %>% 
  arrange(floy_tag)

# make from and to to intergerges 
edges <- edges %>% 
  mutate(from = as.integer(from), 
         to = as.integer(to))
edges

# paste together to get from and to locations 
edges <- edges %>% 
  mutate(from_to = paste(rec_source_group, rec_dest_group, sep = "-"))


glimpse(edges)
glimpse(cd)
edges

# ---- prep cost distance sf object to be joined to edge list -----
# remove id - to in cd 
# cds <- cd %>% 
#   select(-id:-to)



# ---- join cost distance to edges -----
edges_sf <- edges %>% 
  left_join(cd, by = "from_to")


glimpse(edges_sf) 

edges_sf <- edges_sf %>% 
  rename(from = from.x, 
         to = to.x, 
         from_nam = from.y, 
         to_nam = to.y)

# make into sf object 
edges_sf <- st_as_sf(edges_sf, sf_column_name = "geometry")

glimpse(edges_sf)

# 
# write_rds(edges_sf, here("Saved Data", 
#                          "lkt_edges_sf.rds"))

# ---- create node sf object ---- 
nodes_sf <- lt %>% 
  group_by(rec_group, receiver_basin, ) %>% 
  summarise(rec_group_lat = unique(rec_group_lat),
            rec_group_long = unique(rec_group_long)) %>% 
  ungroup() %>% 
  arrange(rec_group) %>% 
  st_as_sf(coords = c("rec_group_long", 
                      "rec_group_lat"), 
           crs = st_crs(p_map)) 

nodes_sf
# correct to edge sf crs which is in utms 
nodes_sf <- st_transform(rl_sum_sf, crs = st_crs(edges_sf)) %>% 
  rowid_to_column("id")


# pdf(here::here("Fish and tagging data",
#                "Receiver Downloads",
#                "network analysis",
#                "individual_season_networks_w_all_rec_groups.pdf"),
#     width = 17.5, height = 17.5)

# ---- test sfnetworks for one fish -----
# grab edges for one fish 
glimpse(edges)

df <- edges_sf %>% 
  filter(floy_tag %in% "05550") %>% 
  select(floy_tag:rec_source_group, 
         rec_dest_group, season, weight, from,
         to, from_to, cost_dist, geometry)
# mutate(
#   from_to_n = paste(from, to, sep = "-")
# )

# grab nodes for one fish 
df_source <- df %>% 
  st_drop_geometry() %>% 
  reframe(id = unique(from), 
          label = unique(rec_source_group))  %>% 
  arrange(label)

df_dest <- df %>% 
  st_drop_geometry() %>%  
  reframe(id = unique(to), 
          label = unique(rec_dest_group)) %>% 
  arrange(label)

df_source
df_dest
# joing source and destination 
df_nodes <- full_join(df_source, df_dest, by = c("id", "label"))

# grab nodes from node_sf 
df_nodes_sf <- nodes_sf %>% 
  filter(rec_group %in% df_nodes$label) %>% 
  select(id, rec_group, geometry) %>% 
  mutate(
    id = as.character(id)
  )
df_nodes_sf

# ---- use sfnetworks to create network ----
test_network <- sfnetwork(nodes = df_nodes_sf, edges = df, 
                          directed = TRUE, 
                          edges_as_lines = TRUE, 
                          node_key = "id",
                          force = TRUE)
