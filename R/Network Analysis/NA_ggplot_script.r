
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

# Bring in shapefile ------
p_map <- st_read(dsn = here::here("Papineau reciever locations and maps",
                                  "Papineau Lake Shapefile",
                                  "."),
                 layer = "plake_edit_wo_link")
# bring in cost_distance analsysis per recevier group --------
cd <- read_rds(here::here("Saved data", 
                          "cost_distance_per_rec_group_sf.rds"))


# convert to tibble ----
cd <- cd %>% 
  st_drop_geometry() %>% 
  as_tibble()

# replace any NA value with 0 
cd$cost_dist[is.na(cd$cost_dist)] <- 0


# bring in receiver locations ------
rl <- read_csv(here::here("Papineau reciever locations and maps", 
                          "Recevier Locations", 
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


# remove error and summarisze mean lat and longs ------
rl_sum <- rl %>% 
  filter(!download_group %in% "error") %>% 
  group_by(rec_group, basin) %>% 
  summarise(lat = mean(lat), 
            long = mean(long)) %>% 
  ungroup()


# create rec sf -----

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
                 y = rec_group_lat, size = n))




# replace rl_sum_sf as as lt coords are more accurate ------
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



# create duplicate colum for destination 
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

edges <- edges %>% 
  mutate(from_to = paste(rec_source_group, rec_dest_group, sep = "-"))


glimpse(edges)
glimpse(cd)


# remove id - to in cd 
cds <- cd %>% 
  select(-id:-to)


# join cost distance to edges ------
edges <- edges %>% 
  left_join(cd, by = "from_to")








make_line <- function(lon1, lat1, lon2, lat2) {
  st_linestring(matrix(c(lon1, lon2, lat1, lat2), 2, 2))
}


edges <- edges %>% 
  rename(lon1 = rec_source_long, 
         lat1 = rec_source_lat, 
         lon2 = rec_dest_long, 
         lat2 = rec_dest_lat)


rl_sum_sf$geometry
d <- edges %>% 
  group_by(rec_source_group, receiver_basin, season) %>% 
  summarise(n = n()) %>% 
  ungroup()







rl_sum_sf <- lt %>% 
  group_by(rec_group, receiver_basin, ) %>% 
  summarise(rec_group_lat = unique(rec_group_lat),
            rec_group_long = unique(rec_group_long)) %>% 
  ungroup() %>% 
  st_as_sf(coords = c("rec_group_long", 
                      "rec_group_lat"), 
           crs = st_crs(p_map)) 


ggplot()


edges_sf <- edges %>% 
  dplyr::select(lon1, lat1, 
                lon2, lat2) %>% 
  pmap(make_line) %>% 
  st_as_sfc(crs = st_crs(p_map)) %>% 
  {tibble(id = edges$floy_tag,
          season = edges$season,
          fish_basin = edges$fish_basin, 
          rec_source = edges$rec_source_group, 
          rec_dest = edges$rec_dest_group, 
          from = edges$from, 
          to = edges$to, 
          weight = edges$weight, 
          from_to = edges$from_to, 
          geometry = .)} %>% 
  st_sf()



rl_sum <- rl_sum %>% 
  arrange(basin)


rl_sum

edges_sl <- edges %>% 
  filter(from == to) 


edges_sl <- edges_sl %>% 
  mutate(receiver_basin = case_when(rec_source_group %in% c("North East-Basin", 
                                                            "Central East-Basin", 
                                                            "Black Bay", 
                                                            "South East-Basin") ~ "East Basin", 
                                    rec_source_group %in% c("North West-Basin", 
                                                            "Sucker Creek", 
                                                            "Central West-Basin", 
                                                            "Monaco Bay") ~ "West Basin", 
                                    rec_source_group %in% c("North North-Basin", 
                                                            "Central North-Basin", 
                                                            "Hidden Bay") ~ "North Basin", 
                                    # TRUE ~ TRUE
  )) 




edges_sl_sf <- st_as_sf(edges_sl, coords = c("lon1", 
                                             "lat1"), 
                        crs = st_crs(p_map)) 
# {tibble(id = edges$floy_tag,
#         season = edges$season,
#         fish_basin = edges$fish_basin, 
#         rec_source = edges$rec_source_group, 
#         rec_dest = edges$rec_dest_group, 
#         from = edges$from, 
#         to = edges$to, 
#         weight = edges$weight, 
#         from_to = edges$from_to, 
#         geometry = .)} %>% 
# st_sf()




# rl_sum_lt <- lt %>% 
#   group_by(rec_group, receiver_basin) %>% 
#   summarise(rec_group_lat = unique(rec_group_lat), 
#             rec_group_long = unique(rec_group_long)) %>% 
#   ungroup()



dev.off()
fish_id <- unique(edges_sf$id)

# pdf(here::here("Fish and tagging data",
#                "Receiver Downloads",
#                "network analysis",
#                "individual_season_networks_w_all_rec_groups.pdf"),
#     width = 17.5, height = 17.5)



dfs <- edges_sf %>% 
  filter(id %in% "05550" & 
           season %in% "Spring")



dfs

glimpse(dfs)





source <- dfs %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  distinct(rec_source) %>% 
  rename(label = rec_source)

ss <- rl_sum_sf %>% 
  filter(rec_group %in% source$label)

m <- sfnetwork(nodes = ss, edges = dfs, directed = TRUE, 
               edges_as_lines = TRUE, node_key = "rec_group",
               force = TRUE)



m <- m %>%
  activate("nodes") %>%
  mutate(bc = centrality_betweenness()) %>% 
  activate("edges") %>% 
  mutate(ebc = centrality_edge_betweenness())



s <- m %>%
  activate("nodes") %>%
  as_tibble()

glimpse(dfs)


p_map <- st_transform(p_map, crs = st_crs(edges_sf))

ggplot() +
  geom_sf(data = p_map) +
  geom_sf(data = st_as_sf(m, "edges"), aes(colour = ebc), 
          size = 1.1) +
  geom_sf(data = st_as_sf(m, "nodes"), aes(size = bc)) +
  # geom_sf(data = rl_sum_sf, aes(colour = receiver_basin), 
  # #         size = 4) + 
  # geom_sf(data =  dat_sl, aes(size = weight, 
  #                             colour = receiver_basin)) + 
  # geom_point(data = rl_sum_lt, aes(x = rec_group_long, 
  #                              y = rec_group_lat, 
  #                              colour = receiver_basin),
  #            size = 4) + 
  
  # geom_sf(data = rl_sum_sf, aes(colour = basin),
  #         size = 4) + 
scale_colour_viridis_c(begin = 0.15, end = 0.75, option = "B") +
  scale_size_continuous(range = c(4, 10), 
                        name = "Betweeness") +
  coord_sf() + 
  
  # facet_wrap(. ~ season, nrow = 1) + 
  theme_bw(base_size = 14) + 
  theme(panel.grid.major = element_blank()) + 
  labs(x = "Longitude", 
       y = "Latitude")

# dfs <- edges_sf %>% 
#   group_by(id, season) %>% 
#   tidyr::nest() %>% 
#   rename(edges = data) %>% 
#   mutate(graph = map(edges, 
#                      ~sfnetwork(nodes = rl_sum_sf, edges = ., directed = TRUE))))







for (i in 1:length(fish_id)) { 
  dat_edges <- edges_sf %>% 
    filter(id %in% fish_id[i])
  
  dat_sl <- edges_sl_sf %>% 
    filter(floy_tag %in% fish_id[i]) 
  
  p <- ggplot() +
    geom_sf(data = p_map) + 
    geom_sf(data = dat_edges, aes(size = weight)) +
    # geom_sf(data = rl_sum_sf, aes(colour = receiver_basin), 
    #         size = 4) + 
    geom_sf(data =  dat_sl, aes(size = weight, 
                                colour = receiver_basin)) + 
    # geom_point(data = rl_sum_lt, aes(x = rec_group_long, 
    #                              y = rec_group_lat, 
    #                              colour = receiver_basin),
    #            size = 4) + 
    
    # geom_sf(data = rl_sum_sf, aes(colour = basin),
    #         size = 4) + 
    scale_colour_viridis_d(begin = 0.15, end = 0.75, option = "B", name = "Basin") +
    scale_size_continuous(range = c(4, 10), 
                          name = "Betweeness") + 
    
    facet_wrap(. ~ season, nrow = 1) + 
    theme_bw(base_size = 14) + 
    theme(panel.grid.major = element_blank()) + 
    labs(x = "Longitude", 
         y = "Latitude", 
         title = paste(unique(dat_edges$id)))
  
  print(p)
  
}

dev.off()


# p_map
# 
# rl_sum_sf









# pdf(here::here("Fish and tagging data", 
#                "Receiver Downloads", 
#                "network analysis",
#                "test.pdf"), 
#     width = 17.5, height = 17.5)
# start loop to generate networks per individaul -------
edges_sf <- edges_sf %>% 
  mutate(edgeID = c(1:n()))

nodes_sf <- edges_sf %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(edgeID = L1) %>%
  group_by(edgeID) %>%
  slice(c(1, n())) %>%
  ungroup() %>%
  mutate(start_end = rep(c('start', 'end'), times = n()/2))

nodes_sf


nodes_sf <- nodes_sf %>%
  mutate(xy = paste(.$X, .$Y)) %>% 
  mutate(nodeID = tidygraph::group_indices(., factor(xy, 
                                                     levels = unique(xy)))) %>%
  dplyr::select(-xy)


source_nodes <- nodes_sf %>%
  filter(start_end == 'start') %>%
  pull(nodeID)

target_nodes <- nodes_sf %>%
  filter(start_end == 'end') %>%
  pull(nodeID)

edges_sfa <- edges_sf %>%
  mutate(from = source_nodes, to = target_nodes)

edges_sfa

graph <- tbl_graph(nodes = nodes_sf, edges = as_tibble(edges_sfa), directed = TRUE)

graph

graph <- graph %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree()) %>%
  mutate(betweenness = centrality_betweenness(directed = TRUE, weights = weight)) %>%
  activate(edges) %>%
  mutate(betweenness = centrality_edge_betweenness(directed = TRUE, weights = weight))

graph



pdf(here::here("Fish and tagging data",
               "Receiver Downloads",
               "network analysis",
               "test3.pdf"),
    width = 17.5, height = 17.5)


for (i in 1:length(fish_id)) { 
  dat_edges_sf <- graph %>%
    activate(edges) %>%
    as_tibble() %>%
    st_as_sf() %>% 
    filter(id %in% fish_id[i])
  
  # dat_sl <- edges_sl_sf %>% 
  #   filter(floy_tag %in% fish_id[i]) 
  #   
  #   
  dat_nodes_sf <- graph %>%
    activate(nodes) %>%
    as_tibble() %>%
    st_as_sf(coords = c("X", "Y"), crs = st_crs(p_map))
  
  
  p <- ggplot() +
    geom_sf(data = p_map) + 
    geom_sf(data = dat_edges_sf, aes(colour = betweenness, size = betweenness)) +
    
    geom_sf(data = dat_nodes_sf, 
            size = 4) + 
    # geom_sf(data =  dat_sl, aes(size = weight, 
    #                             colour = receiver_basin)) + 
    # geom_point(data = rl_sum_lt, aes(x = rec_group_long, 
    #                              y = rec_group_lat, 
    #                              colour = receiver_basin),
    #            size = 4) + 
    
    scale_colour_viridis_c(option = 'inferno') +
    scale_size_continuous(range = c(0, 4)) +
    # scale_colour_viridis_d(begin = 0.15, end = 0.75, option = "B", name = "Basin") +
    # scale_size_continuous(range = c(4, 10), 
    # name = "Betweeness") + 
    
    facet_wrap(. ~ season) + 
    theme_bw(base_size = 14) + 
    theme(panel.grid.major = element_blank()) + 
    labs(x = "Longitude", 
         y = "Latitude", 
         title = paste(unique(dat_edges$id)))
  
  print(p)
  
}

dev.off()
