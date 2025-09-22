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

# convert from and to to hacaracters 
df <- df %>%
  mutate_at(vars(from, to), as.character)

df
test <- df %>%
  mutate(S = map2_chr(from, to,
                      ~str_flatten(sort(c(.x,.y)))))


t1 <- test %>%
  st_drop_geometry() %>%
  
  dplyr::select(S) %>%
  distinct() %>%
  mutate(pair_id = 1:n() )

t1
t2 <- left_join(test, t1, by = "S") %>%
  arrange(from_to)

t2 %>%
  print( n = 53)

# myDf[!duplicated(t(apply(myDf, 1, sort))),]

# ---- use sfnetworks to create network ----
test_network <- sfnetwork(nodes = df_nodes_sf, edges = df, 
                          directed = TRUE, 
                          edges_as_lines = TRUE, 
                          node_key = "id",
                          force = TRUE)



node <- 3

from_x <- test_network %>% 
  igraph::shortest_paths(from = node, to = igraph::V(test_network), 
                         mode = "in")
from_x
net <- test_network %>% 
  activate("edges") %>%
  mutate(weight = edge_length())

paths = st_network_paths(net, 
                         from = c(1:4), 
                         to = c(2, 3, 4), 
                         weights = "weight")
paths %>% 
  unlist()
paths %>%
  slice(1) %>%
  pull(node_paths) %>%
  unlist()

paths %>%
  slice(1) %>%
  pull(edge_paths) %>%
  unlist()


plot_path = function(node_path) {
  net %>%
    activate("nodes") %>%
    slice(node_path) %>%
    plot(cex = 1.5, lwd = 1.5, add = TRUE)
}


colors = sf.colors(3, categorical = TRUE)

plot(net, col = "grey")
paths %>%
  pull(node_paths) %>%
  walk(plot_path)
net %>%
  activate("nodes") %>%
  st_as_sf() %>%
  slice(c(1, 2, 3)) %>%
  plot(col = colors, pch = 8, cex = 2, lwd = 2, add = TRUE)

#' select upstream nodes from spatial network
up.ss <- rep(FALSE, nrow(st_as_sf(test_network, "nodes")))
# up.ss[x] <- TRUE

n.ss.up <- test_network |>
  activate("nodes") |>
  filter(up.ss)

plot_grid(autoplot(test_network), autoplot(n.ss.up))

#' plot upstream nodes
plot_grid(autoplot(n.ss), autoplot(n.ss.up))
test_network[[4]]
str(test_network)
library(DiagrammeR)
as_id
attr(E(test_network), "agr")
plot(test_network, vertex.label= V(test_network)$name)
test_network %>% 
  activate("nodes") %>% 
  as_ids()
get_edge_


which_multiple()
test_network %>% 
  activate("edges") %>% 
  mutate(
    from_direct = which_mutual(.) * 0.1
  ) %>%  
  st_as_sf("edges") %>% 
  print(n = 53)

with_graph(test_network, graph_is_directed())
glimpse(test_network)

mutual_edges <- lapply(V(gr), function(x) which_mutual(gr, es = E(gr)[from(x) | to(x)]))
df <- data.frame(Vertex= names(mutual_edges), 
                 Edges=unlist(lapply(V(gr), function(x) length(E(gr)[from(x)]) )), 
                 no_mutual=unlist(lapply(mutual_edges, function(x) sum(x)/2)))
# 
# admat2 <- as_adjacency_matrix(test_network, sparse = FALSE)
# admat2 <- admat2 * t(admat2)
# g.likes2 <- graph_from_adjacency_matrix(admat2, mode = 'directed')
plot(g.likes2)
# ---- working with test network ----
test_network <- test_network %>%
  activate("nodes") %>%
  # morph(to_split, season) %>%
  mutate(bc = centrality_betweenness(directed = TRUE,
                                     weights = weight,
                                     normalized = TRUE),
         sg = centrality_subgraph(loops = TRUE), 
         deg = centrality_degree(
           weights = weight,
           loops = TRUE, 
           normalized = TRUE, mode = "all")
  ) %>% 
  
  activate("edges") %>% 
  
  morph(to_split, season) %>%
  mutate(ebc = centrality_edge_betweenness(weights = weight,
                                           directed = TRUE )) %>% 
  unmorph()


test_network
edges_network <- test_network %>% 
  st_as_sf("edges") %>% 
  arrange(from, to) %>% 
  mutate(
    from_to_n = paste(from, to, sep = "-")
  )

ggplot() +
  geom_sf(data = p_map) +
  geom_sf(data = edges_network , 
          aes(
            linetype = from_to_n,
            colour = weight
          ),
          # arrow = arrow(angle = 45, ends = "last", 
          #               type = "open", length = unit(0.5, "cm")),
          # 
          size = 1.1) +
  geom_sf(data = st_as_sf(test_network, "nodes"), 
          aes(
            # size = deg
          )
  )  + 
  facet_wrap(. ~ season, nrow = 1) +
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
# scale_colour_viridis_c(begin = 0.15, end = 0.75, option = "B", 
#                        name = "Edge Betweeness") +
scale_size_continuous(range = c(4, 10), 
                      name = "Degree") +
  coord_sf() + 
  
  # facet_wrap(. ~ season, nrow = 1) + 
  theme_bw(base_size = 14) + 
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "Longitude", 
       y = "Latitude") 
# 
# 
# sss <- m %>% 
#   activate(edges) %>% 
#   as_tibble()
# sss
# sss$ebc
# 
# sm <- m %>% 
#   activate(nodes) %>% 
#   as_tibble()
# 
# sm
# 
# 
# plot(m)
# mss <- layout_sf(graph = m)
# mssss <- layout_with_fr(m)
# msss <- layout_edges(graph = m)
# layout.fruchterman.reingold(m)
# 
# p_map <- st_transform(p_map, crs = st_crs(m))
# 
# plot(m, layout = mssss)
# ggraph(graph = m, layout = "fr") +
#   # geom_sf(data = p_map) +
#   # geom_sf(data = st_as_sf(m, "edges")) +
#   geom_node_point(aes(colour = rec_group), size = 8) +
#   geom_edge_fan(colour = "gray50", 
#                 aes(width = weight), alpha = 0.5) +
#   # coord_sf(crs = st_crs(m)) +
#   facet_wrap(.~season, nrow = 1) +
#   theme_bw() + 
#   theme(panel.grid = element_blank(), 
#         axis.text.x = element_text(angle = 45, hjust = 1))
# 



# title = paste(unique(dat$floy_tag)))

print(p)


edges_sf <- edges_sf %>%
  filter(!floy_tag %in% "2")

# edges_sf_1 <- edges_sf %>%
#   filter(season %in% c("Fall", "Winter", 
#                        "Spring", "Summer")) %>%
#   ungroup()

nrow(edges_sf)
edges_sf <- edges_sf %>% 
  # st_drop_geometry() %>% 
  group_by(floy_tag, season) %>% 
  filter(if (NROW(season) %in% 1) !(season %in% season)
         else season %in% season) %>% 
  ungroup()


edges_sf <- edges_sf %>% 
  # st_drop_geometry() %>% 
  group_by(floy_tag, season) %>% 
  filter(if (NROW(floy_tag) %in% 1) !(floy_tag %in% floy_tag)
         else floy_tag %in% floy_tag) %>% 
  ungroup()

unique(edges_sf$floy_tag)

# ms <- edges_sf %>% 
#   st_drop_geometry() %>%
#   group_by(floy_tag, season) %>% 
#   summarise(n = NROW(season)) %>% 
#   ungroup() %>% 
#   arrange(n)

lts <- lt %>% 
  filter(floy_tag %in% "1160")



glimpse(lts) 

unique(lts$name)
unique(lts$year)

ggplot(data = lts, aes(x = detection_timestamp_utc, y = as.factor(name))) + 
  geom_point(aes(colour = as.factor(name)))


edges_sf
unique(edges_sf$season)


glimpse(edges_sf_1)

fish_id <- unique(edges_sf$floy_tag)

length(fish_id)

pdf(here::here("Fish and tagging data",
               "Receiver Downloads",
               "network analysis",
               "sf_networks_example.pdf"),
    width = 14, height = 8.5)



for (i in 1:length(fish_id)) {
  
  dat <- edges_sf %>% 
    filter(floy_tag %in% fish_id[i])
  
  
  
  sour <- dat %>% 
    st_drop_geometry() %>% 
    as_tibble() %>% 
    distinct(rec_source_group) %>% 
    rename(label = rec_source_group)
  
  des <- dat %>%
    st_drop_geometry() %>% 
    as_tibble() %>% 
    distinct(rec_dest_group) %>%
    rename(label = rec_dest_group)
  sour
  des
  
  
  nodes_1 <- full_join(sour, des, by = "label")
  nodes_1
  rl_filter <- rl_sum_sf %>% 
    filter(rec_group %in% nodes_1$label)
  rl_filter
  
  
  
  glimpse(dat)
  unique(dat$season)
  unique(dat$to)
  
  dat <- dat %>% 
    mutate_at(vars(from, to), as.character)
  
  
  rl_filter$id <- as.character(rl_filter$id)
  glimpse(dat)
  
  glimpse(rl_filter)
  
  
  
  
  m <- sfnetwork(nodes = rl_filter, edges = dat,
                 directed = TRUE, 
                 edges_as_lines = TRUE, 
                 node_key = "id",
                 force = TRUE)
  
  
  
  
  
  m <- m %>%
    activate("nodes") %>%
    mutate(deg = centrality_degree(loops = TRUE,mode = "in", 
                                   weights = weight)
    ) %>%
    
    activate("edges") %>%
    
    morph(to_split, season) %>%
    mutate(ebc = centrality_edge_betweenness(weights = weight,
                                             directed = TRUE)) %>%
    unmorph()
  
  bcs <- m %>% 
    activate("nodes") %>% 
    as_tibble()
  bcs <- unique(bcs$bc)
  
  
  dats <- dat %>% 
    select(to, from, season, weight, geometry)
  
  if (is.nan(bcs)) {
    p <- ggplot() +
      geom_sf(data = p_map) +
      geom_sf(data = st_as_sf(m, "edges"), 
              aes(colour = ebc),
              size = 1.1) +
      geom_sf(data = st_as_sf(m, "nodes"), 
              # aes(size = bc)
      ) +
      facet_wrap(.~season, nrow = 1) + 
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
    scale_colour_viridis_c(begin = 0.15, end = 0.75, option = "B", 
                           name = "Edge Betweeness") +
      scale_size_continuous(range = c(1, 5), 
                            name = "Centraility Betweeness") +
      coord_sf() + 
      
      # facet_wrap(. ~ season, nrow = 1) + 
      theme_bw(base_size = 14) + 
      theme(panel.grid.major = element_blank()) + 
      labs(x = "Longitude", 
           y = "Latitude", 
           title = paste(unique(dat$floy_tag)))
  } else {
    p <- ggplot() +
      geom_sf(data = p_map) +
      geom_sf(data = st_as_sf(m, "edges"), 
              aes(colour = ebc),
              size = 1.1) +
      geom_sf(data = st_as_sf(m, "nodes"),
              aes(size = bc)
      ) +
      facet_wrap(.~season, nrow = 1) + 
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
    scale_colour_viridis_c(begin = 0.15, end = 0.75, option = "B", 
                           name = "Edge Betweeness") +
      scale_size_continuous(range = c(1, 5), 
                            name = "Centraility Betweeness") +
      coord_sf() + 
      
      # facet_wrap(. ~ season, nrow = 1) + 
      theme_bw(base_size = 14) + 
      theme(panel.grid.major = element_blank()) + 
      labs(x = "Longitude", 
           y = "Latitude", 
           title = paste(unique(dat$floy_tag))) 
  }
  print(p)
  
  
}
dev.off()
# ---- removing sf part of network ----- 
# glimpse(m)
# sss <- st_cast(sss, "MULTIPOINT")
# 
# 
# glimpse(sss)
# sss <- st_cast(sss, "POINT")
# 
# 
# ggplot() + 
#   geom_sf(data = sss)
# 
# 
# 
# sss2 <- sss %>% 
#   as_Spatial() %>% 
#   as_tibble()
# 
# glimpse(sss2)
# 
# sms <- sm %>% 
#   as_Spatial() %>% 
#   as_tibble() %>% 
#   rename(x = coords.x1, 
#          y = coords.x2, 
#          from = id)  %>% 
#   mutate(from = as.integer(from))
# sms
# 
# s2 <- sss2 %>% 
#   left_join(sms, by = "from")
# glimpse(s2)
# 
# library(metR)
# 
# ggplot() + 
#   geom_sf(data = p_map) +
#   geom_arrow(data = sss2, aes(x = x, y = y), skip = 1)
# 
# 
# 
# mm <- dfss %>% 
#   st_drop_geometry() %>% 
#   select(from, to, weight)
# 
# mm
# 
# sss <- ss %>% 
#   st_drop_geometry()
# 
# 
# routes_igraph <- graph_from_data_frame(d = mm,
#                                        vertices = sss,
#                                        directed = TRUE)
# 
# plot(routes_igraph, edge.arrow.size=.4, edge.curved=.1, 
#     vertex.label = V(routes_igraph)$rec_group)
# ws  <-  c(1, rep(100, ecount(routes_igraph)-1))
# 
# l <- layout_with_fr(routes_igraph, weights = ws)
# 
# deg <- degree(routes_igraph, mode="all", loops = TRUE)
# 
# 
# V(routes_igraph)$size <- deg * 1.2
# E(routes_igraph)$width <- E(routes_igraph)$weight / 1000
# graph_attr(routes_igraph, "layout") <- layout_with_fr
# # l <- layout_with_fr(routes_igraph, weights = ws)
# 
# clp <- cluster_optimal(routes_igraph)
# class(clp)
# 
# 
# V(routes_igraph)$community <- clp$membership
# colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
# plot(routes_igraph, vertex.color=colrs[V(routes_igraph)$community])
# 
# plot(routes_igraph)
# glimpse(m)
# 
# 
# m[[5]]
# 
# t <-tidygraph::create_path(10)
# 
# plot(t)
# 
# ?to_sho