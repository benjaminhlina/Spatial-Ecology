
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


# edges_s
# 
# m1 <- glmer.nb(weight ~ from_to * season + (1|floy_tag), 
#                data = edges_w, 
#                glmerControl(optimizer = "bobyqa", 
#                             optCtrl = list(maxfun = 2e5))) 
# 
# 
# 
# m2 <- glmer.nb(sum_weight ~ from_to + (1|floy_tag), 
#                data = edges_s)
#                glmerControl(optimizer = "bobyqa", 
#                             optCtrl = list(maxfun = 2e5))) 




# ss <- edges %>% 
#   group_by(floy_tag, season) %>% 
#   summarise(n = n()) %>% 
#   ungroup()
# 
# ss
# ts <- edges %>% 
#   filter(floy_tag %in% "1670")
# 
# 
# View(ts)

fish_id <- unique(edges$floy_tag)



glimpse(edges)


 
  





# pdf(here::here("Fish and tagging data", 
#                "Receiver Downloads", 
#                "network analysis",
#                "test.pdf"), 
#     width = 17.5, height = 17.5)
# start loop to generate networks per individaul -------

for (i in 1:length(fish_id)){ 
  dat <- edges %>% 
    filter(floy_tag %in% fish_id[i])
  
  seas <- unique(dat$season) 
  
  # season_list <- list()
  
  for (ses in 1:length(seas)) {
    
    dat_1 <- dat %>% 
      filter(season == seas[ses])
    
    
    
    dat_2 <- dat_1 %>% 
      dplyr::select(from, to, weight) %>%
      mutate(to = as.character(to), 
             from = as.character(from))
    
    sources <- dat_1 %>%
      # filter(floy_tag %in% fish_id[i]) %>%
      distinct(
        from,
        rec_source_group) %>%
      rename(
        id = from,
        label = rec_source_group
      )
    
    destinations <- dat_1 %>%
      # filter(floy_tag %in% fish_id[i]) %>%
      distinct(
        to,
        rec_dest_group) %>%
      rename(
        id = to,
        label = rec_dest_group)
    
    nodes <- full_join(sources, destinations, by = c("id", "label")) 
    nodes <- nodes %>%
      dplyr::select(id, label)
    
    
    nodes <- nodes %>% 
      mutate(id = as.character(id))
    
    
    # nodes <- nodes %>%
    #   rowid_to_column("id")
    # nodes
    # 
    # unique(dat_1$from)
    # dat <- dat %>% 
    #   filter(weight >= 10)
    # 
    # routes_network <- network(dat_1, 
    #                           vertex.attr = nodes, loops = TRUE,
    #                           multiple = TRUE,
    #                           matrix.type = "edgelist", ignore.eval = FALSE)
    # print(plot(routes_network, vertex.cex = 3, 
    #            main = paste(unique(dat$floy_tag), 
    #                         unique(dat$fish_basin), 
    #                         sep = " - ")))
    
    
    # routes_tidy <- tbl_graph(nodes = nodes, edges = dat_1, directed = TRUE)
    # routes_igraph <- graph_from_data_frame(d = dat_2 
    #                                        vertices = nodes,
    #                                        directed = TRUE)
    # 
    
    # network layout
    # coords <- unique(DB[,c(19,23,24)]) # column with name station , X and Y coords
    # rownames(coords)<-coords[,1]
    # coords1<-coords
    # coords<-coords[,2:3]
    
    
    
    plot(routes_igraph, 
         edge.arrow.size = 0.2, 
         main = paste(unique(dat_1$floy_tag), 
                      unique(dat_1$fish_basin),
                      unique(dat_1$season),
                      sep = " - "), 
         vertex.label.color = "black", 
         vertex.label.dist = 2.5,
         remove.multiple = TRUE,
         edge.attr.comb = c(weight = "sum", 
                            type = "ignore")
         # vertex.color = c("#2c0b57", "#a82e5f", 
         #                  "#f98c0a")[1 + (V(routes_igraph)$)]
    )
    # print(p)
  }
  
}



season_list <- list()

results_list <- list()

for (k in 1:length(fish_id)){ 
  dat <- edges %>% 
    filter(floy_tag %in% fish_id[k])
  
  seass <- unique(dat$season) 
  
  season_list <- list()
  
  for (see in 1:length(seass)) {
    
    dat_1 <- dat %>% 
      filter(season == seass[see])
    
    
    
    dat_2 <- dat_1 %>% 
      dplyr::select(from, to, weight) %>%
      mutate(to = as.character(to), 
             from = as.character(from))
    
    sources <- edges %>%
      # filter(floy_tag %in% fish_id[i]) %>%
      distinct(
        from,
        rec_source_group) %>%
      rename(
        id = from,
        label = rec_source_group
      )
    
    destinations <- edges %>%
      # filter(floy_tag %in% fish_id[i]) %>%
      distinct(
        to,
        rec_dest_group) %>%
      rename(
        id = to,
        label = rec_dest_group)
    
    nodes <- full_join(sources, destinations, by = c("id", "label")) 
    nodes <- nodes %>%
      dplyr::select(id, label)
    
    
    nodes <- nodes %>% 
      mutate(id = as.character(id))
    
    nodes
    
    edges
    # nodes <- nodes %>%
    #   rowid_to_column("id")
    # nodes
    # 
    # unique(dat_1$from)
    # dat <- dat %>% 
    #   filter(weight >= 10)
    # 
    # routes_network <- network(dat_1, 
    #                           vertex.attr = nodes, loops = TRUE,
    #                           multiple = TRUE,
    #                           matrix.type = "edgelist", ignore.eval = FALSE)
    # print(plot(routes_network, vertex.cex = 3, 
    #            main = paste(unique(dat$floy_tag), 
    #                         unique(dat$fish_basin), 
    #                         sep = " - ")))
    
    
    routes_tidy <- tbl_graph(nodes = nodes, edges = dat_1, directed = TRUE)
    # # routes_igraph <- graph_from_data_frame(d = dat_2, 
    #                                        vertices = nodes, 
    #                                        directed = TRUE)
    #                                        
    season_list[[see]] <- routes_tidy
    # season_merged <- do.call(rbind, season_list)
    
    
  }
  
  rm(season_list)
  
  library(purrr)
  
  
  
  edges %>% 
    group_by(floy_tag, season) %>% 
    map((~tbl_graph(edges = ., nodes = nodes, directed = TRUE))) 
  
  results_list[[k]] <- season_merged
  
  
}

str(season_list[[1]])
season_list[[1]]
glimpse(season_list[[1]])
str(results_list[[1]])

results_list[[1]]

# dev.off()
# visNetwork(nodes, dat_1)
# p <- ggraph(graph = routes_tidy,
#             layout = "graphopt") +
#   # geom_node_point(aes(size = )) +
# 
#   geom_edge_loop(aes(width = weight)) +
#   geom_edge_link(aes(width = weight), alpha = 0.8) +
#   scale_edge_width(range = c(0.2, 2)) +
#   geom_node_text(aes(label = label),
#                  repel = TRUE
#   ) +
#   labs(edge_width = "Letters", title = paste(unique(dat$floy_tag),
#                                              unique(dat$fish_basin),
#                                              sep = " - ")) +
#   theme_graph()

# print(p)

summary(routes_network)

plot(routes_network, vertex.cex = 3)

plot(routes_network, vertex.cex = 3, mode = "circle")
# detach(package:network)
# rm(routes_network)

routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
routes_igraph
plot(routes_igraph, edge.arrow.size = 0.2)
plot(routes_igraph, layout = layout_with_graphopt, edge.arrow.size = 0.2)




routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
routes_igraph_tidy <- as_tbl_graph(routes_igraph)


routes_tidy


routes_tidy %>% 
  activate(edges) %>% 
  arrange(desc(weight))


ggraph(routes_tidy) + 
  geom_edge_link() + 
  geom_node_point() + 
  theme_graph()


ggraph(routes_tidy,
       layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), 
                 repel = TRUE
  ) +
  labs(edge_width = "Letters") +
  theme_graph()


ggraph(routes_igraph, layout = "linear") + 
  geom_edge_arc(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label)) +
  labs(edge_width = "Letters") +
  theme_graph()



visNetwork(nodes, edges)
edges_1 <- mutate(edges, width = weight/5 + 1)
visNetwork(nodes, edges_1) %>% 
  visIgraphLayout(layout = "layout_with_fr") %>% 
  visEdges(arrows = "middle", width = 0.001)
nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to - 1)

forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
             NodeID = "label", Group = "id", Value = "weight", 
             opacity = 1, fontSize = , zoom = FALSE)
sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
              NodeID = "label", Value = "weight", fontSize = 16, unit = "Letter(s)")

