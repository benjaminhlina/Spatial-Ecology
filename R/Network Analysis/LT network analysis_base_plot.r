
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
library(ggraph)
library(visNetwork)
library(networkD3)

# bring in clean downloaded ----

lt <- read_rds(here::here("Saved data", 
                          "kenauk lake trout 2017 - 2020.rds"))



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





rl_sum

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


lt <- lt %>%
  mutate(rec_source_to_dest = paste(rec_source_group, rec_dest_group, sep = " - "))


glimpse(lt)


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






# nodes <- tibble(
#   id = as.integer(unique(lt$name_order)),
#   label = as.character(unique(lt$letter_name))
# ) %>%
#   arrange(id)



# create edge list -----
# per_route <- lt %>% 
#   group_by(
#     rec_source_group, rec_dest_group,
#     # rec_source_n, rec_dest_n
#   ) %>% 
#   summarise(weight = n()) %>% 
#   ungroup() %>% 
#   arrange(weight)
# View(per_route)

per_route_id <- lt %>% 
  group_by(floy_tag, fish_basin, receiver_basin, 
           rec_source_group, rec_dest_group, season) %>% 
  summarise(weight = n()) %>% 
  ungroup() %>% 
  arrange(weight, rec_source_group, floy_tag)

# per_route <- per_route %>% 
#   mutate(from_to = paste(rec_source_n, rec_dest_n, sep = "-")) %>% 
#   dplyr::select(from_to, weight)


# per_route_id
# View(per_route_id)
# 
# per_route_id 
# nodes

edges <- per_route_id %>% 
  left_join(nodes, by = c("rec_source_group" = "label")) %>% 
  rename(from = id)


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


edges <- edges %>% 
  mutate(from = as.integer(from), 
         to = as.integer(to))
edges

edges <- edges %>% 
  mutate(from_to = paste(rec_source_group, rec_dest_group, sep = "-"))






ggplot(data = edges, aes(x = weight)) + 
  geom_histogram()

# View(edges)



descdist(edges$weight, discrete = TRUE)


fit.neg.bi <- fitdist(edges$weight, distr = "nbinom", method = "mme")

par(mfrow = c(2, 2))

plot(fit.neg.bi)

glimpse(edges)

edges <- edges %>% 
  arrange(floy_tag)




sum_edges <- edges %>% 
  group_by(season, weight) %>% 
  summarise(n = n()) %>% 
  ungroup()




edges_w <- edges %>% 
  filter(weight >= 10)


# View(sum_edges)

edges_s <- edges %>% 
  group_by(floy_tag, fish_basin, receiver_basin, from_to) %>%
  summarise(sum_weight = sum(weight)) %>% 
  ungroup()


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











dev.off()


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
  
  for (i in 1:length(seas)) {
    
    dat_1 <- dat %>% 
      filter(season == seas[i])
    
    
    rl_n <- rl_sum %>% 
      filter(rec_group %in% dat_1$rec_source_group)
    
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
    routes_igraph <- graph_from_data_frame(d = dat_2, 
                                           vertices = nodes, 
                                           directed = TRUE)
    # network layout
    coords <- rl_n %>%
      select(rec_group, long, lat) %>% 
      as.data.frame()
    # coords <- as.data.frame(unique(rl_n[,c(1, 3, 4)]))
    # create coords into matrix with row names as rec_group -----
    
    # column with name station , X and Y coords
    rownames(coords) <- coords[,1]
    coords1 <- coords
    coords <- coords[,2:3]
    names(coords) <- c("X", "Y") #change name of column
    coords2 <- (as.matrix(coords))
    coords <- coordinates(coords)
    
    xlim <- c(min(coords1$long) - 0.05, 
              max(coords1$long) + 0.05)
    
    ylim <- c(min(coords1$lat) - 0.05, 
              max(coords1$lat) + 0.05)
    
    
    statdd <- data.frame(cbind(rl_n$long, 
                               rl_n$lat))
    #create a new dataframe with only lat and long in decimal degrees
    statdd <- unique(statdd)
    names(statdd) <- c("X", "Y") 
    # dev.off()
    plot(coords1$long, 
         coords1$lat, 
         col = "gray30", 
         pch = 16, asp = 0.7, 
         cex = 0.5, type ="n", 
         xlim, ylim
         )
    
    
    plot(p_map_spd, add = TRUE, 
         col = "gray90", 
         border = "black")
    points(statdd$X, statdd$Y, 
           col = "grey60", 
           pch = 16, 
           xlab = "Longitude",
           ylab = "Latitude", 
           asp = 0.7, cex = 1) #plot you receivers
    
    text(statdd$X, 
         statdd$Y,
         labels = unique(rl_n$rec_group), 
         cex = 0.7, pos =1)
    plot(
      simplify(
      routes_igraph, 
                  remove.loop = TRUE
                  ),
         add = TRUE, rescale = FALSE,
         layout = coords,
         vertex.size = 0.25, vertex.label.dist = -100, 
         vertex.label.color = "grey40", 
         vertex.color = "yellow", 
         edge.curved = TRUE, edge.color = "blue", 
         main = paste(unique(dat_1$floy_tag), 
                      unique(dat_1$fish_basin),
                      unique(dat_1$season),
                      sep = " - "))
    
    # plot(routes_igraph, 
    #      edge.arrow.size = 0.2, 
    #      main = paste(unique(dat_1$floy_tag), 
    #                   unique(dat_1$fish_basin),
    #                   unique(dat_1$season),
    #                   sep = " - "), 
    #      vertex.label.color = "black", 
    #      vertex.label.dist = 2.5,
    #      remove.multiple = TRUE,
    #      edge.attr.comb = c(weight = "sum", 
    #                         type = "ignore")
         # vertex.color = c("#2c0b57", "#a82e5f", 
         #                  "#f98c0a")[1 + (V(routes_igraph)$)]
    # )
    # print(p)
  }
  
}


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
}

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

