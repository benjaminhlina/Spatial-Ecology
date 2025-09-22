
# load packages ----

library(data.table)
library(dplyr)
library(forcats)
library(glatos)
library(ggplot2)
library(here)
library(lubridate)
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

cd <- read_rds(here::here("Saved data", 
                          "cost_distance_per_rec_sf.rds"))


# add species column to each dataframe -----

lt$species <- "lt"

glimpse(lt)


lt <- lt %>% 
  filter(passed_filter == 1)

# lt <- lt %>% 
#   filter(sensor_unit %in% c("m/s²", "m", NA) & passed_filter == 1)
# 



# make receiver groups based on location in the lake -----
# lt <- lt %>% 
#   mutate(rec_group = case_when(name %in% c(3, 4, 11, 19, 23)  ~ "Central East-Basin", 
#                                name %in% 12 ~ "Black Bay", 
#                                name %in% c(2, 18) ~ "South East-Basin", 
#                                name %in% c(1, 9, 10) ~ "North East-Basin", 
#                                name %in% c(5, 6) ~ "Central West-Basin",
#                                name %in% c(21, 22) ~ "Monaco Bay", 
#                                name %in% c(7, 20) ~ "Sucker Creek", 
#                                name %in% c(8, 17) ~ "North West-Basin",
#                                name %in% 16 ~ "Hidden Bay",
#                                name %in% 15 ~ "Central North-Basin", 
#                                name %in% c(13, 14) ~ "North North-Basin"))






# determine everytime letter_name switches ------
lt <- lt %>% 
  arrange(floy_tag, detection_timestamp_utc) %>% 
  group_by(floy_tag) %>% 
  mutate(change_rec = if_else(letter_name != lag(letter_name) | 
                                is.na(lag(letter_name)), 
                              1, 0)) %>% 
  ungroup()


glimpse(lt)
# filter in switches  
lt_switch <- lt %>% 
  filter(change_rec == 1)


glimpse(lt_switch)


ls <- lt %>% 
  dplyr::select(floy_tag, detection_timestamp_utc, name, change_rec)


# View(ls)
# create duplicate colum for destiation 
lt_switch <- lt_switch %>% 
  mutate(
    # rec_dest_ln = letter_name,
    rec_dest_n = name
  )

glimpse(lt_switch)


# create where fish are coming from 
lt_switch <- lt_switch %>% 
  arrange(floy_tag, detection_timestamp_utc) %>% 
  group_by(floy_tag) %>% 
  mutate(
    # rec_source_ln = lag(rec_dest_ln), 
    rec_source_n = lag(rec_dest_n)
  ) %>% 
  ungroup()

# ts <- lt_switch %>%
#   group_by(rec_source_ln) %>%
#   summarise(n = n()) %>%
#   ungroup()
# 
# View(ts)


# remove the first time its heard 
lt_switch <- lt_switch %>% 
  filter(
    # !(rec_source_ln == is.na(rec_source_ln)),
    !(rec_source_n == is.na(rec_source_n)))


lt_switch <- lt_switch %>%
  mutate(rec_source_to_dest = paste(rec_source_n, rec_dest_n, sep = "-"))


glimpse(lt_switch)







sum_rec_switch <- lt_switch %>% 
  group_by(rec_source_to_dest) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(n)




sum_rec_switch

View(sum_rec_switch)
# create node list ------
# sources <- lt_switch %>%
#   distinct(rec_source_ln) %>%
#   rename(label = rec_source_ln)
# 
# destinations <- lt_switch %>%
#   distinct(rec_dest_ln) %>%
#   rename(label = rec_dest_ln)
# 
# nodes <- full_join(sources, destinations, by = "label")
# nodes
# 
# nodes <- nodes %>%
#   rowid_to_column("id")
# nodes

nodes <- tibble(
  id = as.integer(unique(lt_switch$name_order)),
  label = #as.character(
    unique(lt_switch$name)
    # )
) %>%
  arrange(id)



# create edge list -----
per_route <- lt_switch %>% 
  group_by(
    # rec_source_ln, rec_dest_ln, 
    rec_source_n, rec_dest_n
  ) %>% 
  summarise(weight = n()) %>% 
  ungroup() %>% 
  arrange(weight)
# View(per_route)

per_route_id <- lt_switch %>% 
  group_by(floy_tag, season, rec_source_n, rec_dest_n,) %>% 
  summarise(weight = n()) %>% 
  ungroup()

# per_route <- per_route %>% 
#   mutate(from_to = paste(rec_source_n, rec_dest_n, sep = "-")) %>% 
#   dplyr::select(from_to, weight)


per_route
# View(per_route)

per_route_id 


edges <- per_route_id %>% 
  left_join(nodes, by = c("rec_source_n" = "label")) %>% 
  rename(from = id)


edges <- edges %>% 
  left_join(nodes, by = c("rec_dest_n" = "label")) %>% 
  rename(to = id)

edges



cd <- cd %>% 
  st_drop_geometry() %>% 
  as_tibble()

cd <- cd %>% 
  select(-id:-to)

cd


cd$cost_dist[is.na(cd$cost_dist)] <- 0


edges <- edges %>% 
  mutate(from_to = paste0(rec_source_n, "-", rec_dest_n, sep = ""))

edges









edges <- edges %>% 
  left_join(cd, by = "from_to")



edges

sum_dist <- edges %>% 
  group_by(floy_tag, from_to, season) %>% 
  summarise(total_dist = sum(cost_dist), 
            avg_dist = mean(cost_dist)) %>% 
  ungroup()


sum_dist




ggplot(data = sum_dist, aes(x = total_dist)) + 
  geom_histogram() + 
  facet_wrap(.~ season, nrow = 1)
ggplot(data = sum_dist, aes(x = avg_dist)) + 
  geom_histogram() + 
  facet_wrap(.~ season, nrow = 1)


ggplot(data = sum_dist, aes(x = from_to, y = avg_dist)) + 
  geom_point(aes(colour = floy_tag), size = 3) +
  # geom_line(aes(group = floy_tag), colour = "black",
  #           size = 1) +
  facet_wrap(.~ season, nrow = 1)




# edges <- edges %>% 
#   dplyr::select(from, to, weight)


edges <- edges %>% 
  mutate(from = as.integer(from), 
         to = as.integer(to))
edges 
routes_network <- network(edges, 
                          vertex.attr = nodes, 
                          matrix.type = "edgelist", ignore.eval = FALSE)
summary(routes_network)

plot(routes_network, vertex.cex = 3)

plot(routes_network, vertex.cex = 3, mode = "circle")
# detach(package:network)
# rm(routes_network)

routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
routes_igraph
plot(routes_igraph, edge.arrow.size = 0.2)
plot(routes_igraph, layout = layout_with_graphopt, edge.arrow.size = 0.2)

str(routes_igraph)


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

