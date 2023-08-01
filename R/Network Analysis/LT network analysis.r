library(dplyr)
library(ggplot2)
library(GGally)
library(here)
library(igraph)
library(magrittr)
library(network)
library(readr)
library(sna)


k_LT_2018 <- read_rds(here::here("Saved data", 
                                 "kenauk lake trout 2017 - 2020.rds"))

glimpse(k_LT_2018)

k_LT_2018 <- lt
#
begin_node <- k_LT_2018 %>%
  group_by(letter_name, transmitter_serial) %>%
  summarise() %>%
  ungroup()

  

begin_node
nodes <- as.matrix(begin_node)


lt_network <- graph.edgelist(nodes, directed = FALSE)


plot(lt_network,layout = layout.lgl(lt_network))
vertex_attr(lt_network)
edge_attr(lt_network)




receiver_node <- k_LT_2018 %>%
  group_by(letter_name, receiver_basin) %>%
  summarise() %>%
  ungroup()

begin_node <- left_join(begin_node, receiver_node, 
                        by = "letter_name")


begin_node

r_node <- begin_node %>%
  select(receiver_basin)




lt_network <- set_edge_attr(lt_network, "r_basin", value = r_node)


lt_network

ne <- layout_nicely(lt_network)



edge_attr(lt_network)
plot(lt_network, layout = ne)

