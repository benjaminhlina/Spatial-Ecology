
# load packages ----

library(dplyr)
library(ggplot2)
library(ggraph)
library(gdistance)
library(here)
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

lt <- read_rds(here::here("Saved data", 
                          "kenauk lake trout 2017 - 2020.rds"))




# co <- read_csv("core_20_rob.csv")
# 
# 
# 
# 
# glimpse(co)



p_map <- st_read(dsn = here::here("Shapefiles", 
                                  "."),
                 layer = "plake_edit_wo_link")


p_map_spd <- as_Spatial(p_map)



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
rl_sum_sf

# write_rds(x = rl_sum_sf, here("Saved data", 
#                               "rl_sum_sf.rds"))


rl_sum_spd <- as_Spatial(rl_sum_sf)


r <- spTransform(p_map_spd, CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))


plot(r)
plot(p_map_spd)

ext <- extent(r)
s <- raster(r, res = 5)
s <- rasterize(r, s, feild = 1)

# s[s == 1] <- 0.5
# s[is.na(s)] <- 1
# s[s == 0.5] <- NA
plot(s)
trans <- transition(s, mean, directions = 16)


rg_sum <- spTransform(rl_sum_spd, CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

rg_sum <- rg_sum %>% 
  as_tibble()

rg_sum <- rg_sum %>% 
  rename(lon = coords.x1, 
         lat = coords.x2) %>% 
  arrange(lon)


co <- rg_sum %>% 
  dplyr::select(-rec_group, -receiver_basin) %>% 
  mutate(llon = lon, 
         llat = lat, 
         lonlat = paste0(lon, ",", lat), 
         llonllat = paste0(llon, ",", llat)) %>% 
  dplyr::select(-lon, -lat, -llon, -llat) %>% 
  tidyr::expand(lonlat, llonllat) %>% 
  separate(lonlat, c("lon", "lat"), ",") %>% 
  separate(llonllat, c("llon", "llat"), ",") %>% 
  rownames_to_column("id") %>%
  mutate_if(is.character, function(x) as.numeric(x)) 
  
  





d <- co %>% 
  mutate(i = c(1:nrow(.))) %>% 
  split(.$i) %>% 
  purrr::map(possibly(~shortestPath(trans,
                                    c(.$llon, .$llat), c(.$lon, .$lat),
                                    output = "SpatialLines"), NA)) %>% 
  purrr::map(possibly(~gLength(.), NA)) %>% 
  bind_rows() %>% 
  t() %>%
  as_tibble() %>%  
  rownames_to_column("id") %>% 
  mutate_if(is.character, function(x) as.numeric(x))  %>% 
  rename(cost_dist = V1)




ds <- co %>% 
  mutate(i = c(1:nrow(.))) %>% 
  split(.$i) %>% 
  purrr::map(possibly(~shortestPath(trans,
                                    c(.$llon, .$llat), 
                                    c(.$lon, .$lat),
                                    output = "SpatialLines"), NA)) %>% 
  purrr::map(possibly(~st_as_sf(., crs = 26918), NA)) %>% 
  bind_rows() %>% 
  rownames_to_column("id") %>% 
  mutate_if(is.character, function(x) as.numeric(x)) 




rg_sum <- rg_sum %>% 
  mutate(rec_group = factor(rec_group, levels = c("Sucker Creek",
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

to_from <- rg_sum %>%
  arrange(lon) %>%
  mutate(from = rec_group,
         to = rec_group) %>%
  expand(from, to) %>%
  mutate(from_to = paste0(from, "-", to)) %>%
  rownames_to_column("id") %>% 
  mutate(id = as.numeric(id))



co_ll <- co %>% 
  rename(from_lon = lon, 
         from_lat = lat, 
         to_lon = llon,
         to_lat = llat)



co_ft <- to_from %>% 
  left_join(co_ll, by = "id") 

co_ft
  
dsd <- ds %>% 
  left_join(co_ft, by = "id")



dsd <- dsd %>% 
  left_join(d, by = "id")


dsd



write_rds(dsd, file = here("Saved data",
                           "cost_distance_per_rec_group_sf.rds"))

p_map_utm <- st_transform(p_map, crs = 26918)
rl_sum_utm <- st_transform(rl_sum_sf, crs = 26918)

p <- ggplot() + 
  geom_sf(data = p_map_utm) +
  geom_sf(data = rl_sum_utm, size = 3.5) + 
  geom_sf(data = dsd, aes(colour = cost_dist), size = 1) + 
  scale_colour_viridis_c(name = "Cost Distance (m)", option = "B") + 
  # geom_segment(data = d %>% 
  #                bind_cols(co %>% 
  #                            mutate(lon = as.numeric(lon),
  #                                   lat = as.numeric(lat),
  #                                   llon = as.numeric(llon),
  #                                   llat = as.numeric(llat))),
  #              aes(x = llon, 
  #                  xend = lon, 
  #                  y = llat, 
  #                  yend = lat, colour = cost_dist))+ 
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  labs(x = "Longitude", 
       y = "Latitude")

p
ggsave(filename = here("Papineau reciever locations and maps", 
                       "Papineau Lake Maps", 
                       "cost_distance_per_rec_group.pdf"), plot = p, 
       height = 14, width = 8.5)
# coord_sf()



geom_point(data = co %>% 
             mutate(lon=as.numeric(lon),
                    lat=as.numeric(lat),
                    llon=as.numeric(llon),
                    llat=as.numeric(llat)), 
           aes(lon, lat)) +
  geom_segment(data = d %>% bind_cols(co %>% 
                                        mutate(lon=as.numeric(lon),
                                               lat=as.numeric(lat),
                                               llon=as.numeric(llon),
                                               llat=as.numeric(llat))),
               aes(x=llon, xend=lon, y=llat, yend=lat, colour=cost_dist))+ 
  xlim(590000, 600000)+ ylim(4787500, 4797000) +
  scale_colour_gradientn(colours=rev(rainbow(7)))
