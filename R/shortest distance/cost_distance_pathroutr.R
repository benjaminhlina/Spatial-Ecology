# ---- load packages ----
# remotes::install_github("benjaminhlina/pathroutr")

{
  library(crawl)
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(here)
  library(lwgeom)
  library(pathroutr)
  library(purrr)
  library(readr)
  library(RcppAlgos)
  library(sf)
  library(stringr)
  library(tibble)
  library(tidyr)
  source("functions\\functions.R")
  
}
# ---- GOAL - Create shortest distance between Acoustic Telemetry Receivers -----

# ---- bring in shapefile ----
# replace sissabagma_lake with your shapefile 
# I choose Big Siss as an example as this is the lake I grew up fishing on 
# back in Wisco
lake <- st_read(dsn = here("Shapefiles",
                           "."),
                layer = "plake_edit_wo_link")


# ---- bring in rec location sf ----
# replace EXAMPLE with your receiver locations as RDS or csv whatever you use 

rl_sum <- read_csv(here("Data",
                        "overall_rec_loc.csv"))
unique(rl_sum$lat)
# change rec_group to factor 

# ---- look at lake and rec structures -----
lake
rl_sum
# ---- reduce down into rec_loc table ----

rl_sum_rec <- rl_sum %>% 
  group_by(name, depth_m, basin, local) %>% 
  summarise(
    mean_lat = mean(lat) %>% 
      round(digits = 5), 
    mean_long = mean(long) %>% 
      round(digits = 5), 
  ) %>% 
  ungroup() %>% 
  mutate(rec_group = case_when(
    name %in% c(3, 4, 11, 19, 23)  ~ 1,
    name %in% 12 ~ 2,
    name %in% c(2, 18) ~ 3,
    name %in% c(1, 9, 10) ~ 4,
    name %in% c(5, 6) ~ 5,
    name %in% c(21, 22) ~ 6,
    name %in% c(7, 20) ~ 7,
    name %in% c(8, 17) ~ 8,
    name %in% 16 ~ 9,
    name %in% 15 ~ 10,
    name %in% c(13, 14) ~ 11)
  )
# mutate(rec_group = case_when(
#   name %in% c(3, 4, 11, 19, 23)  ~ "Central East Basin",
#   name %in% 12 ~ "Black Bay",
#   name %in% c(2, 18) ~ "South East Basin",
#   name %in% c(1, 9, 10) ~ "North East Basin",
#   name %in% c(5, 6) ~ "Central West Basin",
#   name %in% c(21, 22) ~ "Monaco Bay",
#   name %in% c(7, 20) ~ "Sucker Creek",
#   name %in% c(8, 17) ~ "North West Basin",
#   name %in% 16 ~ "Hidden Bay",
#   name %in% 15 ~ "Central North Basin",
#   name %in% c(13, 14) ~ "North North Basin")
# )

rl_sum_rec_group <- rl_sum_rec %>% 
  group_by(rec_group, basin) %>% 
  summarise(
    lat = mean(mean_lat) %>% 
      round(digits = 5), 
    long = mean(mean_long) %>% 
      round(digits = 5), 
  ) %>% 
  ungroup() %>% 
  st_as_sf(., coords = c("long", "lat"), 
           crs = st_crs(lake)) 

# 45.803202-74.78619



rl_sum_sf <- rl_sum_rec_group 
# filter(rec_group %in% c(3, 10,
#                         3, 11))
# ---- convert utms as distance calcualutations need to be metric ----
lake_utm <- lake %>% 
  st_transform(., crs = 32618)

rl_sum_sf_utm <- rl_sum_sf %>% 
  st_transform(., crs = 32618) 
# nudge monaco 

# both have the same CRS so we are good
# if CRS is different between body of water shape and receiver stations 
# then you will need to change CRS of one so they both match 

# ---- for pathroutr inland lakes/rivers need to inverted ----- 

# as pathrout looks at polygons as land barriers not water 
# grab the extent of the shapefile you're working with 
# and convert it into a sf object  
bbox_new <- st_bbox(lake_utm, crs = st_crs(lake_utm)) # current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

bbox_new[1] <- bbox_new[1] - (0.05 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.05 * xrange) # xmax - right
bbox_new[2] <- bbox_new[2] - (0.05 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.05 * yrange) # ymax - top

ext <- bbox_new %>%  # take the bounding box ...
  st_as_sfc()  %>%
  st_sf()

# use st_difference to create sf object with body of water as empty 
inverse <- st_difference(ext, lake_utm)

# plot to make sure it looks proper 
ggplot() + 
  geom_sf(data = inverse) +
  theme_void()

# ---- create land region to use as a barrier -----
land_region <- rl_sum_sf_utm %>% 
  st_buffer(dist = 2000) %>% # change dist to site specific buffer
  st_union() %>%
  st_convex_hull() %>% 
  st_intersection(inverse) %>% 
  st_sf()
ggplot() + 
  geom_sf(data = land_region)

# ---- prep path to determine shortest length ----
# convert receiver location sf object to table with each rec possibility 

prep_path <- rl_sum_sf %>%
  mutate(
    lon = st_coordinates(.)[,"X"],
    lat = st_coordinates(.)[,"Y"],
  ) %>% 
  st_drop_geometry() %>% 
  # once geometry removed create to and from lat longs 
  mutate(llon = lon,
         llat = lat,
         lonlat = paste0(lon, ",", lat),
         llonllat = paste0(llon, ",", llat)) %>%
  dplyr::select(-lon, -lat, -llon, -llat) %>%
  expand(lonlat, llonllat) %>% # expand for each to and from combo 
  separate(lonlat, c("lon", "lat"), ",") %>%
  separate(llonllat, c("llon", "llat"), ",")

# n <- 2
# 
# prep_path_long <- prep_path %>% 
#   pivot_longer(cols = -id, 
#                names_to = "lat_long",
#                values_to = "value") %>% 
#   select(-id) %>% 
#   separate(value, c("lon", "lat"), ",") %>%
#   mutate(
#     lon = as.numeric(lon), 
#     lat = as.numeric(lat), 
#   ) %>% 
#   left_join(
#    rl_sum_sf %>% 
#      mutate(
#        lon = st_coordinates(.)[,"X"], 
#        lat = st_coordinates(.)[,"Y"]
#      ) %>% 
#      st_drop_geometry(), by = c("lon", "lat")) %>% 
#   st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
#   mutate(id = (row_number() - 1) %/% n + 1) %>% 
#   # pivot_longer()
#   
#   group_by(id) %>% 
#   summarise(do_union = FALSE) %>% 
#   st_cast('LINESTRING')
#   
# ggplot() + 
#   geom_sf(data = prep_path_long)

# prep_path will be a dataframe of the lat/long combination of each possible
# to and from 

# ---- Create df that is each rec combo linked to lat long for combo ----

rec_order <- prep_path %>%
  left_join(
    rl_sum_sf %>% 
      mutate(
        lon = as.character(st_coordinates(.)[,"X"]), 
      ) %>% 
      st_drop_geometry() %>% 
      rename(from = rec_group), by = c("lon")
  ) %>%  
  left_join(
    rl_sum_sf %>% 
      mutate(
        lon = as.character(st_coordinates(.)[,"X"])
      ) %>% 
      st_drop_geometry() %>% 
      rename(to = rec_group,
             llon = lon),
    by = c("llon"), 
  ) %>% 
  mutate(
    from_to = paste(from, "-", to, sep = "")
  ) %>% 
  dplyr::select(from, to, from_to, lon, lat, llon, llat) %>% 
  mutate(across(lon:llat, ~as.numeric(.)))

rec_order

rec_order %>% 
  print(n = 121)
# rec_order %>%
#   filter(from_to %in% "3-11") %>%
#   ggplot() +
#   geom_point(aes(x = lon, y = lat), size = 3) +
#   geom_point(aes(x = llon, y = llat), size = 3) +
#   geom_sf(data = lake, fill = "NA")


# rec_order is pre_path, with the metadata of which receivers go where  
prep_path <- prep_path %>%
  mutate_if(is.character, function(x) as.numeric(x))
  

# ---- convert prep_path df to sf object with lat/long being linestrings ----
path <- prep_path %>%
  pmap(make_line) %>%
  st_as_sfc(crs = 4326) %>%
  st_sf() %>%  
  mutate(
    lon = st_startpoint(.) %>%
      st_coordinates(.) %>%
      as_tibble() %>%
      .$X,
    llon = st_endpoint(.) %>%
      st_coordinates(.) %>%
      as_tibble() %>%
      .$X
  ) %>% 
  left_join(rec_order %>%
              select(from:lon, llon),
            by = c("lon", "llon")
  ) %>%
  select(from:from_to) %>% 
  st_transform(crs = 32618) %>% 
  arrange(from, to)

rec_order
path %>% 
  print(n = 121)

# path %>%
#   filter(from_to %in% "3-11") %>%
#   ggplot() +
#   geom_sf(linewidth = 1) +
#   geom_sf(data = land_region, fill = "cornsilk3")

# ---- sample points along path for pathroutr and convert to multipoint -----
# change dfMaxLength to whatever sampling interval of interest, I 
# choose 5 m but this can be any metric 
# Without path_pts R only can look at receiver to receiver 
path_pts <- path %>% 
  st_segmentize(dfMaxLength = units::set_units(5, m), 
                # type = "regular"
  ) %>%
  st_cast("MULTIPOINT") %>% 
  filter(!(from == to))



# path_pts_fixs <- path_pts %>%
#   filter(from != 6 & to != 6)

#
path_pts %>%
  filter(from_to %in% "3-10") %>%
  ggplot() +
  geom_sf(linewidth = 1) +
  geom_sf(data = land_region, fill = "cornsilk3")
# # ---- plot land_region, rec_locs, and paths and path points ------
ggplot() +
  geom_sf(data = land_region, fill = "cornsilk3") +
  geom_sf(data = rl_sum_sf, colour = "deepskyblue3", size = 2) +
  geom_sf(data = path, colour = "darkgrey", linewidth = 1) +
  theme_void()
ggplot() +
  geom_sf(data = land_region, fill = "cornsilk3") +
  geom_sf(data = rl_sum_sf, colour = "deepskyblue3", size = 3) +
  # geom_sf(data = path_pts, colour = "darkgrey", size = 2) +
  theme_void() 

# 
# ggsave(filename = here("pathroutr issue", 
#                        "maps", 
#                        "study_lake_rec_loc.png"), 
#        height = 11, width = 8.5, plot = p)

# ggplot() +
#   geom_sf(data = rl_sum_sf, colour = "deepskyblue3", size = 3) +
#   geom_sf(data = path, colour = "darkgrey",
#           linewidth = 1) +
#   theme_void()
#
#
ggplot() +
  geom_sf(data = land_region, fill = "cornsilk3") +
  geom_sf_label(data = rl_sum_sf, aes(label = rec_group),
                size = 4) 
#   geom_sf(
#     data = path %>% # look at just rec 1 to make sure 1 goes to each rec
#       filter(from %in% "Black Bay"), aes(colour = from_to),
#     linewidth = 1) +
#   theme_void()

# ---- use pathroutr to id track points that intersect the shore of the lake ------
path_pts_fix <- prt_trim(trkpts = path_pts, barrier = land_region)

# ggplot() +
#   geom_sf(data = land_region, fill = "cornsilk3") +
#   geom_sf(data = rl_sum_sf, colour = "deepskyblue3", size = 3) +
#   geom_sf(data = path_pts_fix, colour = "darkgrey", size = 2) +
#   theme_void() -> p1
# 
# 
# ggsave(filename = here("pathroutr issue", 
#                        "maps", 
#                        "study_lake_rec_loc_path_pts.png"), 
#        height = 11, width = 8.5, plot = p1)
# 

# fix(prt_trim)

# barrier_intersect <- path_pts %>% 
#   mutate(
#     inster = sf::st_intersects(path_pts, land_region) %>% 
#       purrr::map_lgl(~length(.x) > 0) 
#   ) %>%
#   filter(inster %in% FALSE) %>% 
#   st_cast("MULTIPOINT")
#   
# 
# barrier_intersect <- barrier_intersect %>% 
#   filter(inster %in% FALSE)
# 
# ggplot() +
#   # geom_sf(data = land_region, fill = "cornsilk3") +
#   # geom_sf(data = rl_sum_sf, colour = "deepskyblue3", size = 2) +
#   geom_sf(data = path_pts_fix %>% 
#             filter(from_to == "11-4"), colour = "black", size = 2) +
#   theme_void()

# prt_trim
# trkpts <- sf::st_cast(path_pts, "POINT")
# barrier_intersect <- trkpts %>% 
#   mutate(intersects = sf::st_intersects(trkpts, land_region) %>% 
#   purrr::map_lgl(~length(.x) > 0))
# 
# barrier_intersect_intersects <- barrier_intersect %>% 
#   filter(intersects == TRUE) %>% 
#   group_by(from_to) %>% 
#   summarise(do_union = FALSE) %>% 
#   st_cast("MULTIPOINT") 


lake_bar_seg <- get_barrier_segments(trkpts = path_pts,
                     barrier =  land_region)
# fix(prt_visgraph)
# View(prt_visgraph)
# aug_points <- rl_sum_sf_utm
# aug_points$geometry %>% 
#   st_geometry()
# aug_points %>% 
#   st_geometry() %>% 
#   class()



# ---- create visgraph using land_regeion which is network analysis triangles -----
# pathroutr will use network analyses of triangles as paths 
vis <- prt_visgraph_bh(barrier = land_region,
                       # aug_points = rl_sum_sf_utm,
                       # buffer = 50
                       # adjust buffer, in this case buffer is 50 m 
                       # away from land
)
# View(prt_visgraph)
# aug_points <- rl_sum_sf_utm$geometry
# 
# barrier <- land_region %>% 
#   st_cast("POLYGON") %>% 
#   st_union()
# buf_poly <- barrier 
# 
# 
# init_dt <- buf_poly %>% 
#   st_cast("POLYGON") %>%
#   # st_cast("MULTILINESTRING") %>% 
#   # st_cast("LINESTRING") %>% 
#   # # st_as_sf() %>% 
#   # # mutate(id = 1:nrow(.))
#   # st_segmentize(dfMaxLength = units::set_units(500, m)) %>% 
#   st_cast("MULTIPOINT") %>% 
#   
#   
#   st_triangulate() %>% # then trianglualte to created dilanted trinagles 
#   # between barrier points 
#   st_collection_extract("POLYGON") # then make those triangles into polygons
# ggplot() + 
#   geom_sf(data = init_dt)
# 
# centroid_limits <- 75000
# 
# centroid_limits <- units::set_units(centroid_limits, "m^2")
# ctr_pts <- st_centroid(init_dt[st_area(init_dt) > centroid_limits])
# 
# 
# init_dt[st_area(init_dt) > centroid_limits] %>%
#   st_centroid() %>%
#   ggplot() +
#   geom_sf()
# 
# c(buf_poly %>%
#     st_cast("POLYGON") %>%
#     st_cast("MULTIPOINT"), 
#   # aug_points, 
#   ctr_pts
#   ) %>%
#   ggplot() +
#   geom_sf()
# 
# edges_2 <- c(buf_poly %>% 
#              st_cast("POLYGON") %>%
#              st_cast("MULTIPOINT"),
#              aug_points
#              # ctr_pts
#              ) %>%
# 
#   st_union() %>% 
#   st_triangulate(bOnlyEdges = TRUE
#   ) %>% 
#   st_cast("LINESTRING") %>% 
#   st_sf()
# 
# # edges_2 %>% 
# # ggplot() + 
# #   geom_sf()
# 
# # edges <- buf_poly %>% 
# #   st_cast("MULTIPOINT") %>% 
# #   st_union() %>% 
# #   st_triangulate(bOnlyEdges = TRUE, 
# #                  dTolerance = 10) %>% 
# #   st_cast("LINESTRING") %>%
# #   st_sf()
# 
# 
# 
# # ggplot() +
# #   geom_sf(data = init_dt)
# # 
# # ggplot() +
# #   geom_sf(data = edges)
# 
# crosses <- do.call(c, barrier %>% 
#                      st_buffer(-1) %>% 
#                      st_intersects(edges_2))
# edges_3 <- edges_2[-crosses, ]
# 
# ggplot() +
#   geom_sf(data = edges_3)
# sln <- suppressWarnings(sfnetworks::as_sfnetwork(edges_3, 
#                                                  directed = FALSE, 
#                                                  length_as_weight = TRUE))
# vis_graph_sf_2 <- sfnetworks::activate(sln, "edges") %>% 
#   sf::st_as_sf()
# 



# use sfnetworks to activate edge reroutes 
vis_graph_sf <- sfnetworks::activate(vis, "edges") %>%
  sf::st_as_sf()


# view visgraph network of triangles that sfnetwork creates 
# sfnetwork is  powerful 
ggplot() + 
  geom_sf(data = vis_graph_sf) +
  geom_sf(data = land_region, fill = "cornsilk3") + 
  theme_void() -> p2

p2

# ggsave(filename = here("pathroutr issue", 
#                        "maps", 
#                        "study_lake_vis_graph_sf.png"), 
#        height = 11, width = 8.5, plot = p2)

# View(prt_shortpath)
#   geom_sf(data = rl_sum_sf, colour = "deepskyblue3", size = 2)
# View(prt_shortpath)
# View(prt_nearestnode)
# create what sections have been rerouted from the shore 
segs_tbl <- lake_bar_seg %>% 
  prt_shortpath(vis_graph = vis, blend = TRUE) 
# segs_tbl
# prt_shortpath
# view rerouted sections around land 
ggplot() + 
  geom_sf(data = land_region, fill = "cornsilk3", size = 0) +
  ggspatial::layer_spatial(segs_tbl$geometry, color = "deepskyblue3", 
                           linewidth = 1) +
  theme_void() -> p3 

p3
# ggsave(filename = here("pathroutr issue", 
#                        "maps", 
#                        "study_lake_segs_tbl.png"), 
#        height = 11, width = 8.5, plot = p3)
# path_pts_fix_1 <- path_pts_fix %>% 
#   # filter(from_to %in% c(
#   #   # "3-11", 
#   #   # "11-3", 
#   #   "10-3")) %>% 
#   prt_trim(trkpts = ., barrier = land_region) %>%
#   group_by(from_to) %>%
#   summarise(do_union = FALSE) %>%
#   st_cast('MULTIPOINT')

# ggplot() +
# geom_sf(data = land_region) +
# geom_sf()
# ---- take striahtline paths and add in the reroutes ------  


track_pts_fix <- prt_reroute(barrier = land_region, vis_graph = vis, 
                             trkpts = path_pts)



# 
# path_pts_fix %>% 
#   as_tibble() %>% 
#   rows_update(y = track_pts_fix, by = "fid") %>% 
#   st_sf(sf_column_name = "geometry") %>% 
#   # group_by(from_to) %>% 
#   # summarise(do_union = FALSE) %>% 
#   # st_cast('LINESTRING') %>%  
#   # ungroup() %>% 
#   separate(from_to, into = c("from", "to"), sep = "-",
#            remove = FALSE) %>%
#   # mutate(
#   #   cost_dist_m = as.numeric(st_length(.))
#   # ) %>% 
#   filter(from != to) %>% 
#   dplyr::select(from, to, from_to, geometry) -> test


# gc()





track_pts_fixs <- prt_update_points(rrt_pts = track_pts_fix,
                                    trkpts = path_pts)

# fix(prt_update_points)


# View(track_pts_fixs)

track_pts_fixs

# ggplot() + 
#   geom_sf(data = land_region) + 
#   geom_sf(data = track_pts_fixs)
#   geom_sf(data = track_pts_fixed)

# convert in linestrings 
track_pts_fixed <- track_pts_fixs %>% 
  # filter(from_to %in% "3-11") %>% 
  group_by(from_to) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast('LINESTRING') %>%  
  ungroup() %>% 
  separate(from_to, into = c("from", "to"), sep = "-",
           remove = FALSE) %>%
  mutate(
    cost_dist_m = as.numeric(st_length(.))
  ) %>%
  filter(from != to) %>% 
  dplyr::select(from, to, from_to, cost_dist_m, geometry)

track_pts_fixed
# 
# test <- track_pts_fixed %>%
#   mutate(
#     lon = st_startpoint(.) %>%
#       st_coordinates(.) %>%
#       as_tibble() %>%
#       .$X,
#     lat = st_startpoint(.) %>%
#       st_coordinates(.) %>%
#       as_tibble() %>%
#       .$Y,
#     llon = st_endpoint(.) %>%
#       st_coordinates(.) %>%
#       as_tibble() %>%
#       .$X,
#     llat= st_endpoint(.) %>%
#       st_coordinates(.) %>%
#       as_tibble() %>%
#       .$Y
#   ) %>%
#   st_drop_geometry() %>%
#   st_as_sf(coords = c("lon", "lat"), crs = st_crs(land_region))
# 
# test
# 
# ggplot() +
#   geom_sf(data = land_region, fill = "cornsilk3", size = 0) +
#   geom_sf(data = test %>% 
#             filter(from_to == "North North Basin-Black Bay")) +
#   geom_sf_label(data = test %>% 
#                   filter(from_to == "North North Basin-Black Bay"), 
#                 aes(label = from))

# view one reroute to confimr pathroutr is reouting 
track_pts_fixs %>% 
  filter(from_to == "11-4") %>% 
  ggplot() + 
  # geom_sf(data = land_region, fill = "cornsilk3", size = 0) +
  # # geom_sf(data = vis_graph_sf) +
  # geom_sf(data = rl_sum_sf_utm,
  #         size = 4, colour = "black") +
  geom_sf(color = "deepskyblue3", 
          size = 3) +
  geom_sf(data = path_pts %>% 
            filter(from_to == "11-4"), 
          colour = "darkgrey") + 
  theme_void() -> p4 
p4
# ggsave(filename = here("pathroutr issue", 
#                        "maps", 
#                        "study_lake_11-4_pts.png"), 
#        height = 11, width = 8.5, plot = p4)

track_pts_fixed %>% 
  filter(from_to == "11-4") %>%
  ggplot() + 
  geom_sf(data = land_region, fill = "cornsilk3", size = 0) +
  # geom_sf(data = vis_graph_sf) +
  geom_sf(data = rl_sum_sf_utm,
          size = 4, colour = "black") +
  geom_sf(color = "deepskyblue3", 
          linewidth = 1) +
  theme_void() -> p5
p5
# ggsave(filename = here("pathroutr issue", 
#                        "maps", 
#                        "study_lake_11-4_linestring.png"), 
#        height = 11, width = 8.5, plot = p5)
# ---- make distance graph between receviers ----- 
ggplot() + 
  geom_sf(data = lake_utm, colour = "black",
          # fill = "cornsilk3", 
          size = 1) +
  # geom_sf(data = vis_graph_sf) + 
  geom_sf(data = rl_sum_sf_utm,
          size = 4, colour = "black") +
  # geom_sf(data = track_pts_fixs, 
  #         color = "deepskyblue3", 
  #         # aes(color = cost_dist_m), 
  #         linewidth = 1) +
  geom_sf(data = track_pts_fixed, 
          # color = "deepskyblue3", 
          aes(color = cost_dist_m),
          linewidth = 1) +
  scale_colour_viridis_c(option = "B", 
                         name = "Cost Distance (m)", begin = 0.25, end = 0.75) + 
  theme_bw(base_size = 13) + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black")) + 
  labs(x = "Longitude", 
       y = "Latitude") 




# ggsave(here( 
#   "test_3.png"), 
#   height = 8.5, width = 8.5, plot = last_plot())
