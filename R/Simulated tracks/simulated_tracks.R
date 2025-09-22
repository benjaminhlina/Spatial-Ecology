# ---- load packages ----

library(dplyr)
library(forcats)
library(ggplot2)
library(ggspatial)
library(glatos)
library(here)
library(lubridate)
library(purrr)
library(readr)
library(sf)
library(sp)
library(tidyr)


# ---- Bring in shapefile ------
p_map <- st_read(dsn = here::here("Shapefiles",
                                  "."),
                 layer = "plake_edit_wo_link")
# ---- bring in cost_distance analsysis per recevier group --------
cd <- read_rds(here::here("Saved data", 
                          "cost_distance_per_rec_group_sf.rds"))
# replace any NA value with 0 
cd$cost_dist[is.na(cd$cost_dist)] <- 0


# ---- bring in rec location sf ----
lt <- read_rds(here::here("Saved data", 
                          "kenauk lake trout 2017 - 2020.rds"))
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

# coord_sf()

# We will now summarize the receiver locations for the entire study period 2017-2021

# ---- summarize receiver locations over the study period ----




# Now that we have summarized receiver locations and created
# receiver groupings we need to summarize by receiver groups as they represent similar habitats

# ---- reduce down into rec_group table ----
rl_sum_rec_group <- lt %>% 
  group_by(rec_group, receiver_basin) %>% 
  summarise(
    lat = mean(rec_group_lat) %>% 
      round(digits = 5), 
    long = mean(rec_group_long) %>% 
      round(digits = 5), 
  ) %>% 
  ungroup() %>% 
  st_as_sf(., coords = c("long", "lat"), 
           crs = st_crs(p_map)) 

rl_sum_sf <- rl_sum_rec_group  %>% 
  rename(basin = receiver_basin)

# 
# ggplot() + 
#   geom_sf(data = p_map) +
#   geom_sf(data = rl_sum_sf, aes(colour = rec_group), 
#              size = 3) 


# Now that we have created summarized receiver groupings 
# and converted them into sf object to determine the shortest paths we 
# need change the crs on all sf objects to utm as everything will be in meters

# ---- convert utms as distance calcualutations need to be metric ----
lake_sp <- p_map %>% 
  as_Spatial()

lake_wgs <- p_map %>% 
  st_transform(., crs = 4326)
rl_sum_sf_wgs <- rl_sum_sf %>% 
  st_transform(., crs = 4326) 

rl_sum_sp <- rl_sum_sf %>% 
  as_Spatial()

lc.trans <- make_transition3(p_map, res = c(0.001,0.001)) 

foo <- crw(theta = c(0, 25), 
           stepLen = 10, 
           initPos = c(0, 0), 
           initHeading = 0, 
           nsteps = 10)
plot(foo, type = "o", pch = 20, asp = c(1, 1))


# create vector to hold simulated paths
paths <- vector("list", length = 100)
# create metadata table to capture simulation specs

path_list <- list()
results_list <- list() 
basin_id <- unique(rl_sum_sf$basin)



# i <- 1
for (i in 1:3){
  
  df <- rl_sum_sf %>% 
    filter(basin %in% basin_id[i]) %>% 
    mutate(
      lon = st_coordinates(.)[,"X"],
      lat = st_coordinates(.)[,"Y"],
    ) %>% 
    st_drop_geometry() %>% 
    slice_sample(n = 1)
  
  df
  
  b <- df$basin
  ip <- c(df$lon, df$lat)
  ip_lon <-  ip[1]
  ip_lat <-  ip[2]
  
  for (p in 1:20) {
    angle <- sample(c(25:100), 1) # after every position the angle after
    length <- sample(c(250:1000), 1) # step length distance of movement in m 
    
    paths <- crw_in_polygon(polyg = lake_sp,
                            theta = c(0, angle),
                            stepLen = length,
                            initPos = ip, 
                            nsteps = 50,
                            cartesianCRS = 3175,
                            sp_out = TRUE) 
    paths <- paths %>% 
      mutate(
        sim_id = paste0("sim_", p), 
        theta = angle, 
        stepLen = length, 
        basin = b, 
        int_lon = ip_lon, 
        int_lat = ip_lat
      )
    path_list[[p]] <- paths 
    
    results <- bind_rows(path_list)
    message('Processing sim_track for basin ', i, " of ", length(basin_id), 
            " for ", p, ' of ', 20)
    
  }
  
  results_list[[i]] <- results 
  
}
beepr::beep()
results <- bind_rows(results_list)


results <- results %>% 
  group_by(basin, sim_id) %>% 
  mutate(
    id = cur_group_id()
  )
ggplot() + 
  geom_sf(data = p_map) + 
  geom_sf(data = results, aes(shape = basin, colour = sim_id), size = 2)

# metadata$start_region[i] <- b 
# metadata$ip_lon[i] <-  ip_lon
# metadata$ip_lat[i] <-  ip_lat
# }

beepr::beep()
path_id <- unique(results$id)

trans <- list()
for(j in 1:60){ 
  df <- results %>% 
    filter(id %in% path_id[j])
  
  
  tran <- transmit_along_path(path = df, 
                              vel = 0.5, 
                              delayRng = c(60, 180), # min and max delay 
                              burstDur = 7
                              )
  tran <- tran %>% 
    mutate(
      sim_id = unique(df$sim_id), 
      theta = unique(df$theta), 
      stepLen = unique(df$stepLen), 
      basin = unique(df$basin), 
      id = unique(df$id)
    )
  trans[[j]] <- tran
  message('Processing sim_track ', j, " of ", 60)
  
  
}

trans_results <- bind_rows(trans)

sim <- vector("list", length = 60)

# Define detection range function (to pass as detRngFun) that returns detection probability for given distance
# assume logistic form of detection range curve where:
#   dm = distance in meters
#   b = intercept and slope
#   DETECTION RANGE ------
pdrf <- function(dm, b = c(1.8, -1 / 200)) {
  p <- 1 / (1 + exp(-(b[1] + b[2] * dm)))
  return(p)
}

pdrf(c(100, 200, 300, 400, 500)) #view detection probs. at some distances

trans_id <- unique(trans_results$id)

# loop through and calculate transmissions along each path
for(l in 1:60){
  sim_df <- trans_results %>% 
    filter(id %in% trans_id[l])
  
  sims <- detect_transmissions(trnsLoc = sim_df, recLoc = rl_sum_sp,
                               detRngFun = pdrf, show_progress = T)
  sims <- sims %>% 
    mutate(
      sim_id = unique(sim_df$sim_id), 
      theta = unique(sim_df$theta), 
      stepLen = unique(sim_df$stepLen), 
      basin = unique(sim_df$basin), 
      id = unique(sim_df$id)
      
    )
  sim[[l]] <- sims
}

sim_results <- bind_rows(sim)

sim_results <- sim_results %>% 
  mutate(
    deploy_lon = st_coordinates(.$rec_geometry)[,"X"],
    deploy_lat = st_coordinates(.$rec_geometry)[,"Y"],
    dist = geosphere::distHaversine(st_coordinates(.$rec_geometry), 
                                    st_coordinates(.$trns_geometry))
  )
start_time <- as.POSIXct("2022-01-01 00:00:00", 
                         format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# create new columns, set factors, and select final columns
sim_final <- sim_results %>% 
  mutate(
    detection_timestamp_utc = start_time + time,
    basin = factor(basin, level = c("East Basin", 
                                    "West Basin", 
                                    "North Basin"))
  ) %>% 
  separate(col = sim_id, into = c("sim", "animal"), sep = "_") %>% 
  mutate(
    animal = as.numeric(animal)
  )
  # separate_wider_delim(col = sim_id, names = c("sim", "animal"), delim = "_")
sim_final

glimpse(sim_final)

rl_sum <- rl_sum_rec_group %>% 
  mutate(
    deploy_lon = st_coordinates(.)[,"X"],
    deploy_lat = st_coordinates(.)[,"Y"],
  ) %>% 
  st_drop_geometry() %>% 
  select(-receiver_basin)


sim_final <- sim_final %>% 
  left_join(rl_sum, by = c("deploy_lon", "deploy_lat"))


  
st_geometry(sim_final) <- "trns_geometry"
 

sim_final


glimpse(sim_final)
ggplot() + 
  geom_sf(data = p_map) + 
  geom_sf(data = sim_final, 
          aes(colour  = as.factor(animal)), 
          size = 1, 
          # shape = 21, colour = "black", 
          # stroke = 0.5
          ) + 
  scale_fill_viridis_d(option = "C", alpha = 0.5) + 
  scale_colour_viridis_d(option = "C", alpha = 0.5, name = "Animal ID") + 
  facet_wrap(.~ basin) + 
  theme_bw(base_size = 15) +
  theme(
    panel.grid = element_blank(), 
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) + 
  labs(
    x = "Latitude", 
    y = "Longitude"
  ) -> p 
p
ggsave(here("Plots", 
            "simulated tracks", 
            "simulated_tracks.png"), plot = p, 
       height = 8.5, width = 11)
  
write_rds()


sim_final_trk <- sim_final %>% 
  group_by(animal, basin) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("MULTIPOINT") %>% 
  st_cast("LINESTRING")
sim_final_trk

ggplot() + 
  geom_sf(data = p_map) + 
  geom_sf(data = sim_final_trk, 
          aes(colour  = as.factor(animal)), 
          # size = 1, 
          # shape = 21, colour = "black", 
          # stroke = 0.5
  ) + 
  scale_fill_viridis_d(option = "C", alpha = 0.5) + 
  scale_colour_viridis_d(option = "C", alpha = 0.5, name = "Animal ID") + 
  facet_wrap(.~ basin) + 
  theme_bw(base_size = 15) +
  theme(
    panel.grid = element_blank(), 
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) + 
  labs(
    x = "Latitude", 
    y = "Longitude"
  ) -> p1
p1
ggsave(here("Plots", 
            "simulated tracks", 
            "simulated_tracks_trck.png"), plot = p1, 
       height = 8.5, width = 11)
