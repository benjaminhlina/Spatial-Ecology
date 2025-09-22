# ----------------------------------------
# Simulated data for model comparison
# 20 Jan 2022
# MHF
# ----------------------------------------
#
# Notes:
#  - 12/7/2022 updated file to double number of simulations for each region (10 sims to 20; 60 total sims)


###----------------------------------------------------------------------------------------------------
### Generate functions for standardizing datasets ####
###----------------------------------------------------------------------------------------------------

#### ---
# FUNCTION: %!in%
# description: negate based on values in a list
#### ---

`%!in%` <- Negate(`%in%`)

# end of %!in%
#### ---

####---
# FUNCTION: set_region
# description: set region for interpolated positions
#### ---

set_region <- function(data, regions_short, latitude, longitude){
  data <- data %>% 
    mutate(regions_short = case_when(is.na(regions_short) & 
                                       latitude > 44.835 & latitude < 44.970 &
                                       longitude > -73.298 & longitude < -73.226 ~ "NE_Channel",
                                     is.na(regions_short) & 
                                       latitude > reg_bbox$ymax[reg_bbox$region == "Main_Central"] &
                                       longitude < -73.314 ~ "Main_North",
                                     is.na(regions_short) &
                                       latitude > 44.550 & latitude < 44.628 & 
                                       longitude > -73.314 ~ "Malletts",
                                     is.na(regions_short) &
                                       latitude > 44.628 & latitude < 44.922 &
                                       longitude > -73.291 ~ "Inland_Sea",
                                     is.na(regions_short) &
                                       latitude > 44.973 & longitude > -73.227 ~ "Missisquoi",
                                     is.na(regions_short) & 
                                       latitude > reg_bbox$ymax[reg_bbox$region == "Main_South"] ~ "Main_Central",
                                     is.na(regions_short) & 
                                       latitude >= reg_bbox$ymin[reg_bbox$region == "Main_South"] ~ "Main_South",
                                     is.na(regions_short) & 
                                       latitude <= reg_bbox$ymin[reg_bbox$region == "Main_South"] ~ "South_Lake",
                                     !is.na(regions_short) ~ regions_short)) %>% 
    mutate(area_sqkm = case_when(is.na(area_sqkm) & regions_short %in% "Missisquoi" ~ 80.459606,
                                 is.na(area_sqkm) & regions_short %in% "NE_Channel" ~ 28.734136,
                                 is.na(area_sqkm) & regions_short %in% "Main_North" ~ 351.817017,
                                 is.na(area_sqkm) & regions_short %in% "Main_Central" ~ 201.214557,
                                 is.na(area_sqkm) & regions_short %in% "Main_South" ~ 145.399656,
                                 is.na(area_sqkm) & regions_short %in% "Inland_Sea" ~ 217.102513,
                                 is.na(area_sqkm) & regions_short %in% "Malletts" ~ 54.811470,
                                 is.na(area_sqkm) & regions_short %in% "South_Lake" ~ 56.971202,
                                 !is.na(area_sqkm) ~ area_sqkm)) %>% 
    mutate(regions_short = factor(regions_short,
                                  levels = c('Missisquoi','NE_Channel','Gut','Inland_Sea','Main_North','Main_Central','Main_South','Malletts', 'South_Lake'),
                                  labels = c('Missisquoi','NE Channel','Gut','Inland Sea','Main North','Main Central','Main South','Malletts', 'South Lake')))
}

# end of set_region
#### ---
###----------------------------------------------------------------------------------------------------


###----------------------------------------------------------------------------------------------------
### Load packages ####
###----------------------------------------------------------------------------------------------------
library(sp)
library(sf)
# library(remotes) 
# remotes::install_github('ocean-tracking-network/glatos', build_vignettes = TRUE)
library(glatos)
# install.packages("devtools")
# devtools::install_github("rossdwyer/VTrack")
library(VTrack)
# remotes::install_github("YuriNiella/RSP") 
library(RSP)
library(actel)
library(magrittr)
library(data.table)
library(dplyr)
###----------------------------------------------------------------------------------------------------


###----------------------------------------------------------------------------------------------------
### Identify path roots ####
###----------------------------------------------------------------------------------------------------
path.drive <- "C:/Users/mttft/OneDrive - University of Vermont/"
path.root <- paste0(path.drive, "Dissertation_Final/Telemetry/")
path.mvt <- paste0(path.root, "ModelComparison/")
path.detect <- paste0(path.mvt, "MergedData_MC/")
path.sim <- paste0(path.mvt, "SimulatedData_MC/")
path.rec <- paste0(path.mvt, "RecLocs_MC/")
path.filt <- paste0(path.mvt, "FilteredTelemetryData_MC/")
path.shp <- paste0(path.drive,"ArcGIS Pro 2.2/Shapefiles/LakeChamplain/")
###----------------------------------------------------------------------------------------------------


###----------------------------------------------------------------------------------------------------
### Load receiver data ####
###----------------------------------------------------------------------------------------------------
# load receiver locations
recs <- readRDS(paste0(path.rec,"TotalReceiverSummary_2013-2020_1.rds"))

str(recs)

# create new column for 
recs_og <- recs %>% 
  mutate(regions = case_when(StationName %in% "Crown Point" ~ "South Lake",
                             deploy_lat > 44.10 & deploy_lat < 44.40 & Tributary %in% "Main Lake" ~ "Main Lake South",
                             deploy_lat > 44.40 & deploy_lat < 44.56 & Tributary %in% "Main Lake" ~ "Main Lake Central",
                             deploy_lat > 44.56 & Tributary %in% "Main Lake" ~ "Main Lake North",
                             Tributary %!in% "Main Lake" ~ Tributary,
                             TRUE ~ "regions")) %>% 
  filter(deploy_date_time <= "2018-01-01" &
         StationName %!in% c("BurlingtonBay_RangeTest","SandBarInlandSea_RangeTest")) %>% 
  mutate(regions = factor(regions),
         StationName = factor(StationName),
         deploy_long = abs(deploy_long)*(-1),
         Tributary = NULL) %>% 
  mutate(regions = as.character(regions)) %>% 
  mutate(regions_short = case_when(regions %in% c("Carry Bay","Missisquoi") ~ "NE Channel",
                                   regions %in% "Northwest Arm" ~ "Main Lake North",
                                   regions %!in% c("Carry Bay","Missisquoi","Northwest Arm") ~ regions,
                                   TRUE ~ "regions_short")) %>% 
  mutate(regions = factor(regions),
         regions_short = factor(regions_short,
                                levels = c('NE Channel','Gut','Inland Sea','Main Lake North','Main Lake Central','Main Lake South','Malletts', 'South Lake'),
                                labels = c('NE Channel','Gut','Inland Sea','Main North','Main Central','Main South','Malletts', 'South Lake')))


# Corrections to recs file
recs_og$recover_date_time[recs_og$StationName %in% "Gut" & recs_og$recover_date_time == "2017-05-05"] <- "2017-06-23" # change recover time due to later detection and unknown recover date in catos log
recs_og$deploy_date_time[recs_og$StationName %in% "Gordon" & recs_og$deploy_date_time == "2017-07-26"] <- "2016-11-02" # change deploy date due to prior failed retrieval


# summarize receiver locations
recs_short <- recs_og %>% 
  group_by(StationName,regions_short) %>% 
  summarize(deploy_lon = mean(deploy_long), deploy_lat = mean(deploy_lat)) %>% 
  ungroup()


# convert to sp object
recs_sf <- st_as_sf(x = recs_short, coords = c('deploy_lon', 'deploy_lat'), 
                    crs = 4326)

recs_sp <- as(recs_sf, "Spatial")

###----------------------------------------------------------------------------------------------------


###----------------------------------------------------------------------------------------------------
### Load lake map ####
###----------------------------------------------------------------------------------------------------
setwd(paste(path.shp, "ChamplainOutline/",sep = ""))
lc_outline <- st_read(dsn = ".",
                      layer = "")

lc.trans <- make_transition3(lc_outline, res = c(0.001,0.001)) # Create transition layer of the lake

lc_spat <- as(lc_outline, "Spatial")

champ <- st_as_sf(lc_outline)
st_crs(champ) <- 4326


# load lake region polygon
setwd(paste0(path.shp, "ChamplainRegions/"))
lc_regions <- st_read(dsn = ".",layer = "ChamplainRegions")
###----------------------------------------------------------------------------------------------------


###----------------------------------------------------------------------------------------------------
### Create simulated data ####
###----------------------------------------------------------------------------------------------------
### Create 100 file names for 100 simulated paths
# Five starting locations with 20 replicates per location
# Possibility for biased random walk? known spawning sites

# example simulation code
foo <- crw(theta=c(0,25), stepLen=10, initPos=c(0,0), initHeading=0, 
           nsteps=10)
plot(foo,type="o",pch=20,asp=c(1,1))


# create vector to hold simulated paths
paths <- vector("list", length = 100)

# create metadata table to capture simulation specs
metadata <- data.frame(matrix(ncol = 4, nrow = 100))
colnames(metadata) <- c("sim_id", "theta", "stepLen", "start_region")

### Inland Sea simulations
for (i in 1:20) {
  angle = sample(c(25:100),1)
  length = sample(250:1000,1)
  
  paths[[i]] <- crw_in_polygon(polyg = lc_spat,
                               theta = c(0, angle),
                               stepLen = length,
                               initPos = c(-73.228,44.924),
                               nsteps = 5000,
                               cartesianCRS = 3175,
                               sp_out = T)
  
  metadata$sim_id <- paste0("sim_",i)
  metadata$theta[i] <- angle
  metadata$stepLen[i] <- length
  metadata$start_region[i] <- "Inland_Sea"
  
  # trails <- crw_in_polygon(polyg = lc_spat,
  #                              theta = angle c(0, 25),
  #                              stepLen = length,
  #                              initPos = c(-73.228,44.924),
  #                              nsteps = 5000,
  #                              cartesianCRS = 3175,
  #                              sp_out = T)
  # 
  # paths[[i]] = list(metadata = c(theta = angle, stepLen = length), path = trails)
}
# 
# write_rds(metadata, here("Saved Data", 
#                          "Simulated_tracks_pap.rds"))
# write_rds(paths, here("Saved Data", 
#                          "Simulated_tracks_sf.rds"))
# 
# metadata
# 
# path_sf <- bind_rows(.id = "id", paths) %>% 
#   mutate(
#     id = as.numeric(id)
#   )
# 
# path_sf
# 
# path_sf <- path_sf %>% 
#   group_by(id) %>% 
#   summarise(do_union = FALSE) %>% 
#   st_cast("MULTIPOINT") %>% 
#   st_cast("LINESTRING")
# 
# write_rds(path_sf, here("Saved Data", 
#                       "Simulated_tracks_sf.rds"))
# 
# 
# ggplot() + 
#   geom_sf(data = lake_wgs) + 
#   geom_sf(data = path_sf)

### Main North simulations
for (i in 21:40) {
  angle = sample(c(25:100),1)
  length = sample(250:1000,1)
  
  paths[[i]] <- crw_in_polygon(polyg = lc_spat,
                               theta = c(0, angle),
                               stepLen = length,
                               initPos = c(-73.352,44.689),
                               nsteps = 5000,
                               cartesianCRS = 3175,
                               sp_out = T)
  
  metadata$sim_id <- paste0("sim_",i)
  metadata$theta[i] <- angle
  metadata$stepLen[i] <- length
  metadata$start_region[i] <- "MainLake_North"
}

### Main Central simulations
for (i in 41:60) {
  angle = sample(c(25:100),1)
  length = sample(250:1000,1)
  
  paths[[i]] <- crw_in_polygon(polyg = lc_spat,
                               theta = c(0, angle),
                               stepLen = length,
                               initPos = c(-73.23214, 44.47108),
                               nsteps = 5000,
                               cartesianCRS = 3175,
                               sp_out = T)
  
  metadata$sim_id <- paste0("sim_",i)
  metadata$theta[i] <- angle
  metadata$stepLen[i] <- length
  metadata$start_region[i] <- "MainLake_Central"
}


# Main South simulations
for (i in 61:80) {
  angle = sample(c(25:100),1)
  length = sample(250:1000,1)
  
  paths[[i]] <- crw_in_polygon(polyg = lc_spat,
                               theta = c(0, angle),
                               stepLen = length,
                               initPos = c(-73.39258, 44.44039),
                               nsteps = 5000,
                               cartesianCRS = 3175,
                               sp_out = T)
  
  metadata$sim_id <- paste0("sim_",i)
  metadata$theta[i] <- angle
  metadata$stepLen[i] <- length
  metadata$start_region[i] <- "MainLake_South"
}


# Malletts Bay simulations
for (i in 81:100) {
  angle = sample(c(25:100),1)
  length = sample(250:1000,1)
  
  paths[[i]] <- crw_in_polygon(polyg = lc_spat,
                               theta = c(0, angle),
                               stepLen = length,
                               initPos = c(-73.23591, 44.60510),
                               nsteps = 5000,
                               cartesianCRS = 3175,
                               sp_out = T)
  
  metadata$sim_id[i] <- paste0("sim_",i)
  metadata$theta[i] <- angle
  metadata$stepLen[i] <- length
  metadata$start_region[i] <- "Malletts_Bay"
}

### Convert list of simulated tracks to data.table
# make empty list to hold results
trans <- vector("list", length = 100)

# loop through and calculate transmissions along each path
for(j in 1:100){
  trans[[j]] <- transmit_along_path(paths[[j]], vel = 0.5, delayRng = c(60,180), burstDur = 7)
}

# make empty list to hold results
sim <- vector("list", length = 100)

# Define detection range function (to pass as detRngFun) that returns detection probability for given distance
# assume logistic form of detection range curve where:
#   dm = distance in meters
#   b = intercept and slope
pdrf <- function(dm, b=c(1.8, -1/200)){
  p <- 1/(1+exp(-(b[1]+b[2]*dm)))
  return(p)
}

pdrf(c(100,200,300,400,500)) #view detection probs. at some distances

# loop through and calculate transmissions along each path
for(k in 1:100){
  sim[[k]] <- detect_transmissions(trans[[k]], recLoc = recs_sp,
                                   detRngFun = pdrf, show_progress = T)
}



# convert each simulated object into a data.frame
sim_df <- lapply(sim, as.data.frame)

# combine list into a single data.table object
sim_df <- rbindlist(sim_df, fill = TRUE, idcol = "virt_fish")

# assign initial regions to each simulation (virt_fish)
sim_df <- sim_df %>% 
  mutate('start_region' = case_when(virt_fish %in% 1:20 ~ "Inland_Sea",
                                    # virt_fish %in% 21:40 ~ "MainLake_North",
                                    # virt_fish %in% 41:60 ~ "MainLake_Central",
                                    # virt_fish %in% 61:80 ~ "MainLake_South",
                                    # virt_fish %in% 81:100 ~ "Malletts_Bay",
                                    TRUE ~ 'start_region'))

# create columns for receiver coordinates in simulated dataframe
sim_recs <- data.frame(st_coordinates(sim_df$rec_geometry)) %>% 
  rename("deploy_lon" = X,
         "deploy_lat" = Y)

sim_df <- bind_cols(sim_df, sim_recs)

# create columns for actual coordinates of simulated movements
sim_dets <- data.frame(st_coordinates(sim_df$trns_geometry)) %>% 
  rename("true_lon" = X,
         "true_lat" = Y)

sim_df <- bind_cols(sim_df, sim_dets)


# determine distance between receivers and simulated detections
sim_df$dist <- geosphere::distHaversine(st_coordinates(sim_df$rec_geometry),st_coordinates(sim_df$trns_geometry))


### Structure sim data as vemco detection file
# Assign StationNames to detections
sim_full <- left_join(sim_df, recs_short)


### reformat sim_full dataframe
# create initial period to build detection timestamp from
start_time <- as.POSIXct("2022-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# create new columns, set factors, and select final columns
sim_final <- sim_full %>% 
  mutate(detection_timestamp_utc = start_time + time,
         animal_id = paste("sim", virt_fish, sep = "_")) %>% 
  mutate(animal_id = factor(animal_id),
         start_region = factor(start_region)) %>% 
  select(animal_id,start_region,deploy_lon,deploy_lat,true_lon,true_lat,StationName,detection_timestamp_utc,trns_id)
  

# ### calculate average rate of movement
# # test2.2 <- data.frame(trans[2])
# 
# sim_final <- sim_final %>%
#   mutate(time_step = detection_timestamp_utc - lag(detection_timestamp_utc)) %>%
#   mutate(dist = c(NA, geosphere::distVincentyEllipsoid(cbind(true_lon,true_lat)))) %>%
#   mutate(rate_kmh = (dist*1000)/(as.numeric(time_step)*360))
# 
# max_rate <- sim_final %>% 
#   group_by(animal_id) %>% 
#   summarize(max = max(rate_kmh, na.rm = T))


### create new file for complete dataset
sim_full_list <- lapply(trans, as.data.frame)

sim_full_df <- rbindlist(sim_full_list, fill = TRUE, idcol = "virt_fish")

# Save simulated data as RDS fild
write_rds(sim_final, paste0(path.sim, "simulated_detections_Apr2023.rds"))

write_rds(sim_full_df, paste0(path.sim,"complete_simulated_detections_Apr2023.rds"))

write_rds(metadata, paste0(path.sim, "simulations_metadata_Apr2023.rds"))
###----------------------------------------------------------------------------------------------------


###----------------------------------------------------------------------------------------------------
### Plot simulated paths
###----------------------------------------------------------------------------------------------------
### for loop for glatos abacus plots
# create list of unique ids
animal <- as.character(unique(sim_final$animal_id))

# create recs dataframe sorted by latitude
recs_abacus <- recs_short %>% 
  arrange(deploy_lat) %>% 
  mutate(StationName = as.character(StationName)) %>% 
  na.omit()

a <- 1

for(a in 1:20){
  abacus_plot(det = sim_final[sim_final$animal_id == animal[a]],
              location_col = "StationName",
              locations = recs_abacus$StationName,
              out_file = paste(path.sim,"AbacusPlots/Sim_",a, ".png", sep = ""))
}
###----------------------------------------------------------------------------------------------------



###----------------------------------------------------------------------------------------------------
### Calculate actual time spent in regions
###----------------------------------------------------------------------------------------------------
### Determine actual time in regions from paths file
# load data

sim_comp <- readRDS(paste0(path.sim,"complete_simulated_detections_100x5000.rds"))

str(sim_comp)

# create columns for coordinates
true_coords <- data.frame(st_coordinates(sim_comp$geometry)) %>% 
  rename("true_lon" = X,
         "true_lat" = Y)

sim_comp_full <- bind_cols(sim_comp, true_coords)

# assign true positions to regions
true_sf <- st_as_sf(sim_comp_full,
                    coords = c("true_lon","true_lat"),
                    crs=4326,
                    remove = F)

# add region to point data
true_sf_regions <- st_join(true_sf,
                          lc_regions[c("GNIS_NAME","AREASQKM")],
                          left = T) %>% 
  rename("basins" = GNIS_NAME,
         "area_sqkm" = AREASQKM)

# assign interpolated points with NA region to regions using set_region function
true_sf_regions <- set_region(data = true_sf_regions,
                              regions_short = true_sf_regions$basins,
                              latitude = true_sf_regions$true_lat,
                              longitude = true_sf_regions$true_lon)

# calculate true regional use
true_use <- data.frame(true_sf_regions) %>% 
  group_by(virt_fish) %>% 
  mutate(total_positions = as.numeric(length(geometry))) %>% 
  ungroup() %>% 
  group_by(virt_fish,regions_short,total_positions,
           .drop = F) %>% 
  summarize(total_region = as.numeric(length(geometry))) %>% 
  mutate(percent_region = total_region*100/total_positions) %>% 
  mutate(total_positions = if_else(is.na(total_positions),0,total_positions),
         total_region = if_else(is.na(total_region),0,total_region),
         percent_region = if_else(is.na(percent_region),0,percent_region)) %>% 
  ungroup()

# save data
saveRDS(true_use,
        file = paste0(path.sim,"simulated_true_data_Feb2023.rds"))
###----------------------------------------------------------------------------------------------------


###----------------------------------------------------------------------------------------------------
### Generate data frames to use for each model
###----------------------------------------------------------------------------------------------------
### detection data
# load data
sim_data <- readRDS(paste0(path.sim, "simulated_detections_100x5000.rds"))

# Calculate the elapsed time (seconds) between detections
sim_data <- sim_data %>% 
  group_by(animal_id) %>% 
  arrange(detection_timestamp_utc) %>%
  mutate(diff = detection_timestamp_utc - lag(detection_timestamp_utc, default = first(detection_timestamp_utc))) %>% 
  ungroup()

# remove duplicated detections
sim_reduce <- sim_data %>%
  filter(diff > 60 | trns_id %in% 1)

# remove detections at tributary receivers
sim_reduce <- sim_reduce %>% 
  filter(StationName %!in% c("Lamoille River Lower", "Otter Creek Lower", "Otter Creek Upper", "Winooski River Upper", "Winooski River Lower"))

str(sim_reduce)

### Assign points to regions
sim_sf <- st_as_sf(sim_reduce,
                    coords = c("deploy_lon","deploy_lat"),
                    crs=4326,
                    remove = F)

# add region to point data
sim_sf_regions <- st_join(sim_sf,
                           lc_regions[c("GNIS_NAME","AREASQKM")],
                           left = T) %>% 
  rename("basins" = GNIS_NAME,
         "area_sqkm" = AREASQKM)

# assign NA basins to correct regions
sim_sf_regions_set <- set_region(data = sim_sf_regions,
                                 regions_short = sim_sf_regions$basins,
                                 latitude = sim_sf_regions$deploy_lat,
                                 longitude = sim_sf_regions$deploy_lon)


# convert to data.frame and create columns
sim_data_df <- data.frame(sim_sf_regions_set) %>% 
  mutate(transmitter = paste0("trans_",animal_id),
         receiver_sn = paste0(StationName,"_sn"))


### GLATOS
sim_glatos <- sim_data_df %>% 
  select(StationName,receiver_sn,detection_timestamp_utc,deploy_lat,deploy_lon,regions_short,animal_id, transmitter) %>% 
  rename("glatos_array" = StationName,
         "deploy_long" = deploy_lon)


### Vtrack
sim_vTrack <- sim_data_df %>% 
  group_by(animal_id) %>% 
  mutate(cap_date = as.Date(min(detection_timestamp_utc)),
         cap_location = start_region) %>% 
  select(animal_id, transmitter, cap_date, cap_location) %>% 
  unique() 

# setup VTrack data
 # trace(setupData, edit = T) # corrected code commented out at end of document
sim_ATTdata <- VTrack::setupData(Tag.Detections = sim_data_df, 
                                 Tag.Metadata = sim_vTrack, 
                                 Station.Information = recs_og, 
                                 source = "VEMCO")

# view data
sim_ATTdata$Tag.Detections %>% View
sim_ATTdata$Tag.Metadata %>% View
sim_ATTdata$Station.Information %>% View


### RSP


# save all data
# saveRDS(sim_data_df, file = paste0(path.sim, "sim_detections_Feb2023.rds"))
# saveRDS(sim_ATTdata, file = paste0(path.sim, "sim_coa_detections_Feb2023.rds"))
# saveRDS(sim_glatos, file = paste0(path.sim, "sim_glatos_detections_Feb2023.rds"))

###----------------------------------------------------------------------------------------------------


###----------------------------------------------------------------------------------------------------
### Model 1: Residency Index (personal and GLATOS)
###----------------------------------------------------------------------------------------------------
### load original data
sim_data_df <- readRDS(paste0(path.sim,"sim_detections_Feb2023.rds")) 

### calculate percent of all detections detected at each region for each fish
# Count total detections in each basin for each fish and season*year
sim_basin_occ <- sim_data_df %>%
  group_by(animal_id, regions_short, .drop = F) %>%
  summarize(region_detect = length(detection_timestamp_utc)) %>%
  ungroup() %>% 
  unique() 


# calculate total number of detections for each virtual id
sim_total_detect <- aggregate(region_detect~animal_id, data = sim_basin_occ, FUN = sum) %>% 
  rename("total_detect" = region_detect)

# calculate average receiver latitude
sim_rec_lat <- aggregate(deploy_lat~regions_short, data = sim_data_df, FUN = mean)

# merge number of detections for each fish in each basin with total detections for each fish 
sim_basin_occ_full <- left_join(sim_basin_occ, sim_total_detect) %>% 
  left_join(sim_rec_lat)

# calculate seasonal percentage of detections in each basin for each fish
sim_basin_occ_full <- sim_basin_occ_full %>% 
  mutate(region_percent = round((region_detect*100/total_detect), digits = 1)) %>% 
  mutate(region_percent = if_else(region_percent %in% NaN, 0, region_percent))


str(sim_basin_occ_full)



### create new summarized dataframe
sim_base_percent <- sim_basin_occ_full %>% 
  select(region_percent, regions_short) %>%
  group_by(regions_short) %>%
  summarise_all(list(mean, sd)) %>% 
  rename("mean_base" = fn1,
         "sd_base" = fn2)



### save final dataframes (full and summary data)
# save(sim_basin_occ_full,sim_base_percent,
#      file = paste0(path.sim, "simulated_base_data_Feb2023.Rdata"))

# load(file = paste0(path.sim, "simulated_base_data_Feb2023.Rdata"), verbose = T)


# ### Figure 1a: Bar graph - Regional percent occupancy based on true detections alone for season*year by individual
# fig1 <- region_barplot(data = sim_basin_occ_full, 
#                        tag_name = sim_basin_occ_final$animal_id, 
#                        plot_title = "Raw Detections")
# 
# fig1
# 
# ggsave(filename = paste0(path.figs, "Simulated_BaseData100_PercentOccurrence_Individual_Feb2023.svg"),
#        plot = fig1,
#        width = 15,
#        height = 8)
# 
# ### Figure 2a: Scatterplot - Regional percent occurrence based on all detections for each season
# fig2 <- region_boxplot(data = basin_occ_final,
#                        plot_title = "Raw Detections")
# 
# fig2
# 
# ggsave(filename = paste0(path.figs, "Simulated_BaseData100_PercentOccurrence_Boxplot_Feb2023.svg"),
#        plot = fig2,
#        scale = 1.5)
###----------------------------------------------------------------------------------------------------


###----------------------------------------------------------------------------------------------------
### Model 2: Centers of Activity
###----------------------------------------------------------------------------------------------------
### Data preparation
# Load vTrack data
sim_ATTdata <- readRDS(paste0(path.sim,"sim_coa_detections_Feb2023.rds"))

# Run COA for 60 minute timesteps
sim_COA_60 <- COA(sim_ATTdata, timestep = 60)

# Wrangle df to change column names and remove empty columns
sim_COA_60_ref <- sim_COA_60 %>% 
  rename("animal_id" = Tag.ID,
         "latitude" = Latitude.coa,
         "longitude" = Longitude.coa,
         "stations_n" = Number.of.Stations,
         "detect_n" = Number.of.Detections) %>% 
  mutate(animal_id = factor(animal_id),
         Sensor.Unit = NULL,
         Sensor.Value.coa = NULL,
         Sci.Name = NULL,
         Common.Name = NULL,
         Tag.Project = NULL,
         Release.Latitude = NULL,
         Release.Longitude = NULL,
         Tag.Life = NULL,
         Tag.Status = NULL,
         Bio = NULL,
         Sex = NULL)

str(sim_COA_60_ref)

summary(LT_COA_60_ref)


### Assign COAs to regions
sim_coa_sf <- st_as_sf(sim_COA_60_ref,
                   coords = c("longitude","latitude"),
                   crs=4326,
                   remove = F)

# add region to point data
sim_coa_sf_regions <- st_join(sim_coa_sf,
                          lc_regions[c("GNIS_NAME","AREASQKM")],
                          left = T) %>% 
  rename("regions_short" = GNIS_NAME,
         "area_sqkm" = AREASQKM) 


### Correct NA values for area and area_sqkm for points on land
# # identify locations of NA positions
# zeros <- coa_sf_regions %>% 
#   filter(is.na(regions_short)) %>% 
#   select(geometry)
# 
# zeros_coords <- data.frame(st_coordinates(zeros$geometry)) %>%
#   mutate(X = round(X,2),
#          Y = round(Y, 2)) %>% 
#   unique()

# assign correct area for NA positions using set_regions function
sim_coa_sf_regions <- set_region(data = sim_coa_sf_regions)

# convert to data.frame
sim_coa_df_regions <- data.frame(sim_coa_sf_regions)

str(sim_coa_df_regions)

summary(sim_coa_df_regions)

### calculate percent of all COAs located in each region for each fish
# Count total COAs in each basin for each fish and season*year
sim_basin_coa <- sim_coa_df_regions %>%
  group_by(animal_id, regions_short, .drop = F) %>%
  summarize(region_detect = length(TimeStep.coa)) %>%
  ungroup() %>% 
  unique() 

# calculate total number of detections for each transmitter
sim_total_detect_coa <- aggregate(region_detect~animal_id, data = sim_basin_coa, FUN = sum) %>% 
  rename("total_detect" = region_detect)

# calculate average receiver latitude
sim_rec_lat_coa <- aggregate(latitude~regions_short, data = sim_coa_sf_regions, FUN = mean)

# merge number of detections for each fish in each basin with total detections for each fish 
sim_basin_coa_full <- left_join(sim_basin_coa, sim_total_detect_coa) %>% 
  left_join(sim_rec_lat_coa)

# calculate seasonal percentage of detections in each basin for each fish
sim_basin_coa_full <- sim_basin_coa_full %>% 
  mutate(region_percent = round((region_detect*100/total_detect), digits = 1)) %>% 
  mutate(region_percent = if_else(region_percent %in% NaN, 0, region_percent))


### create new summarized dataframe
sim_coa_percent <- sim_basin_coa_full %>% 
  select(region_percent, regions_short) %>%
  group_by(regions_short) %>%
  summarise_all(list(mean, sd)) %>% 
  rename("mean_coa" = fn1,
         "sd_coa" = fn2)


# ### save final dataframes (full and summary data)
# save(sim_basin_coa_full,sim_coa_percent,
#      file = paste0(path.sim, "simulated_COA_data_Feb2023.Rdata"))
# 
# load(file = paste0(path.sim, "simulated_COA_data_Feb2023.Rdata"), verbose = T)
# 
# 
# ### Figure 3: Bar graph - Regional percent occupancy based on Centers of Activity for season*year by individual
# fig3 <- region_barplot(data = basin_coa_final,
#                        tag_name = basin_coa_final$animal_id,
#                        plot_title = "COA Detections")
# 
# fig3
# 
# ggsave(filename = paste0(path.figs, "COA_PercentOccurrence_Individual_Feb2023.svg"),
#        plot = fig3,
#        width = 15,
#        height = 8)
# 
# ### Figure 4: Scatterplot - Regional percent occurrence based on Centers of Activity for each season
# fig4 <- region_boxplot(data = basin_coa_final,
#                        plot_title = "COA Detections")
# 
# fig4
# 
# ggsave(filename = paste0(path.figs,"COA_PercentOccurrence_Boxplot_Feb2023.svg"),
#        plot = fig4,
#        scale = 1.5)
###----------------------------------------------------------------------------------------------------


###----------------------------------------------------------------------------------------------------
### Model 3: Standardized detections: GLATOS non-linear interpolated path
###----------------------------------------------------------------------------------------------------
# Load data
sim_glatos <- readRDS(file = paste0(path.sim, "sim_glatos_detections_Feb2023.rds"))


### Interpolate path for each year with 30 min (1800 sec) & 60 min (3600 sec) timestamp
sim_glatos_60 <- interpolate_path(sim_glatos,
                                 trans = lc.trans$transition,
                                 int_time_stamp = 3600,
                                 lnl_thresh = 0.999)

sim_NLpos_60 <- setDT(sim_glatos_60)
sim_int_60 <- unique(sim_NLpos_60, by = c("bin_timestamp", "animal_id"))


### Assign interpolated points to regions
sim_int_sf <- st_as_sf(sim_int_60,
                       coords = c("longitude","latitude"), 
                       crs = 4326,
                       remove = F)

# add region to point data
sim_int_sf_regions <- st_join(sim_int_sf,
                              lc_regions[c("GNIS_NAME","AREASQKM")],
                              left = T) %>% 
  rename("regions_short" = GNIS_NAME,
         "area_sqkm" = AREASQKM) 

# assign interpolated points with NA region to regions using set_region function
sim_int_sf_regions <- set_region(data = sim_int_sf_regions)

# convert to data frame and edit data
sim_int_df_full <- data.frame(sim_int_sf_regions) %>% 
  mutate(animal_id = factor(animal_id))

str(sim_int_df_full)

summary(sim_int_df_full)

### calculate percent of all detections located in each region for each fish
# Count total detections in each basin for each fish and season*year
sim_basin_glatos <- sim_int_df_full %>%
  group_by(animal_id, regions_short, .drop = F) %>%
  summarize(region_detect = length(bin_timestamp)) %>%
  ungroup() %>% 
  unique() 

# calculate total number of detections for each transmitter
sim_total_detect_glatos <- aggregate(region_detect~animal_id, data = sim_basin_glatos, FUN = sum) %>% 
  rename("total_detect" = region_detect)

# calculate average receiver latitude
sim_rec_lat_glatos <- aggregate(latitude~regions_short, data = sim_int_df_full, FUN = mean)

# merge number of detections for each fish in each basin with total detections for each fish 
sim_basin_glatos_full <- left_join(sim_basin_glatos, sim_total_detect_glatos) %>% 
  left_join(sim_rec_lat_glatos)

# calculate seasonal percentage of detections in each basin for each fish
sim_basin_glatos_full <- sim_basin_glatos_full %>% 
  mutate(region_percent = round((region_detect*100/total_detect), digits = 1)) %>% 
  mutate(region_percent = if_else(region_percent %in% NaN, 0, region_percent))


### create new summarized dataframe
sim_glatos_percent <- sim_basin_glatos_full %>% 
  select(region_percent, regions_short) %>%
  group_by(regions_short) %>%
  summarise_all(list(mean, sd)) %>% 
  rename("mean_glatos" = fn1,
         "sd_glatos" = fn2)


### save final dataframes (full and summary data)
# save(sim_basin_glatos_full,sim_glatos_percent,
#      file = paste0(path.sim, "simulated_GLATOS_data_Feb2023.Rdata"))

# load(file = paste0(path.sim, "simulated_GLATOS_data_Feb2023.Rdata"), verbose = T)
# 
# 
# ### Figure 5: Bar graph - Regional percent occupancy based on GLATOS interpolated data for season*year by individual
# fig5 <- region_barplot(data = basin_glatos_final,
#                        tag_name = basin_glatos_final$animal_id,
#                        plot_title = "GLATOS Detections")
# 
# fig5
# 
# ggsave(filename = paste0(path.figs, "GLATOS_PercentOccurrence_Individual_Feb2023.svg"),
#        plot = fig5,
#        width = 15,
#        height = 8)
# 
# ### Figure 6: Scatterplot - Regional percent occurrence based on LATOS interpolated data for each season
# fig6 <- region_boxplot(data = basin_glatos_final,
#                        plot_title = "GLATOS Detections")
# 
# fig6
# 
# ggsave(filename = paste0(path.figs,"GLATOS_PercentOccurrence_Boxplot_Feb2023.svg"),
#        plot = fig6,
#        scale = 1.5)
###----------------------------------------------------------------------------------------------------


###----------------------------------------------------------------------------------------------------
### Percent Occupancy
###----------------------------------------------------------------------------------------------------
### load data
load(file = paste0(path.sim, "simulated_base_data_Feb2023.Rdata"), verbose = T)
load(file = paste0(path.sim, "simulated_COA_data_Feb2023.Rdata"), verbose = T)
load(file = paste0(path.sim, "simulated_GLATOS_data_Feb2023.Rdata"), verbose = T)
readRDS(file = paste0(path.sim,"simulated_true_data_Feb2023.rds"))

### Combine percent occurance among methods for each season_year 
# rename columns for each model dataframe
sim_basin_occ_join <- sim_basin_occ_full %>% 
  rename("region_percent_occ" = region_percent,
         "region_detect_occ" = region_detect) %>% 
  select(animal_id,regions_short,region_percent_occ, region_detect_occ)

sim_basin_coa_join <- sim_basin_coa_full %>% 
  rename("region_percent_coa" = region_percent, 
         "region_detect_coa" = region_detect) %>% 
  select(animal_id,regions_short,region_percent_coa, region_detect_coa)

sim_basin_glatos_join <- sim_basin_glatos_full %>% 
  rename("region_percent_glatos" = region_percent,
         "region_detect_glatos" = region_detect) %>% 
  select(animal_id,regions_short,region_percent_glatos, region_detect_glatos)

sim_basin_true_join <- true_use %>% 
  filter(regions_short %!in% "Missisquoi") %>% 
  mutate(regions_short = factor(regions_short),
         animal_id = factor(paste("sim", virt_fish ,sep = "_"))) %>% 
  rename("region_percent_true" = percent_region,
         "region_detect_true" = total_region) %>% 
  select(animal_id,regions_short,region_percent_true, region_detect_true)

# create new dataframe with data from all models
sim_model_join <- list(sim_basin_occ_join, sim_basin_coa_join, sim_basin_glatos_join, sim_basin_true_join) %>% 
  reduce(left_join)

# remove regions that were never visited by fish
sim_model_join_short <- filter(sim_model_join, region_percent_true %!in% 0)

# transmute dataframe to long format
sim_model_join_long <- sim_model_join_short %>% 
  pivot_longer(cols = region_percent_occ:region_detect_true, 
               names_to = c("model"),
               values_to = "region_detect") %>% 
  mutate(region_percent = if_else(model %in% c("region_percent_occ","region_percent_coa","region_percent_glatos","region_percent_true"), 
                                  region_detect,
                                  NULL),
         region_detect = if_else(model %!in% c("region_percent_occ","region_percent_coa","region_percent_glatos","region_percent_true"), 
                                 region_detect,
                                 NULL)) %>% 
  mutate(model = factor(model,
                        levels = c("region_detect_occ","region_detect_coa","region_detect_glatos","region_detect_true",
                                   "region_percent_occ","region_percent_coa","region_percent_glatos","region_percent_true"),
                        labels = c("base","coa","glatos","true","base","coa","glatos","true")))

sim_mj1 <- sim_model_join_long %>% 
  select(-region_percent) %>% 
  filter(!is.na(region_detect))


sim_mj2 <- sim_model_join_long %>% 
  select(-region_detect) %>% 
  filter(!is.na(region_percent))

sim_model_join_final <- left_join(sim_mj1,sim_mj2)


summary(sim_model_join_final$region_detect)

# create new column for scaled detections based on max and min detections per season and model
aggregate(region_detect~model, data = sim_model_join_final, FUN = max)

sim_model_join_final <- sim_model_join_final %>% 
  group_by(model) %>% 
  mutate(region_scale = (38000/max(region_detect))*(subtract(region_detect,max(region_detect)))+38000,
         region_proportion = region_percent/100) %>% 
  ungroup()

# compare average detections between original and scaled data
sim_obs <- aggregate(region_detect~model, data = sim_model_join_final, FUN = mean)

sim_obs_scaled <- aggregate(region_scale~model, data = sim_model_join_final, FUN = mean)

sim_obs_full <- left_join(sim_obs,sim_obs_scaled)

### compare percent occupancy among models with mixed effects model
# summary of data
summary(sim_model_join_final)

str(sim_model_join_final)

# histogram of detection variable
hist(sim_model_join_final$region_scale)

# calculate proportion of data equal to zero
length(which(sim_model_join_final$region_scale == 0))/nrow(sim_model_join_final)

# assign order to model
sim_model_join_final <- sim_model_join_final %>% 
  mutate(model = factor(model,
                        levels = c("true","base","coa","glatos")))

# create glmm
sim_mod_reg_det <- glmmTMB(region_scale ~ model*regions_short + (1|animal_id),
                           family = "tweedie", data = sim_model_join_final, na.action = "na.fail")

summary(sim_mod_reg_det)

sim_modsel_det <- MuMIn::dredge(sim_mod_reg_det, trace=T, fixed=c('cond(model)'))

best.meta_det <- subset(sim_modsel_det, delta<2)
best.meta_det <- subset(best.meta_det, !is.na(delta))
rowind_det <- which(best.meta_det$df==min(best.meta_det$df))
best.meta_det <- MuMIn::get.models(best.meta_det, subset=rowind_det)[[1]]

summary(best.meta_det)

# NEED TO UPDATE MODEL FOR PROPORTIONAL DATA
hist(sim_model_join_final$region_proportion)

sim_mod_reg_per <- glmmTMB(region_proportion ~ model*regions_short + (1|animal_id),
                           family = "beta_family", data = sim_model_join_final, na.action = "na.fail")

summary(sim_mod_reg_per)

sim_modsel_per <- MuMIn::dredge(sim_mod_reg_per, trace=T, fixed=c('cond(model)'))

best.meta_per <- subset(modsel_det, delta<2)
best.meta_per <- subset(best.meta_per, !is.na(delta))
rowind_per <- which(best.meta_per$df==min(best.meta_per$df))
best.meta_per <- MuMIn::get.models(best.meta_per, subset=rowind_per)[[1]]

summary(best.meta_per)

### Figure 7a: Percent occupancy be region and season
fig7a <- sim_model_join_final %>% 
  filter(regions_short %!in% c("NE Channel", "Gut")) %>% 
  mutate(regions_short = factor(regions_short,
                                levels = c("Main North", "Main Central", "Main South", "Inland Sea", "Malletts", "South Lake"))) %>% 
  ggplot(aes(model,region_percent, fill = model)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge2(width = 1.5, padding = 0.25)) +
  geom_point(shape = 21, alpha = 0.4, position = position_jitterdodge()) +
  facet_rep_wrap(~regions_short, ncol = 3) +
  scale_fill_manual(values = c("#003c30","#80cdc1","#bf812d","543005")) +
  theme_classic()

fig7a

ggsave(filename = paste0(path.figs,"AllModels_PercentDetections_Boxplot&Points_Feb2023.svg"),
       plot = fig7a,
       width = 10,
       height = 5)

### Figure 7b: Scaled occupancy be region and season
fig7b <- model_join_final %>% 
  ggplot(aes(model,region_scale, fill = season)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge2(width = 1.5, padding = 0.25)) +
  geom_point(shape = 21, alpha = 0.4, position = position_jitterdodge()) +
  facet_rep_wrap(~regions_short, ncol = 4) +
  scale_fill_manual(values = c("#003c30","#80cdc1","#bf812d","543005")) +
  theme_classic()

fig7b

ggsave(filename = paste0(path.figs,"AllModels_ScaledDetections_Boxplot&Points_Feb2023.svg"),
       plot = fig7b,
       width = 10,
       height = 5)


### calculate difference in regional percent use among each model and true value for each fish
# percent difference between true and each model
sim_model_summary <- sim_model_join_short %>% 
  group_by(animal_id,regions_short) %>% 
  summarize(true_base = region_percent_true-region_percent_occ,
            true_coa = region_percent_true-region_percent_coa,
            true_glatos = region_percent_true-region_percent_glatos,
            base_glatos = abs(region_percent_glatos-region_percent_occ),
            coa_glatos = abs(region_percent_glatos-region_percent_coa))

# convert to long format
sim_model_summary_long <- sim_model_summary %>% 
  pivot_longer(cols = true_base:true_glatos, 
               names_to = c("models"),
               values_to = "percent_diff") %>% 
  mutate(models = factor(models),
         prop_diff = percent_diff/100)

aggregate(prop_diff~models, data = sim_model_summary_long, FUN = IQR)

### Figure 8: boxplot of differences between percent occupancy for true data and each model 
fig8 <- sim_model_summary_long %>% 
  ggplot(aes(y = prop_diff, x = models, fill = models)) +
  geom_boxplot(outlier.shape = NA) +
  # geom_violin() +
  geom_point(shape = 21, alpha = 0.6, position = position_jitter()) +
  scale_fill_manual(values = c("#8c510a","#80cdc1","#003c30"))+
  theme_classic()

fig8

ggsave(filename = paste0(path.figs,"Model_Accuracy_violin_Feb2023.svg"),
       plot = fig8,
       height = 6,
       width = 7)

ggsave(filename = paste0(path.figs,"Model_Accuracy_boxplot_Feb2023.svg"),
       plot = fig8,
       height = 6,
       width = 7)

###----------------------------------------------------------------------------------------------------


###----------------------------------------------------------------------------------------------------
### Extra code below
###----------------------------------------------------------------------------------------------------
#
# ### Sections from loading receiver data
# # Add Tribs2 data
# recs$Tribs2 <- if_else(recs$deploy_lat < 44.40 & recs$Tributary == "Main Lake",
#                        "Main Lake South",
#                        if_else(recs$deploy_lat > 44.40 & recs$deploy_lat < 44.56 &
#                                  recs$Tributary == "Main Lake", "Main Lake Central",
#                                if_else(recs$deploy_lat > 44.56 & recs$Tributary ==
#                                          "Main Lake","Main Lake North",recs$Tributary)))
# 
# 
# recs <- filter(recs, deploy_date_time <= "2018-01-01")
# 
# # Corrections to recs file
# recs$recover_date_time[recs$StationName %in% "Gut" & recs$recover_date_time == "2017-05-05"] <- "2017-06-23" # change recover time due to later detection and unknown recover date in catos log
# recs$deploy_date_time[recs$StationName %in% "Gordon" & recs$deploy_date_time == "2017-07-26"] <- "2016-11-02" # change deploy date due to prior failed retrieval
# 
# 
# recs_new <- read.csv("ReceiverLocations/dissertation_receiver_log.csv")
# recs_new <- unique(recs_new[,c("station_name", "group")])
# setnames(recs_new, 'station_name', 'StationName')
# 
# recs.1 <- left_join(recs, recs_new)
# recs.1$group <- if_else(recs.1$StationName %in% c("Arnold Central","Arnold West", "Arnold East"), "Arnold",
#                         if_else(recs.1$Tributary %in% c("Carry Bay", "Lamoille River", "Otter Creek", "Winooski River"),
#                                 recs.1$Tributary, recs.1$StationName))
# 
# # summarize receiver locations
# recs_short <- recs.1 %>%
#   group_by(StationName) %>%
#   summarize(x = mean(deploy_long), y = mean(deploy_lat)) %>%
#   ungroup()
# 
# # remove range test receivers
# recs_short <- filter(recs_short, StationName != "BurlingtonBay_RangeTest" &
#                        StationName != "SandBarInlandSea_RangeTest")
# 
# 
# # convert to sp object
# recs_sf <- st_as_sf(x = recs_short, coords = c('x', 'y'), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# 
# recs_sp <- as(recs_sf, "Spatial")
# recs_sp <- SpatialPointsDataFrame(coords = recs_short[,c('x','y')], data = recs_short,
#                                   proj4string = '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
# 
# 
# ### Sections from creating simulated data
# paths <- 1:30
# for (i in 1:30) {
#  paths[i] <- paste0('sim_',i)
# }
#
#
# # Remove redundant files
# sim_full$recv_x.1 <- NULL
# sim_full$recv_y.1 <- NULL
#
#
# # create initial period to build detection timestamp from
# start_time <- as.POSIXct("2022-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# 
# # Create detection_timestamp_utc column from simulated time
# sim_full$detection_timestamp_utc <- start_time + sim_full$time
# 
# # Create column for animal_id
# sim_full$animal_id <- paste("sim", sim_full$virt_fish, sep = "_")
# 
# # Change column names for receiver coordinates
# setnames(sim_full, c('recv_x','recv_y'), c('deploy_lon', 'deploy_lat'))


# ### Original code for visualizing simulated data
# ### Evaluate simulation data 
# 
# 
# # Calculate mean elapsed time difference between first detections on successive receivers by fish
# out <- sim[, .SD[1,"etime"], by = c("recv_id", "virt_fish")]
# out[, t_diff := etime - data.table::shift(etime), by = virt_fish]
# rec_by_dtc_sim1 <- mean(out$t_diff, na.rm = TRUE)
# 
# 
# ### Plots of simulated data
# plot
# fish27 <- filter(true_sf_regions, virt_fish %in% 69)
# fish_track <- ggplot(champ) +
#   geom_sf(fill = "gray90") +
#   geom_path(data = fish27, aes(x = true_lon, y = true_lat), color = "red", inherit.aes = F)+
#   theme_classic()
# 
# fish_track
# 
# ggsave(filename = paste0(path.figs,"fish_track.svg"),
#        plot = fish_track)
# 
# 
# sp::plot(champ, col = "lightgrey", border = "grey")
# points(sim_1,type="o", pch = 20, col = "red")

# # zoom in
# sp::plot(lc_spat, col = "lightgrey", border = "grey", 
#          xlim = sp::bbox(sim_1)[1,], ylim = sp::bbox(sim_1)[2,])
# points(sim_is_1,type="o", pch = 20, col = "red")
# 
# # plot track and detected transmissions
# sp::plot(lc_spat, col = "lightgrey", border = "grey", 
#          xlim = sp::bbox(sim_1)[1,], ylim = sp::bbox(sim_1)[2,])
# points(sim_is_1, type = "o", pch = 20, col = "red")
# plot(st_geometry(recs_sf[,2]), pch = 20, col = "black", add = TRUE)
# points(mydtc, col = "blue", cex = 2, pch=20)

###----------------------------------------------------------------------------------------------------


###----------------------------------------------------------------------------------------------------
### Code for updating VTrack loading function
###----------------------------------------------------------------------------------------------------
# function (Tag.Detections, Tag.Metadata, Station.Information, 
#           source = NULL, tzone = "UTC", crs = NULL) 
# {
#   detection_timestamp <- transmitter_id <- station_name <- receiver_name <- latitude <- longitude <- NULL
#   sensor_value <- sensor_unit <- Date.and.Time..UTC. <- Transmitter <- Station.Name <- Receiver <- Latitude <- NULL
#   Longitude <- Sensor.Value <- Sensor.Unit <- tag_id <- scientific_name <- common_name <- tag_project_name <- NULL
#   release_latitude <- release_longitude <- ReleaseDate <- tag_expected_life_time_days <- tag_status <- sex <- NULL
#   measurement <- installation_name <- project_name <- deploymentdatetime_timestamp <- recoverydatetime_timestamp <- NULL
#   station_latitude <- station_longitude <- status <- NULL
#   if (is.null(source)) 
#     stop("Can't recognize the source of your tag detection data.\n'source' should be either 'IMOS' or 'VEMCO'")
#   if (source %in% "IMOS") {
#     Tag.Detections = as_tibble(Tag.Detections) %>% transmute(Date.Time = lubridate::ymd_hms(detection_timestamp, 
#                                                                                             tz = tzone), Transmitter = transmitter_id, Station.Name = station_name, 
#                                                              Receiver = receiver_name, Latitude = latitude, Longitude = longitude, 
#                                                              Sensor.Value = sensor_value, Sensor.Unit = sensor_unit)
#   }
#   if (source %in% "VEMCO") {
#     Tag.Detections = as_tibble(Tag.Detections) %>% transmute(Date.Time = lubridate::ymd_hms(detection_timestamp_utc, 
#                                                                                             tz = "UTC"), Transmitter = transmitter, Station.Name = StationName, 
#                                                              Receiver = receiver_sn, Latitude = deploy_lat, Longitude = deploy_lon, 
#                                                              Sensor.Value = NA, Sensor.Unit = NA, Receiver.Group = regions_short)
#   }
#   object <- structure(list(Tag.Detections = Tag.Detections, 
#                            Tag.Metadata = as_tibble(Tag.Metadata) %>% transmute(Tag.ID = animal_id, 
#                                                                                 Transmitter = transmitter, Sci.Name = NA, Common.Name = NA, 
#                                                                                 Tag.Project = NA, Release.Latitude = NA, Release.Longitude = NA, 
#                                                                                 Release.Date = lubridate::as_date(cap_date), Tag.Life = NA, 
#                                                                                 Tag.Status = NA, Sex = NA, Bio = NA, Clip = NA), 
#                            Station.Information = as_tibble(Station.Information) %>% 
#                              transmute(Station.Name = StationName, Receiver = receiver_sn, 
#                                        Installation = NA, Receiver.Group = regions, 
#                                        Deployment.Date = lubridate::as_date(deploy_date_time), 
#                                        Recovery.Date = lubridate::as_date(recover_date_time), 
#                                        Station.Latitude = deploy_lat, Station.Longitude = deploy_long, 
#                                        Receiver.Status = NA)), class = "ATT")
#   if (inherits(crs, "CRS")) {
#     attr(object, "CRS") <- crs
#   }
#   else {
#     message("Geographic projection for detection positions not recognised, reverting to WGS84 global coordinate reference system")
#     attr(object, "CRS") <- CRS("+init=epsg:4326")
#   }
#   return(object)
# }
###----------------------------------------------------------------------------------------------------
