# ----load packages ----
{
  library(adehabitatHR)
  library(concaveman)
  library(dplyr)
  library(ggplot2)
  library(here)
  library(MASS)
  library(purrr)
  library(readr)
  library(sf)
  library(smoothr)
  library(units)
}


# ---- Bring in shapefile ------
p_lake <- st_read(dsn = here::here("Shapefiles",
                                   "."),
                  layer = "plake_edit_wo_link") %>% 
  
  dplyr::select(AREA_GEO, geometry)
p_lake_utm <- p_lake %>% 
  st_transform(., crs = 32618) 

# ---- bring in COAs ----

df_sf <- read_rds(here("Saved Data",
                       "Seasonal_COA_lt_smb_2h.rds"))

df_utm <- df_sf %>% 
  st_transform(., crs = 32618) 
df_lt <- df_utm %>% 
  filter(species == "lt")


# ---- create datafreame remove sf objectve -----

# create loop to do KUD for each fish for each season -----

cl_list <- list()
cl_list_final <- list()
pt_list <- list()
fish_id <- unique(df_lt$floy_tag)
# i <- 1
# se <- 1

for (i in 1:length(fish_id)) {
  dat <- df_lt %>% 
    filter(floy_tag %in% fish_id[i])
  
  id <- unique(dat$floy_tag)
  seasons <- unique(dat$season)
  
  for (se in 1:length(seasons)) {
    dat_1 <- dat %>% 
      filter(season %in% seasons[se])
    dat_2 <- dat_1 %>% 
      dplyr::select(floy_tag, geometry) %>% 
      rename(id = floy_tag)
    
    dat_sp <- dat_2 %>% 
      as_Spatial()
    
    ht <-  kernelUD(dat_sp, h = 750, grid = 1000, extent = 10)
    # image(ht)
    
    
    perc <- seq(10, 90, 10)
    for (p in 1:length(perc)){
      percentage <- perc[p]
      pt <- getverticeshr(ht, percent = perc[p],
                          unin = 'm', unout='m2')
      pt_sf <- st_as_sf(pt, coords = c("x","y")) %>% 
        mutate(
          percentage = percentage
        )
      
      pt_list[[p]] <- pt_sf
    }
    
    pt_result_sf <- do.call(rbind, pt_list)
    
    pt_result_sf <- pt_result_sf %>% 
      mutate(
        seasons = unique(dat_1$season)
      )
    cl_list[[se]] <- pt_result_sf
  }
  cl_list_final[[i]] <- cl_list
  message('Processing KDE for fish ', i, " of ", length(fish_id), " for ",
          unique(dat_1$season), ' of ', length(se))
}

pt_result_sf <- do.call(bind_rows, cl_list_final)

pt_result_sf_s <- st_intersection(p_lake_utm, pt_result_sf)

ggplot() + 
  geom_sf(data = p_lake_utm, fill = NA) +
  geom_sf(data = pt_result_sf_s, aes(fill = percentage)) + 
  facet_grid(seasons ~ id) + 
  scale_fill_viridis_c(, alpha = 0.25)
vkde_point_sf

cl <- ht$`2`@coords)
ht$`2`@data
nrow(.)

cl <- contourLines(ht)

cl_lines <- do.call(rbind,
                    Map(function(x){
                      st_as_sf(
                        data.frame(
                          density = x$level,
                          geometry = st_sfc(st_linestring(cbind(x$x, x$y)))
                        ), 
                        crs = st_crs(p_lake_utm)
                      )}, cl
                    )
)
cl_lines <- cl_lines %>% 
  mutate(
    floy_tag = unique(dat_1$floy_tag), 
    season = unique(dat_1$season),
  )


message('Processing KDE for fish ', i, " of ", length(fish_id), " for ",
        unique(dat_1$season), ' of ', length(se))
cl_list[[se]] <- cl_lines 
}
