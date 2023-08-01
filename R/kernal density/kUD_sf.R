# ----load packages ----
{
library(dplyr)
library(ggplot2)
library(here)
library(ipdw)
library(MASS)
library(readr)
library(spatstat)
library(stars)
library(sf)
}


# ---- Bring in shapefile ------
p_lake <- st_read(dsn = here::here("Shapefiles",
                                   "."),
                  layer = "plake_edit_wo_link")
p_lake_utm <- p_lake %>% 
  st_transform(., crs = 32618) 

# ---- bring in COAs ----

df <- read_rds(here("Saved Data",
                    "Seasonal_COA_lt_smb_2h.rds"))

df_utm <- df %>% 
  st_transform(., crs = 32618) 


df_f <- df_utm %>% 
  filter(floy_tag %in% "05550" & season == "Summer")

# costras <- costrasterGen(df_f, p_lake_utm, extent = "pnts",
#                          projstr = projection(p_lake_utm), resolution = 5)
# 
# beepr::beep()
# 
# 
# costras
# costras[160:170, 1:80] <- 10000
# 
# gc()
# W <- owin(range(c(st_bbox(df_f)["xmin"], st_bbox(df_f)["xmax"])),
#           range(c(st_bbox(df_f)["ymin"], st_bbox(df_f)["ymax"])))
# kat.pp <- ppp(st_coordinates(df_f)[,1], 
#               st_coordinates(df_f)[,2], window = W)
# 
# plot(kat.pp)
# 
# mean.neighdist <- mean(nndist(kat.pp))
# 
# 
# # grid building
# gridsize <- mean.neighdist * 2
# grainscale.fac <- gridsize / res(costras)[1]
# gridras <- aggregate(costras, fact = grainscale.fac)
# gridpol <- rasterToPolygons(gridras)
# gridpol$value <- row.names(gridpol)
# # plot(gridpol)
# 
# fulldataset.over <- sf::st_join(df_f, st_as_sf(gridpol))
# glimpse(fulldataset.over)
# 
# set.seed(2)
# gridlev <- unique(fulldataset.over$value)
# for (i in seq_along(gridlev)) {
#   activesub <- subset(fulldataset.over, fulldataset.over$value == gridlev[i])
#   selectnum <- gdata::resample(seq_len(nrow(activesub)), 1)
#   if (i == 1) {
#     training <- activesub[selectnum, ]
#   } else {
#     training <- rbind(training, activesub[selectnum, ])
#   }
#   message('Processing training ', i, ' of ', length(gridlev))
# }
# beepr::beep()
# 
# validate <- fulldataset.over[!(row.names(fulldataset.over) %in%
#                                  row.names(training)), ]
# plot(costras)
# plot(st_geometry(training), add = TRUE)
# plot(st_geometry(validate), col = "red", add = TRUE)
# 
# final.ipdw <- ipdw(training, costras, range = mean.neighdist * 10,
#                    overlapped = TRUE)
# 
# plot(final.ipdw, main = "Kattegat salinity (ppt)")
# 
# 
# library(raster)
# library(ade4)
# library(adehabitatHR)
# 
# df_f_sp <- df_f %>% 
#   as_Spatial()
# plot(df_f_sp)
# 

# sp_lake_utm <- p_lake_utm %>% 
#   as_Spatial() %>% 
#   as('SpatialLinesDataFrame')
# sp_lake_utm
# 
# kde.output <- kernelUD(df_f_sp, h="href", grid = 1000, )
# 
# 
# glimpse(kde.output)
# 
# plot(kde.output)
# kde <- raster(kde.output)
# projection(kde) <- CRS("+init=EPSG:32618")
# head(kde.output@data)
# nrow(kde.output@data)
# nrow(kde.output@coords)
# kde.output@coords
# kde_sf <- bind_cols(kde.output@coords, 
#                     kde.output@data) %>% 
#   st_as_sf(coords = c("Var2", "Var1"), 
#            crs = st_crs(p_lake_utm)) 
# 
# summary(kde.output@data)
# kde_sf %>% 
#   filter(ud > 1e-08) %>%
#   # mutate(
#   #   ud = round(ud, 11)
#   # ) %>% 
#   # group_by(ud) %>% 
#   # summarise(do_union = FALSE, 
#   #           n = n()) %>% 
#   # st_cast("MULTIPOINT") %>%
#   # 
#   ungroup() -> test
# 
# 
# test %>% 
#   # filter(n > 4) %>% 
#   # group_by(ud) %>% 
#   # summarise(do_union = FALSE) %>% 
#   # st_cast("POLYGON") %>% 
#   # st_cast("MULTIPOLYGON") %>% 
#   ggplot() + 
#   # geom_sf(data = p_lake_utm, fill = NA) +
#   geom_sf(aes(colour = ud)) +
#   scale_colour_viridis_c(option = "B", end = 0.85) + 
#   # scale_fill_viridis_c(option = "B", end = 0.85) + 
#   theme_bw(base_size = 15) + 
#   theme(panel.grid = element_blank())
# 
# st_dimension(test)
# 
# library(tmap)
# 
# # maps the raster in tmap, "ud" is the density variable
# tm_shape(kde) + tm_raster("ud")
# bounding_box <- kde.output@bbox
# 
# # maps the raster within the bounding box
# tm_shape(kde, bbox = bounding_box) + tm_raster("ud")
# 

df_f


df_f_m <- df_f %>% 
  mutate(
    lon = st_coordinates(.)[,"X"],
    lat = st_coordinates(.)[,"Y"]
  ) %>%
  st_drop_geometry()


glimpse(df)

ht <- kde2d(x = df_f_m$lon, y = df_f_m$lat, h = 750) 



# 
# p <- ggplot() + 
#   geom_density_2d_filled(data = df_f_m, aes(x = lon, y = lat), 
#                          contour_var = "ndensity")
# 
# dat <- layer_data(p, i = 1)
# 
# 
# 
# glimpse(dat)
# 
# # Id of polygon
# dat$pol <- paste0(dat$group, "_", dat$subgroup)
# ids <- unique(dat$pol)
# 
# 
# # Split and create polygons based on the id
# pols <- lapply(ids, function(x){
#   topol <- dat[dat$pol == x, ]
#   
#   closepol <- rbind(topol, topol[1, ])
#   
#   pol <- st_polygon(list(as.matrix(closepol[,c("x", "y")])))
#   
#   # Add features
#   df <- unique(topol[, grepl("level", names(topol))])
#   
#   tofeatures <- st_as_sf(df, geometry=st_sfc(pol), 
#                          crs = st_crs(p_lake_utm))
#   
#   return(tofeatures)  
# })
# 
# 
# 
# final_pols <- do.call(rbind, pols)
# 
# final_pol <- final_pols %>% 
#   filter(level_low > 0.1)
# 
# ggplot(final_pol) +
#   geom_sf(data = p_lake_utm, fill = NA) + 
#   geom_sf(aes(fill=level), colour = NA) +
#   scale_fill_viridis_d() + 
#   coord_sf()
# 
# 
# 
# 
# library(tidyr)

glimpse(df_f_m)


# expand_grid(x = ht$x, y = ht$y)
# ht_den <- expand_grid(lon = ht$x, lat = ht$y) %>% 
#   bind_cols(., 
#             
#             
#             as_tibble(ht$z) %>% 
#               mutate(
#                 id  = 1:nrow(.)
#               ) %>% 
#               pivot_longer(
#                 cols = -id, 
#                 names_to = "column_name",
#                 values_to = "density"
#               ) %>% 
#               mutate(
#                 ndensity = (density / max(density, na.rm = TRUE))
#               )
#   )
# 
# 
# 
# ggplot(data = ht_den, aes(x = lon, y = lat)) + 
#   geom_point(aes(colour = ndensity)) + 
#   scale_colour_viridis_c() +
#   coord_sf() 

image(ht)

cl <- contourLines(ht)
lapply(cl, lines)


cllines <- do.call(rbind,
                   Map(function(x){
                     st_as_sf(
                       data.frame(
                         density = x$level,
                         geometry = st_sfc(
                           st_linestring(cbind(x$x, x$y))
                         )
                       ), 
                       crs = st_crs(p_lake_utm)
                     )}, cl
                   ))


cllines

boundary_poly <- p_lake_utm %>% 
  dplyr::select(AREA_GEO, geometry)
tes <- cllines %>% 
  st_cast("POLYGON") %>% 
  mutate(
   ndesnity = (density / max(density, na.rm = TRUE)),
  ) %>% 
  




st_intersects(tes, boundary_poly)



ggplot() + 
  geom_sf(data = p_lake_utm, fill = NA) + 
  geom_sf(data = sf_contain, aes(fill = ndesnity)) + 
  scale_fill_viridis_c()


st_intersection(boundary_poly, tes) %>% 
  ggplot() + 
  geom_sf(data = p_lake_utm, fill = NA) + 
  geom_sf(aes(fill = ndesnity), colour = NA) +
  # scale_colour_viridis_c()
  scale_fill_viridis_c() 

idx <- st_intersects(
  x = boundary_poly
  , y = tes
)

idx <- unlist( idx)

sf_crosses <- tes[ idx, ]



ggplot() + 
  geom_sf(data = p_lake_utm, fill = NA) + 
  geom_sf(data = sf_crosses, aes(fill = ndesnity), colour = NA) +
  # scale_colour_viridis_c()
  scale_fill_viridis_c()


ggplot() + 
  geom_sf(data = p_lake_utm, fill = NA) + 
  geom_sf(data = tes %>% 
            filter(ndesnity < 0.3), aes(fill = ndesnity), colour = NA) +
  # scale_colour_viridis_c()
  scale_fill_viridis_c()

dens <- ht
nx <- nrow(df_f_m) # number of observations in this group
df <- expand.grid(x = dens$x, y = dens$y)
df$density <- as.vector(dens$z)
df$group <- data$group[1]
df$ndensity <- df$density / max(df$density, na.rm = TRUE)
df$count <- nx * df$density
df$n <- nx
df$level <- 1
df$piece <- 1


df <- expand.grid(x = dens$x, y = dens$y) %>% 
  as_tibble()

df_sf <- df_sf %>% 
  dplyr::select(-density)

df_sf <- df %>% 
  st_as_sf(coords = c("x", "y"), crs = st_crs(p_lake_utm))

df_sf

ggplot() + 
  geom_sf(data = df_sf, aes(fill = ndensity), shape = 21, 
          stroke = 1, size = 2) + 
  scale_fill_viridis_c()
ggplot() + 
  geom_contour_filled(data = df, aes(x = x, y = y, z = ndensity)) + 
  scale_colour_viridis_c() + 
  coord_sf()
plot(df_sf)

df_stars<- stars::st_as_stars(df_sf)


plot(df_stars)
st_contour(x = df_stars, na.rm = TRUE)
