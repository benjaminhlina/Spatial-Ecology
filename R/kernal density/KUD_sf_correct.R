# ----load packages ----
{
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

# ---- create datafreame remove sf objectve -----
df <- df_utm %>% 
  mutate(
    lon = st_coordinates(.)[,"X"],
    lat = st_coordinates(.)[,"Y"]
  ) %>%
  st_drop_geometry()

df_lt <- df %>% 
  filter(species == "lt")
df_t <- df %>% 
  filter(floy_tag == "05550" & season == "Summer")
# create loop to do KUD for each fish for each season -----

cl_list <- list()
cl_list_final <- list()
fish_id <- unique(df_lt$floy_tag)

for (i in 1:length(fish_id)) {
  dat <- df_lt %>% 
    filter(floy_tag %in% fish_id[i])
  
  id <- unique(dat$floy_tag)
  seasons <- unique(dat$season)
  
  for (se in 1:length(seasons)) {
    dat_1 <- dat %>% 
      filter(season %in% seasons[se])
    ht <-  kde2d(x = dat_1$lon, y = dat_1$lat, h = 750)
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
  
  cl_list_df <- do.call(rbind, cl_list)
  cl_list_final[[i]] <-  cl_list_df
}


# ---- combine all sf objects into 1 sf object -----
cl_sf <- do.call(rbind, cl_list_final) %>% 
  mutate(
    n_density = (density / max(density, na.rm = TRUE))
  ) 

cl_sf
cl_poly <- smooth(cl_sf, method = "ksmooth") %>% 
  st_cast("POLYGON")  %>% 
  mutate(
    area = st_area(.)
  )

cl_poly_area <- cl_poly %>%
  st_drop_geometry() %>%
  group_by(
    floy_tag, season, n_density
  ) %>% 
  
  summarise(
    total_area = sum(area)
  ) %>% 
  ungroup()

cl_poly_area <- cl_poly_area %>% 
  mutate(
    total_area_m = as.numeric(total_area),
    tot_area_km = set_units(total_area, km^2), 
    total_area_km = as.numeric(tot_area_km)
  )

cl_90 <- cl_poly_area %>% 
  filter(n_density < 0.10) %>% 
  mutate(
    l_90 = 0.1
  ) %>% 
  group_by(floy_tag, season, l_90) %>% 
  summarise(
    area_km = sum(total_area_km)
  ) %>% 
  ungroup()



ggplot(data = cl_90, aes(x = area_km)) + 
  geom_histogram()

df_lt_id <- df_lt %>% 
  dplyr::select(floy_tag, fish_basin) %>% 
  group_by(floy_tag, fish_basin) %>% 
  summarise() %>% 
  ungroup()

cl_poly_area <- cl_poly_area %>% 
  left_join(df_lt_id, by = "floy_tag")
cl_90 <- cl_90 %>% 
  left_join(df_lt_id, by = "floy_tag")


fitdistrplus::descdist(cl_90$area_km)

library(glmmTMB)

m <- glmmTMB(area_km ~ season * fish_basin + (1|floy_tag), 
             data = cl_90, family = Gamma(link = "log"))


library(DHARMa)
res <- simulateResiduals(m)
plot(res)

library(broom.mixed)

car::Anova(m)
car::Anova(m1)


# model choosen is M1 ------

main_effects <- tidy(car::Anova(m, 
                                test.statistic = c("Chisq")))

ind_effects <- tidy(m)

glance(m)
glance(m1)

model_fit <- glance(m)
main_effects
ind_effects
model_fit

library(emmeans)
multi_comp <- emmeans(m,  ~  fish_basin * season, 
                      adjust = "tukey", type = "response")
contrast(multi_comp)
contrast_effects <- contrast(multi_comp, method = "pairwise") %>% 
  as_tibble() %>% 
  arrange(p.value)
contrast_effects

# 
# cl_poly <- p_lake_utm %>% 
#   st_intersection(l_smooth_ksmooth) %>%
#   mutate(
#     area = st_area(.)
#   ) 


ggplot() + 
  geom_sf(data = p_lake_utm, fill = NA) +
  geom_sf(data = cl_poly, aes(
    # colour = n_density
    fill = n_density,
  ), 
  colour = NA
  ) + 
  scale_fill_viridis_c(name = "Density") +
  # scale_colour_viridis_c(name = "Density") +
  facet_grid(season ~ floy_tag) +
  # facet_wrap(. ~ season) + 
  theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank()
  ) + 
  labs(x = "Longitude", 
       y = "Latitude")
