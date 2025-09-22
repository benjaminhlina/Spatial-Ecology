# ---- load pakcages ------ 
{
  library(amt)
  library(dplyr)
  library(ggplot2)
  library(ggspatial)
  library(lubridate)
  library(here)
  library(purrr)
  library(readr)
  library(sf)
}

# ----- bring in coa ----- 
coa <- read_rds(
  here("Saved data", 
       "Seasonal_COA_lt_smb_2h.rds")) 

p_map <- st_read(dsn = here("Shapefiles",
                            "."),
                 layer = "plake_edit_wo_link")


plot(p_map)
ggplot()+ 
  geom_sf(data = p_map)
  
p_map_utm_km <- p_map |> 
  st_transform(crs = "+proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs")
# ---p_map# ----- filter out just lake trout ----- 
coa_lt <- coa |> 
  filter(species == "lt")

glimpse(coa_lt)

# ---- det ---
det_split <- coa_lt %>%
  mutate(
    year = year(time_bin),
    month_abb = month(time_bin, label = TRUE, abbr = TRUE),
    id_month = paste(fish_basin, season, sep = "_")
  )


glimpse(det_split)
# ----- convert to utms km ------ 
de_split_utm_km <- det_split |> 
  st_transform(
    crs = "+proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs")

glimpse(de_split_utm_km)
coords <- st_coordinates(de_split_utm_km)
de_split_utm_km <- de_split_utm_km |> 
  mutate(
    x = coords[,"X"],
    y = coords[,"Y"]
  ) |> 
  st_drop_geometry()

# ----- grab names of all fish and when they ahve less than 5 dets ------ 
dt_filter <- de_split_utm_km %>%
  group_by(id_month) %>%
  summarise(
    n_dist = n_distinct(x)
  ) %>%
  ungroup() %>%
  arrange(n_dist) %>%
  filter(n_dist < 5) %>%
  print(n = 317)

# ----- filter out fish that have less than 5 dets ------ 
det_split_update <- de_split_utm_km |> 
  filter(!(id_month %in% dt_filter$id_month))


# ----- create amt object ------ 
df1 <- make_track(det_split_update,
                  .x = x, .y = y, .t = time_bin,
                  # crs = 4326,
                  crs = "+proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs",,
                  all_cols = TRUE
                  
)
df1
glimpse(df1)

# ----- make trast to prj on ------
trast <- make_trast(df1, res = 0.01) 
hs <- hr_kde_ref(df1)         

p_map_utm_km_g <- p_map_utm_km |> 
  select(geometry)

# ----- make kde ---- 
kde <- df1 %>%
  split(.$id_month) %>%
  map(~ hr_kde( .x,
                trast = trast,
                h = hs, levels = c(0.5, 0.8, 0.95)), 
      .progress = TRUE)


# ----- make contours 
kde_href_contours <- kde %>%
  imap( ~ hr_isopleths(.x, levels = c(0.5, 0.8, 0.95))) %>%
  bind_rows(.id = "id_month") %>%
  # st_transform(crs = 32618) %>%
  st_intersection(p_map_utm_km_g) %>%
  tidyr::separate(id_month, sep = "_", into = c("fish_basin",
                                                "season")) %>%
  mutate(
    season = factor(season, levels = c("Fall",
                                       "Winter",
                                       "Spring",
                                       "Summer")),
    level = factor(level), 
    fish_basin = factor(stringr::str_remove(fish_basin, " Basin"), 
                        levels = c("East", "West", "North")
    )
  )

glimpse(kde_href_contours)
# kde_href_contours

# ----- plot ------ 
p <- ggplot() +
  geom_sf(data = p_map_utm_km_g) +
  # geom_sf(data = rec_loc_filter, shape = 4) +
  geom_sf(data = kde_href_contours, aes(fill = level)) +
  facet_grid(fish_basin ~ season) +
  # coord_sf(xlim = c(-77.75, -76.0)) +
  scale_fill_viridis_d(
    end = 0.95,
    begin = 0.25,
    option = "H", direction = -1,
    name = "Level",
    labels = c("50%", "80%", "95%")) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    # legend.position = "inside",
    # legend.position.inside = c(0., 0.6)
    
  )

# p

p + ggview::canvas(height = 9, width = 7)


ggsave(here("Plots",
            "KUD",
            "seqason_basin_kud.png"), 
       height = 9, width = 7
       )

