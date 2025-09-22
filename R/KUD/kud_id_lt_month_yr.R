# ---- load pakcages ------ 
{
  library(amt)
  library(dplyr)
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
    id_month = paste(floy_tag, season, year, sep = "_")
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

glimpse(kde$`05550_Spring_2019`)
# ----- make contours 
kde_href_contours <- kde %>%
  imap( ~ hr_isopleths(.x, levels = c(0.5, 0.8, 0.95))) %>%
  bind_rows(.id = "id_month") %>%
  # st_transform(crs = 32618) %>%
  st_intersection(p_map_utm_km_g) %>%
  tidyr::separate(id_month, sep = "_", into = c("id",
                                                "season", "year")) %>%
  mutate(
    # month_abb = factor(month_abb, levels = month),
    level = factor(level)
  )
bas <- det_split |> 
  st_drop_geometry() |> 
  distinct(floy_tag, fish_basin)


bas
# glimpse(kde_href_contours)
kde_href_contours <- kde_href_contours |>
  left_join(bas,
    by = c("id" = "floy_tag")
  )
kde_href_contours
glimpse()
# ------ save output for glmm analysis ------ 
qs::qsave(x = kde_href_contours, file =  here("Saved Data",
                                              "kud",
                                              "monthly_for_each_id_kud_120_min_coa.qs"))

p_map_utm_km_g |> 
  st_area() |> 
  as.numeric()

kde_href_contours <- kde_href_contours %>%
  mutate(
    area_num = round(as.numeric(area), 2),
    percent_use = round( ((area_num / 27.65571) * 100), 2)
  ) %>%
  st_drop_geometry() %>%
  arrange(
    level, season
  )


kde_href_contours
# basins <-  coa_lt  %>%
#   distinct(floy_tag, fish_basin)

kde_summary <- kde_href_contours %>%
  group_by(
    fish_basin, season, level
  ) %>%
  summarise(
    mean_area = mean(area_num),
    sem_area = sd(area_num) / sqrt((n())),
    mean_perc = mean(percent_use),
    sem_perc = sd(percent_use) / sqrt((n())),
  ) %>%
  ungroup() %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(
    summary_area = paste(mean_area, sem_area, sep = " ± "),
    summary_perc = paste(mean_perc, sem_perc, sep = " ± ")
  )

kde_summary

kde_wide <- kde_summary %>%
  dplyr::select(season, level, summary_area, summary_perc) %>%
  pivot_wider(id_cols = season,
              names_from = level,
              values_from = c("summary_area",
                              "summary_perc")) |> 
  unnest()


kde_wide

openxlsx::write.xlsx(kde_summary,
                     here::here("results",
                                "kud-area",
                                "season_perc_used_summary_n.xlsx")
)
openxlsx::write.xlsx(kde_wide,
                     here::here("results",
                                "kud-area",
                                "season_perc_used_summary_n_wide.xlsx"))
