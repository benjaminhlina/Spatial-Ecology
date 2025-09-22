# ---- load pakcages ------ 
{
  library(amt)
  library(dplyr)
  library(lubridate)
  library(here)
  library(purrr)
  library(readr)
  library(openxlsx)
  library(sf)
  library(tidyr)
}

# ----- bring in coa ----- 
coa <- read_rds(
  here("Saved data", 
       "Seasonal_COA_lt_smb_2h.rds")) 

p_map <- st_read(dsn = here("Shapefiles",
                            "."),
                 layer = "plake_edit_wo_link")

plot(p_map)
p_map_basin <- st_read(dsn = here("Shapefiles",
                                  "."),
                       layer = "plake_edit_wo_link_seper") |> 
  st_transform(crs = "+proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs")

plot(p_map)

plot(p_map_basin)

p_map_utm_km <- p_map |> 
  st_transform(crs = "+proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs")
# ----- bring in cld ----- 
cld <- read_csv(here::here("Results",
                           "kud-area",
                           "cld_glmm_basin_season.csv"))

glimpse(cld)

# ----- filter out just lake trout ----- 
coa_lt <- coa |> 
  filter(species == "lt")

basins <-  coa_lt  %>%
  st_drop_geometry() |> 
  distinct(floy_tag, fish_basin)


kde_href_contours <- qs::qread(file =  here("Saved Data",
                                            "kud",
                                            "monthly_for_each_id_kud_120_min_coa.qs"))
p_map_utm_km_g |> 
  st_area() |> 
  as.numeric()



# ------ have it seperate by basin ----- 

kde_href_contours_basin <- kde_href_contours %>%
  st_intersection(p_map_basin)


# ----- fix the percentage before summary ----- 
kde_href_contours_basin <- kde_href_contours_basin %>%
  mutate(
    area = st_area(geometry), 
    area_num = round(as.numeric(area), 2),
    percent_use = round( ((area_num / 27.65571) * 100), 2)
  ) %>%
  st_drop_geometry() %>%
  arrange(
    level, season
  )


glimpse(kde_href_contours)




kde_summary <- kde_href_contours_basin %>%
  # left_join(basins, by = c("id" = "floy_tag")) |> 
  group_by(
    fish_basin, basin, season, level
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
    summary_perc = paste(mean_perc, sem_perc, sep = " ± "), 
    fish_basin = factor(stringr::str_remove(fish_basin, " Basin"), 
                        levels = c("East", "West", "North")),  
    season = factor(season, levels = c("Fall",
                                       "Winter",
                                       "Spring", 
                                       "Summer"))
  ) |> 
  arrange(
    fish_basin, season, level
  )

kde_summary




# ----- fix cld and try to add it on ------ 

cld_select <- cld |> 
  select(level, fish_basin, basin, season, .group) |> 
  mutate(
    level = factor(level), 
    fish_basin = factor(fish_basin, levels = sort(unique(kde_summary$fish_basin))),
    basin = factor(stringr::str_to_sentence(basin), levels = c("East", 
                                                               "West", 
                                                               "North")),
    season = factor(season, levels = sort(unique(kde_summary$season)))
  )


cld_select

kde_summary <- kde_summary |> 
  left_join(cld_select)


kde_summary


kde_summary







superscript_map <- c(
  a="ᵃ", b="ᵇ", c="ᶜ", d="ᵈ", e="ᵉ", f="ᶠ", g="ᵍ", h="ʰ",
  i="ⁱ", j="ʲ", k="ᵏ", l="ˡ", m="ᵐ", n="ⁿ", o="ᵒ", p="ᵖ",
  r="ʳ", s="ˢ", t="ᵗ", u="ᵘ", v="ᵛ", w="ʷ", x="ˣ", y="ʸ", z="ᶻ"
)


add_superscript <- function(text, group) {
  sup <- paste0(superscript_map[strsplit(group, "")[[1]]], collapse = "")
  paste0(text, sup)
}




kde_summary <- kde_summary %>%
  rowwise() %>%
  mutate(
    summary_area = add_superscript(summary_area, .group),
    summary_perc = add_superscript(summary_perc, .group)
  ) %>%
  ungroup()



kde_wide <- kde_summary %>%
  mutate(
    basin = factor(basin, levels = c("East", 
                                     "West", 
                                     "North"))
  ) |> 
  dplyr::select(fish_basin, basin, season, level, summary_area, summary_perc) %>%
  pivot_wider(id_cols = c(fish_basin, season, basin),
              names_from = level,
              values_from = c("summary_area",
                              "summary_perc")) |> 
  mutate(
    season = factor(season, levels = c("Fall",
                                          "Winter",
                                          "Spring",
                                          "Summer"))
  ) |> 
  unnest() |> 
  arrange(fish_basin, season, basin)


kde_wide













openxlsx::write.xlsx(kde_summary,
                     here::here("Results",
                                "kud-area",
                                "season_perc_used_summary_n.xlsx")
)
openxlsx::write.xlsx(kde_wide,
                     here::here("Results",
                                "kud-area",
                                "season_perc_used_summary_n_wide.xlsx"))
