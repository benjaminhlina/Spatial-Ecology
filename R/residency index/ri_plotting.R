
# load packages ----

library(dplyr)
library(forcats)
library(ggplot2)
library(here)
library(lemon)
library(lubridate)
library(readr)


# bring in clean downloaded ----

ri <- read_rds(here("Saved Data", 
                    "season_ri_lt.rds"))

unique(ri$rec_group)

glimpse(ri)
ri <- ri %>% 
  mutate(
    fish_basin = factor(stringr::str_replace(as.character(fish_basin), " Basin",
                                             ""), 
                        levels = c("East", "West", "North")
    ), 
    rec_group = fct_relevel(rec_group,
                            "North East-Basin", "Central East-Basin", 
                            "Black Bay", "South East-Basin", 
                            "North West-Basin", "Central West-Basin",
                            "Sucker Creek",  "Monaco Bay", "North North-Basin",
                            "Central North-Basin", "Hidden Bay")
  )



ri_sum <- ri %>% 
  group_by(fish_basin, rec_group, season) %>% 
  summarise( 
    mean_ri = mean(ri_w),
    se_ri = sd(ri_w) / sqrt(n())
  ) %>% 
  ungroup()

roi_sum <- ri %>% 
  group_by(fish_basin, rec_group, season) %>% 
  summarise( 
    mean_roi = mean(roi_w),
    se_roi = sd(roi_w) / sqrt(n())
  ) %>% 
  ungroup()



ggplot(data = ri, aes(x = rec_group, y = ri_w)) + 
  geom_boxplot(aes(fill = fish_basin), 
               position = position_dodge(0.9, preserve = "single")) +
  facet_wrap(. ~ season, nrow = 2) + 
  scale_fill_viridis_d(name = "Basin",
                       option = "G", begin = 0.25, end = 0.7) +
  theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank(), 
    axis.text = element_text(colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) + 
  labs(x = "Location", 
       y = "Residency Index")

ggplot() + 
  geom_boxplot(data = ri, aes(x = rec_group, y = ri_w, 
                             fill = fish_basin), 
               outlier.colour = NA, 
              position = position_dodge(0.9, 
                                        # preserve = "single"
                                        )
              ) +
  geom_point(data = ri_sum, aes(x = rec_group, y = mean_ri, 
                              group = fish_basin),
           size = 2,
           position = position_dodge(0.9)
) +
  geom_errorbar(data = ri_sum, aes(x = rec_group, y = mean_ri,
                                   ymin = mean_ri - se_ri,
                                   ymax = mean_ri + se_ri,
                                   group = fish_basin),
                width = 0.1, 
                position = position_dodge(0.9)) +
  facet_grid(fish_basin ~ season,
             # switch = "x", space = "free_x"
             ) + 
  # scale_colour_viridis_d(name = "Basin",
  #                        option = "G", begin = 0.25, end = 0.7) +
  scale_fill_viridis_d(name = "Basin",
                         option = "G", begin = 0.25, end = 0.7, 
                       alpha = 0.55) +
  theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank(), 
    axis.text = element_text(colour = "black"),
    axis.text.x = element_text(angle = 90, size = 12, hjust = 0), 
    strip.text.y = element_blank(), 
    legend.position = c(0.95, 0.9), 
    legend.background = element_blank()
  ) + 
  labs(x = "Location", 
       y = "Residency Index") -> p 


ggsave(here::here(
  "Plots",
  "residency index", 
  "season_basin_loc_boxplot.png"), 
  plot = p, 
  width = 11, height = 8.5)


ggplot() + 
  geom_boxplot(data = ri, aes(x = rec_group, y = roi_w, 
                              fill = fish_basin), 
               outlier.colour = NA, 
               position = position_dodge(0.9, 
                                         # preserve = "single"
               )
  ) +
  geom_point(data = roi_sum, aes(x = rec_group, y = mean_roi, 
                                group = fish_basin),
             size = 2,
             position = position_dodge(0.9)
  ) +
  geom_errorbar(data = roi_sum, aes(x = rec_group, y = mean_roi,
                                   ymin = mean_roi - se_roi,
                                   ymax = mean_roi + se_roi,
                                   group = fish_basin),
                width = 0.1, 
                position = position_dodge(0.9)) +
  facet_grid(fish_basin ~ season,
             # switch = "x", space = "free_x"
  ) + 
  # scale_colour_viridis_d(name = "Basin",
  #                        option = "G", begin = 0.25, end = 0.7) +
  scale_fill_viridis_d(name = "Basin",
                       option = "G", begin = 0.25, end = 0.7, 
                       alpha = 0.55) +
  theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank(), 
    axis.text = element_text(colour = "black"),
    axis.text.x = element_text(angle = 90, size = 12, hjust = 0), 
    strip.text.y = element_blank(), 
    legend.position = c(0.95, 0.9), 
    legend.background = element_blank()
  ) + 
  labs(x = "Location", 
       y = "Roaming Index") -> p1 

ggsave(here::here(
  "Plots",
  "roaming index", 
  "roi_season_basin_loc_boxplot.png"), 
  plot = p1, 
  width = 11, height = 8.5)
