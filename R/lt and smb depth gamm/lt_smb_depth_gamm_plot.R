# --- bring in packages ----
{
  library(broom.mixed)
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(gratia)
  library(here)
  library(itsadug)
  library(mgcv)
  library(readr)
}

dat <- read_rds(here("Saved Data", 
                     "daily_mean_sensor_sp.rds"))

m1 <- read_rds(here("model objects", 
                    "lt_smb_gamm_depth.rds"))


dat <- dat %>% 
  filter(sensor_unit == "m" & mean_value > 0) %>% 
  mutate(
    floy_tag = "a", 
    year = "b"
  )

fit <- predict.gam(m1, newdata = dat, type = "link", discrete = TRUE, 
                   exclude = "s(floy_tag, year)", se.fit = TRUE)


preds <- tibble(dat, 
                .fit = fit$fit, 
                .se.fit = fit$se.fit) %>% 
  mutate(
    .fit = exp(1) ^ .fit, 
    .se.fit = exp(1) ^ .se.fit, 
    lower = .fit - 1.96 * .se.fit,
    upper = .fit + 1.96 * .se.fit, 
  )

dat %>%
  group_by(doy, species) %>%
  summarise(mean_depth = mean(mean_value)) %>%
  ungroup() -> mean_depth

ggplot(data = preds) +
  # geom_rect(data = rect_summer, aes(xmin = xmin,
  #                                   xmax = xmax,
  #                                   ymin = ymin,
  #                                   ymax = ymax),
  #           fill = "grey80",
  #           alpha = 0.75,
  #           inherit.aes = FALSE) +
  # geom_rect(data = rect_winter, aes(xmin = xmin,
  #                                   xmax = xmax,
  #                                   ymin = ymin,
  #                                   ymax = ymax),
  #           fill ="grey80",
  #           alpha = 0.75,
  #           inherit.aes = FALSE) +
  # geom_text(
  #   aes(x = xmin + 30, y = 125, label = season),
  #   data = rect_summer,
  #   size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  # geom_text(
  #   aes(x = xmin + 32, y = 125, label = season),
  #   data = rect_winter,
  #   size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  # geom_point(data = mean_rmr, aes(x = doy_id, y = mean_rmr,
  #                                  colour = fish_basin,
  # ), alpha = 0.25, size = 2.75) +
  geom_point(data = mean_depth, aes(x = doy, y = mean_depth,
                                  colour = species), 
  alpha = 0.5, size = 3) +
  
  geom_line(
    aes(x = doy, y = .fit, colour = species), linewidth = 1) +
  geom_ribbon(
    aes(ymin = lower,
        ymax = upper,
        x = doy, y = .fit,
        fill = species), alpha = 0.25) +
  scale_y_reverse() + 
  lemon::facet_rep_wrap(ncol = 1, . ~ fish_basin, repeat.tick.labels = TRUE) + 
  # scale_y_continuous(breaks = seq(45, 135, 15)) +
  # scale_x_continuous(breaks = seq(09, 344, 67),
  #                    label = month_label) +
  scale_colour_viridis_d(name = "Species",
                         option = "G", begin = 0.35, end = 0.75) +
  scale_fill_viridis_d(name = "Species",
                       option = "G", begin = 0.35, end = 0.75) +
  # scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
  
  # facet_rep_wrap(.~ floy_tag, repeat.tick.labels = TRUE,
  #                # ncol = 1
  # ) +
  theme_bw(base_size = 15) +
  theme(panel.grid = element_blank(),
        # strip.text = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.95, 0.92),
        legend.background = element_blank(),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Date",
       y = "Depth Use (m)") -> p 

p





