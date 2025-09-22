
# day_vec <- seq(as.Date("2020-05-01"), as.Date("2021-04-30"), by = "days")
# days(day_vec)



# load packages ----

library(broom.mixed)
library(dplyr)
library(fitdistrplus)
library(ggplot2)
# library(ggh4x)
# library(gamm4)
library(gratia)
library(here)
library(itsadug)
library(lubridate)
library(lemon)
library(janitor)
# library(lme4)
library(mgcv)
# library(multcomp)
library(openxlsx)
library(patchwork)
library(purrr)
library(readr)
library(tibble)
# library(tidymv)
library(tidyr)
# library(visreg)
source(here::here("R", 
                  "functions",
                  "julian_date_reorder.r"))

# bring in RDS -----

# ful <- read_rds(here("Saved Data",
#                      "lkt_depth.rds"))
# 
# glimpse(ful)
# ##### SUBSET OUT ONLY TEMP DATA and determine daily temp FOR 2017 - 2021-------
# 
# unique(ful$sensor_unit)
# 
# ful_depth <- ful %>%
#   group_by(floy_tag, date, fish_basin,
#            week, month, season, year,
#            sensor_unit) %>%
#   summarise(mean_depth = mean(sensor_value)) %>%
#   ungroup() %>%
#   mutate(date_2 = as.numeric(date),
#          floy_tag = factor(floy_tag)) %>%
#   filter(date_2 <= 18559) %>%
#   mutate(fish_basin = factor(stringr::str_replace(as.character(fish_basin), " Basin",
#                                                   ""),
#                              levels = c("East", "West", "North")),
#          doy = yday(date),
#          month = factor(month,
#                         levels = c("May", "June", "July",
#                                    "August", "September", "October",
#                                    "Novemeber", "December", "January",
#                                    "February", "March", "April")
#          )
#   ) %>%
#   arrange(month) %>%
#   mutate(doy_id = days(date)) %>%
#   arrange(date)
# 
# glimpse(ful_depth)
# 
# 
# # remove big objects to free up RAM -----
# gc()
# 
# 
# 
# write_rds(ful_depth, here("Saved Data",
#                         "daily_depth.rds"))

ful_depth <- read_rds(here("Saved Data",
                           "daily_depth.rds"))


tl_d <- ful_depth %>% 
  group_by(fish_basin, floy_tag, year) %>% 
  summarise(doy_min = min(doy_id)) %>% 
  ungroup()

tl_d
# test <- start_event(ful_depth, column = c("floy_tag", "doy_id"))




ful_depth <-  ful_depth %>% 
  group_by(floy_tag, year) %>% 
  arrange(floy_tag, year, doy_id) %>% 
  mutate(start_event = if_else(doy_id == min(doy_id), true = TRUE, 
                               false = FALSE)) %>% 
  ungroup() %>% 
  arrange(date, start_event) %>% 
  dplyr::select(-month, -week, -date_2)



ggplot(data = ful_depth, aes(x = mean_depth)) + 
  geom_histogram()


descdist(ful_depth$mean_depth)

gamma_dist <- fitdist(ful_depth$mean_depth, distr = "gamma", method = "mme")
plot(gamma_dist)



n_distinct(ful_depth$year)
n_distinct(ful_depth$floy_tag)

# crete day of year (doy) variable and refactor month 
# have doy start in may and end in april... 

# -----------------------START GAMMS -------------------------------
m <- bam(mean_depth ~ fish_basin  + 
           s(doy_id, by = fish_basin, bs = "cc", k = 15) +
           s(floy_tag, year, by = fish_basin, bs = c("re", "re"))
         # k = c(20, 4)
         # ), 
         method = "fREML",
         family = Gamma(link = "log"),
         data = ful_depth, 
         select = TRUE
)

acf(resid_gam(m))

r1 <- start_value_rho(m, plot = TRUE, lag = 17)
r1


# gratia::

m1 <- bam(mean_depth ~ fish_basin  + 
            s(doy_id, by = fish_basin, bs = "cc", k = 15) +
            s(floy_tag, year, by = fish_basin,
              bs = c("re", "re")), 
          # k = c(20, 4)), 
          family = Gamma(link = "log"),
          method = "fREML",
          data = ful_depth, 
          select = TRUE,
          discrete = TRUE, 
          rho = r1, 
          AR.start = ful_depth$start_event
          
)

acf(resid_gam(m1))


# check model fit -----
par(mfrow = c(2, 2))
gam.check(m)
gam.check(m1)



# look at overall effect terms -----
m_overall <- anova.gam(m1, freq = FALSE)

# grab parametic overall effect 
overall_parm <- m_overall$pTerms.table %>% 
  as_tibble(rownames = "terms") %>% 
  clean_names()
# grab inddial effect 
ind_parm <- tidy(m1, parametric = TRUE) %>% 
  clean_names()
# smoother effect 
smoothers <- tidy(m1) %>% 
  clean_names()

# model comparison and fit info 
m_glance <- glance(m1) %>% 
  clean_names()


# view all model info ----

overall_parm
ind_parm
smoothers
m_glance

# =---- save summaries 

overall_parm %>%
  openxlsx::write.xlsx(here::here("Results",
                                  "Depth Results",
                                  "gamm_depth_param_overall.xlsx"))
ind_parm %>%
  openxlsx::write.xlsx(here::here("Results",
                                  "Depth Results",
                                  "gamm_depth_param_ind.xlsx"))

smoothers %>%
  openxlsx::write.xlsx(here::here("Results",
                                  "Depth Results",
                                  "gamm_depth_smoothers.xlsx"))
m_glance %>%
  openxlsx::write.xlsx(here::here("Results",
                                  "Depth Results",
                                  "gamm_depth_model_fit.xlsx"))
# # pridicted model --------

# create new datafreame with dummmy variables for RE for plotting 
dat_2 <- ful_depth %>% 
  mutate(
    floy_tag = "a",
    year = 0,
  ) 
# unique(is.na(dat_2))


glimpse(dat_2)
# rm(m)
# use prediction to get interpolated points 
fits <- predict.gam(object = m1, 
                    newdata = dat_2,
                    discrete = TRUE,
                    n.threads = 6,
                    type = "link", 
                    se = TRUE,
                    exclude = "s(floy_tag, year)"
)



# combine fits with dataframe for plotting and calc upper and lower 
# add in month abb for plotting 
predicts <- data.frame(dat_2, fits) %>% 
  mutate(
    fit = exp(fit), 
    se.fit = exp(se.fit), 
    lower = fit - 1.96 * se.fit,
    upper = fit + 1.96 * se.fit, 
    month_abb = month(date, label = TRUE, abbr = TRUE), 
    month_abb = factor(month_abb, 
                       levels = c("May", "Jun", "Jul", 
                                  "Aug", "Sep", "Oct",
                                  "Nov", "Dec", "Jan",
                                  "Feb", "Mar", "Apr"))) %>% 
  arrange(floy_tag, doy_id)

# double check that predicts looks correct 
glimpse(predicts) 

# calculate daily mean temp by fish basin 
ful_depth %>%
  group_by(doy_id, fish_basin) %>% 
  summarise(mean_depth = mean(mean_depth)) %>% 
  ungroup() -> depth_mean

# create month labels 
predicts %>% 
  filter(doy_id %in% seq(25, 350, 65)) %>% 
  group_by(year, month_abb) %>% 
  summarise() %>% 
  .$month_abb -> month_label 
month_label

# plotting prep -------

# figure out where your shading for summer and winter goes 
predicts %>% 
  group_by(season) %>% 
  summarise(first = first(doy_id),
            last = last(doy_id)) %>% 
  ungroup()

rect_summer <- tibble(
  season = "Summer",
  xmin = 32,
  xmax = 123,
  ymin = -Inf,
  ymax = Inf
)

rect_winter <- tibble(
  season = "Winter",
  xmin = 215,
  xmax = 305,
  ymin = -Inf,
  ymax = Inf
)

# ---------- plot doy gamm for 2017 - 2020 with mean daily temp ------
ggplot(predicts) +
  geom_rect(data = rect_summer, aes(xmin = xmin,
                                    xmax = xmax,
                                    ymin = ymin,
                                    ymax = ymax),
            fill = "grey80",
            alpha = 0.75,
            inherit.aes = FALSE) +
  geom_rect(data = rect_winter, aes(xmin = xmin,
                                    xmax = xmax,
                                    ymin = ymin,
                                    ymax = ymax),
            fill ="grey80",
            alpha = 0.75,
            inherit.aes = FALSE) +
  geom_text(
    aes(x = xmin + 30, y = -1.0, label = season),
    data = rect_summer,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  geom_text(
    aes(x = xmin + 32, y = -1.0, label = season),
    data = rect_winter,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 1, 
             colour = "black") + 
  geom_point(data = depth_mean, aes(x = doy_id, y = mean_depth,
                                    colour = fish_basin,
  ), alpha = 0.5, size = 3) +
  
  geom_line(
    aes(x = doy_id, y = fit, colour = fish_basin), linewidth = 1) +
  geom_ribbon( 
    aes(ymin = lower,
        ymax = upper,
        x = doy_id, y = fit,
        fill = fish_basin), alpha = 0.25) +
  scale_y_reverse(breaks = rev(seq(0, 35, 5))) +
  scale_x_continuous(breaks = seq(25, 350, 65), 
                     label = month_label) +
  scale_colour_viridis_d(name = "Basin",
                         option = "G", begin = 0.35, end = 0.75) +
  scale_fill_viridis_d(name = "Basin",
                       option = "G", begin = 0.35, end = 0.75) +
  # scale_colour_viridis_d(name = "Basin",
  #                        option = "G", begin = 0.25, end = 0.7) +
  scale_shape_discrete(name = "Basin") +
  # scale_fill_viridis_d(name = "Basin",
  #                      option = "G", begin = 0.25, end = 0.7) +
  # scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
  
  # facet_rep_wrap(.~ floy_tag, repeat.tick.labels = TRUE,
  #                # ncol = 1
  # ) +
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.05, 0.83),
        legend.background = element_blank(),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Date",
       y = "Daily Depth (m)") -> p

p
# p6

write_rds(p, here("Plot Objects", 
                  "daily_depth_GAMM_plot.rds"))


ggsave(plot = p, filename = here("plots",
                                 "Depth Use GAMM",
                                 "gamm_depth_doy_wo_ACF.png"), width = 11,
       height = 8.5)




# ------------ plot each fish's profile facted --------
dat_3 <- ful_depth 

glimpse(dat_3)

# use prediction to get interpolated points 
fits_id <- predict.bam(m1, newdata = dat_3, 
                       type = "link", se = TRUE)



# calcuate lower and upper ci per id add month abb 
pred_id <- data.frame(dat_3, fits_id) %>% 
  mutate(
    fit = exp(fit), 
    se.fit = exp(se.fit),
    lower = fit - 1.96 * se.fit,
         upper = fit + 1.96 * se.fit,
         month_abb = month(date, label = TRUE, abbr = TRUE), 
         month_abb = factor(month_abb, 
                            levels = c("May", "Jun", "Jul", 
                                       "Aug", "Sep", "Oct",
                                       "Nov", "Dec", "Jan",
                                       "Feb", "Mar", "Apr"))) %>% 
  arrange(floy_tag, doy_id)




# double check that predicts looks correct 
glimpse(pred_id)
# create month labels 
predicts %>% 
  filter(doy_id %in% seq(25, 350, 65)) %>% 
  group_by(year, month_abb) %>% 
  summarise() %>% 
  .$month_abb -> month_label 
month_label

# supp.labs <- c("3: 328 m", "5: 277 m", "6: 535 m", "9: 2372 m", 
#                "10: 1404 m", "11: 345 m", "15: 368 m", "19: 848 m", 
#                "23: 1006 m")
# names(supp.labs) <- c("3", "5", "6", "9", "10", 
#                       "11", "15", "19", "23") 
# ---- plot 2017 - 2020 gamm with mean daily temp per ID ------
ggplot(data = pred_id) +
  geom_point(aes(x = doy_id, y = mean_depth,
                 colour = fish_basin,
  ), alpha = 0.25, size = 2) +
  geom_line(aes(x = doy_id, y = fit, colour = fish_basin), linewidth = 1) +
  geom_ribbon(aes(ymin = lower,
                  ymax = upper,
                  x = doy_id, y = fit,
                  fill = fish_basin), alpha = 0.5) +
  scale_y_reverse() +
  scale_x_continuous(breaks = seq(25, 350, 65), 
                     label = month_label) +
  scale_colour_viridis_d(name = "Basin",
                         option = "G", begin = 0.35, end = 0.75) +
  scale_shape_discrete(name = "Basin") +
  scale_fill_viridis_d(name = "Basin",
                       option = "G", begin = 0.35, end = 0.75) +
  # scale_x_date(date_breaks = "4 month", date_labels = "%b %Y") + 
  
  facet_rep_grid(year ~ floy_tag, repeat.tick.labels = FALSE,
                 # ncol = 1
  ) +
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = "black"),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        # legend.position = c(0.95, 0.14),
        legend.background = element_blank(),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Date",
       y = "Daily Deph Use (m)") -> p1

p1

# ggsave(plot = p1, filename = here("plots",
# "Depth Use GAMM individual",
# "gamm_temp_2017_2021_id.png"),
# width = 11 * 4,
# height = 7 * 3)
ful_depth %>% 
  filter(floy_tag %in% "1163") %>% 
  tail()
ful %>% 
  filter(floy_tag %in% "1163") %>% 
  head() %>% 
  glimpse()

ggplot(data = pred_id %>% 
         filter(floy_tag %in% "1163")) + 
  geom_point(aes(x = doy_id, y = mean_depth,
                 colour = fish_basin,
  ), alpha = 0.25, size = 2) +
  geom_line(aes(x = doy_id, y = fit, colour = fish_basin), size = 1) +
  geom_ribbon(aes(ymin = lower,
                  ymax = upper,
                  x = doy_id, y = fit,
                  fill = fish_basin), alpha = 0.5) +
  scale_y_reverse() +
  scale_x_continuous(breaks = seq(25, 350, 65), 
                     label = month_label) +
  scale_colour_viridis_d(name = "Basin",
                         option = "G", begin = 0.35, end = 0.75) +
  scale_shape_discrete(name = "Basin") +
  scale_fill_viridis_d(name = "Basin",
                       option = "G", begin = 0.35, end = 0.75) +
  # scale_x_date(date_breaks = "4 month", date_labels = "%b %Y") + 
  facet_rep_grid(year ~., repeat.tick.labels = FALSE) +
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = "black"),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        # legend.position = c(0.95, 0.14),
        legend.background = element_blank(),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Date",
       y = "Daily Depth Use (m)") -> p2

ggsave(plot = p2, filename = here("plots",
                                  "Depth Use GAMM individual",
                                  "gamm_temp_2017_2021_1163.png"),
       width = 8.5,
       height = 11)
