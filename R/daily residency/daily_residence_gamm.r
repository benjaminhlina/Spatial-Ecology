
# load packages ----
# library(brms) # beta mixed mobels
library(broom)
library(car)
library(dplyr)
library(emmeans)
library(fitdistrplus)
library(ggplot2)

library(glmmTMB) # beta mixed models
library(here)
library(lemon)
library(lubridate)
library(mgcv)
library(janitor)
library(purrr)
library(tidyr)
library(openxlsx)
library(readr)
library(stringr)
library(stringi)
# remotes::install_github("glmmTMB/glmmTMB/glmmTMB")

# or possibly install.packages("glmmTMB", repos="https://glmmTMB.github.io/glmmTMB/repos)



# bring in clean downloaded ----

ri <- read_rds(here("Saved Data", 
                    "daily_ri_roi_lkt.rds"))


glimpse(ri)


# assess ri and roaming index distrubtions -------
descdist(ri$ri_w)
descdist(ri$roi_w)


# plot histogram 

ggplot(data = ri, aes(x = ri_w)) + 
  geom_histogram()



ggplot(data = ri, aes(x = roi_w)) + 
  geom_histogram()

summary(ri$roi_w)
summary(ri$ri_w)


ri_90 <- ri %>% 
  filter(ri_w > 0.9)

ri_90

glimpse(ri)

ri <- ri %>% 
  mutate(
    doy = yday(date), 
    year = as.factor(year), 
    floy_tag = as.factor(floy_tag), 
    ri_w = case_when(
      ri_w == 1 ~ 0.99, 
      TRUE ~ ri_w
    )
  ) 

ri_sum <- ri %>% 
  group_by(doy, season, year, fish_basin) %>% 
  summarise(
    mean_ri = mean(ri_w),
    sem_ri = sd(ri_w, na.rm = TRUE) / sqrt(n()),
    n = n()
  ) %>% 
  ungroup()


ri_sum_2 <-   ri %>% 
  group_by(floy_tag, doy, season, year, fish_basin, receiver_basin) %>% 
  summarise(
    mean_ri = mean(ri_w),
    sem_ri = sd(ri_w, na.rm = TRUE) / sqrt(n())
  ) %>% 
  ungroup()

glimpse(ri_sum)



ggplot(data = ri_sum_2, aes(x = doy, y = mean_ri)) + 
  geom_point(size = 3, aes(colour = fish_basin)) + 
  facet_grid(fish_basin ~ receiver_basin) 




ggplot(data = ri_sum, aes(x = doy, y = mean_ri)) + 
  geom_point(aes(colour = fish_basin)) + 
  facet_rep_wrap(. ~ rec_group) + 
  scale_colour_viridis_d(end = 0.8, begin = 0.2, name = "Basin") 



ri %>% 
  summarise(
    nt = n_distinct(floy_tag), 
    ny = n_distinct(year) 
  )


# m <- read_rds(here("model objects",
#                    "daily_residence_gamm_update.rds"))

m <- bam(mean_ri~ fish_basin * receiver_basin +
           s(doy, by = fish_basin, bs = "cc", k = 15) +
           s(doy, by = receiver_basin, bs = "cc", k = 15) +
           s(floy_tag, year, by = fish_basin, bs = c("re", "re"),
             k = c(51, 5)),
         method = "fREML",
         family = betar(), 
         # discrete = TRUE,
         data = ri_sum_2,
         select = TRUE, nthreads = c(8, 4)
)

gratia::appraise(m)


# m1 <- bam(mean_ri~ fish_basin  + 
#            s(doy, by = fish_basin, bs = "cc", k = 15) +
#            # s(doy, by = receiver_basin, bs = "cc", k = 15) +
#            s(floy_tag, year, by = fish_basin, bs = c("re", "re"),
#              k = c(51, 5)), 
#          method = "fREML",
#          family = betar(thet),
#          data = ri_sum_2,
#          select = TRUE, nthreads = 8 
#          )

par(mfrow = c(2, 2))
gam.check(m)
anova.gam(m)

glimpse(ri)

ri_sum_3 <- ri_sum_2 %>% 
  group_by(doy, fish_basin, receiver_basin) %>% 
  summarise(
    mr = mean(mean_ri)
  ) %>% 
  ungroup() %>% 
  mutate(
    floy_tag = "a",
    year = 0
  ) %>% 
  rename(mean_ri = mr) %>% 
  dplyr::select(floy_tag, doy, year, fish_basin, receiver_basin, mean_ri)



pred <- broom.mixed::augment(m, newdata = ri_sum_3, 
                             type.predict = "response",
                             se = TRUE, exclude = "s(floy_tag, year") %>% 
  mutate(
    lower = .fitted - (1.96 * .se.fit),
    upper = .fitted + (1.96 * .se.fit)
    # .ftted =  ((1 / .fitted) / (1 - (.fitted / 1)))
  )
pred



ggplot() +
  geom_point(data = ri_sum_3, aes(x = doy, y = mean_ri, colour = fish_basin), 
             alpha = 0.5, size = 3) + 
  geom_line(data = pred, aes(x = doy, y = .fitted, 
                             colour = fish_basin), 
            linewidth = 1) + 
  geom_ribbon(data = pred, 
    aes(ymin = lower,
        ymax = upper,
        x = doy, y = .fitted,
        fill = fish_basin), alpha = 0.25) +
        facet_grid(fish_basin ~ receiver_basin) +
  scale_colour_viridis_d(name = "Basin",
                         option = "G", begin = 0.35, end = 0.75) +
  scale_fill_viridis_d(name = "Basin",
                         option = "G", begin = 0.35, end = 0.75) +
  
  facet_rep_grid(receiver_basin ~ fish_basin, repeat.tick.labels = FALSE,
                 # ncol = 1
  ) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), 
                     limits = c(0, 1)) + 
  theme_bw(base_size = 15) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = "black"),
        strip.background = element_blank(),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.95, 0.14),
        legend.background = element_blank(),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Date",
       y = "Daily Residency Index (%)")


write_rds(m, here("model objects",
                  "daily_residence_gamm_update.rds"))
write_rds(m1, here("model objects",
                   "daily_residence_gamm_wo_receiver_basin.rds"))




ri_sum_4 <- ri_sum_2 %>% 
  group_by(doy, fish_basin) %>% 
  summarise(
    mr = mean(mean_ri)
  ) %>% 
  ungroup() %>% 
  mutate(
    floy_tag = "a",
    year = 0
  ) %>% 
  rename(mean_ri = mr) %>% 
  dplyr::select(floy_tag, doy, year, fish_basin, mean_ri)

pred <- broom.mixed::augment(m1, newdata = ri_sum_4, exclude = "s(floy_tag, year")

pred <- pred


ggplot(data = pred, aes(x = doy, y = .fitted)) + 
  geom_line(linewidth = 1, aes(colour = fish_basin)) + 
 
  scale_colour_viridis_d(name = "Basin",
                         option = "G", begin = 0.35, end = 0.75) +
  
  facet_rep_wrap(. ~ fish_basin, repeat.tick.labels = FALSE,
                 # ncol = 1
  ) +
  theme_bw(base_size = 15) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = "black"),
        strip.background = element_blank(),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.95, 0.14),
        legend.background = element_blank(),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Date",
       y = "Daily Residency Index (m)")
# ri_sum_day <- ri %>% 
#   group_by(floy_tag, season, year, fish_basin, rec_group
#   ) %>% 
#   summarise(
#     mean_ri = mean(ri_w)
#   ) %>% 
#   ungroup()
# ri_sum_day <- ri_sum_day %>% 
#   mutate(
#   mean_ri = case_when(mean_ri == 1 ~ 0.99, 
#                       TRUE ~ mean_ri)
#     )
# 
# season_model <- function(df) {
#   glmmTMB(mean_ri ~ rec_group * fish_basin + 
#        (1|floy_tag) + 
#        (1|year),
#      data = df,
#      # zi = ~ rec_group * season,
#      # verbose = TRUE,
#      family = beta_family()
#   )
# }
# 
# 
# by_season <- ri_sum_day %>% 
#   group_by(season) %>% 
#   nest()
# 
# models <- map(by_season$data, season_model)
# 
# lapply(models, Anova, type = "III")
# 
