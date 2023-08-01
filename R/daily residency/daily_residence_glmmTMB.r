# load packages ----
# remotes::install_github("glmmTMB/glmmTMB/glmmTMB")


{
  library(broom)
  library(broom.mixed)
  library(car)
  library(dplyr)
  library(DHARMa)
  library(emmeans)
  library(fitdistrplus)
  library(forcats)
  library(ggplot2)
  library(glmmTMB) # beta mixed models
  library(here)
  library(lemon)
  library(multcomp)
  library(lubridate)
  library(purrr)
  library(tidyr)
  library(openxlsx)
  library(readr)
  library(stringr)
}
# bring in clean downloaded ----

ri <- read_rds(here("Saved Data", 
                    "daily_ri_roi_lkt.rds"))


glimpse(ri)
unique(ri$rec_group_1)

ri <- ri %>% 
  mutate(
    fish_basin = factor(str_remove(fish_basin, " Basin"), 
                        levels = c("East", "West", "North")), 
    receiver_basin_a = factor(paste("Receiver", str_remove(receiver_basin, " Basin"), 
                                    sep = " "), 
                              levels = c("Receiver East", "Receiver West", "Receiver North")),
    receiver_basin = factor(str_remove(receiver_basin, " Basin"),
                            levels = c("East", "West", "North")),
    rec_group = fct_relevel(rec_group,
                            "North East-Basin", "Central East-Basin",
                            "Black Bay", "South East-Basin", 
                            
                            "North West-Basin", "Sucker Creek", 
                            "Central West-Basin",  "Monaco Bay",
                            "North North-Basin", "Central North-Basin",
                            "Hidden Bay"), 
    season = fct_relevel(season, "Spring", "Summer", "Fall", 
                         "Winter")
  )



ri_n <- ri %>% 
  group_by(fish_basin, receiver_basin_a, season) %>% 
  summarise(
    n = n_distinct(floy_tag)
  ) %>% 
  ungroup()
ri_n
ri_n_b <- ri %>% 
  group_by(fish_basin, season) %>% 
  summarise(
    n = n_distinct(floy_tag)
  ) %>% 
  ungroup()
ri_n_b

print(ri_n, n = 34)

ri %>% 
  group_by(fish_basin) %>% 
  summarise(
    n_distinct(floy_tag)
  ) %>% 
  ungroup()
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

# ------ model ------

m <- glmmTMB(ri_w ~ fish_basin * season +
               (1|floy_tag/receiver_basin) + 
               (1|year), 
             data = ri,
             control = glmmTMBControl(rank_check = "adjust"), 
             
             # zi = ~ rec_group * season,
             # verbose = TRUE,
             family = beta_family(link = "probit")
)


# ---- model selection ------
m1 <- update(m, .~ fish_basin +
               (1|floy_tag) + 
               (1|year))

m2 <- update(m, .~ season +
               (1|floy_tag) + 
               (1|year))

m3 <- update(m, .~ receiver_basin +
               (1|floy_tag) + 
               (1|year))

m4 <- update(m, .~ fish_basin * season +
               (1|year))

m5 <- update(m, .~ fish_basin * season +
               (1|floy_tag))

m6 <- update(m, .~ fish_basin * season +
               (1|receiver_basin))

m7 <- update(m, .~ fish_basin * season)

m8 <- update(m, .~ fish_basin * season + 
               (1|floy_tag) + 
               (1|receiver_basin))

m9 <- update(m, .~ fish_basin  + 
               (1|year) + 
               (1|receiver_basin))

m10 <- glmmTMB(ri_w ~ fish_basin * season + 
                 (1|floy_tag/receiver_basin) + 
                 (1|year), 
               # doFit = FALSE,
               data = ri,
               family = beta_family(), 
               # control = glmmTMBControl(optimizer = optim, 
               #                          optArgs = list(method = "BFGS"), 
               #                          rank_check = "adjust"
)

# create model list for model selection ------
model_list <- list(m, m1, m2, 
                   m3, m4, m5, m6, m7,
                   m8, m9, m10
                   # m11, m19, m20, m21
)
# give the elements useful names
names(model_list) <- c("m", 
                       "m1", "m2",
                       "m3", "m4", "m5", "m6", "m7",
                       "m8", "m9", 
                       "m10"
                       # "m11", "m19", "m20", "m21"
)
glance(m)

# get the summaries using `lapply

summary_list <- lapply(model_list, function(x) tidy(x, parametric = TRUE))
glance_list <- lapply(model_list, glance)

glance_summary <- map_df(glance_list, ~as.data.frame(.x), .id = "id") %>% 
  mutate(model = lapply(model_list, formula) %>%
           as.character() 
  ) %>% 
  dplyr::select(model, id:df.residual) %>% 
  arrange(AIC)

glance_summary <- glance_summary %>% 
  mutate(
    delta_AIC = AIC - first(AIC),
    AIC_weight = exp(-0.5 * delta_AIC) / sum(exp(-0.5 * delta_AIC))
  ) %>% 
  dplyr::select(model:AIC, delta_AIC, AIC_weight, BIC:df.residual)



glance_summary
glance_summary$AIC_weight
glance_summary %>% 
  write.xlsx(here("Results", 
                  "residency index", 
                  "glmmTMB_AIC_model_selection.xlsx"))


# ----- model M is our best ----- 

res <- simulateResiduals(m)
plot(res)
hist(resid(m))
par(mfrow = c(2, 2))
plot(m)
emmeans(m)
Anova(m, )
summary(m)
m$modelInfo


main_effects <- tidy(car::Anova(m))
m$fitted



ind_effects <- tidy(m)


# main_effects %>% 
# main_effects %>% 
#   openxlsx::write.xlsx(here::here("results",
#                                   "residency index",
#                                   "lmer_main_effect_RI_basin_season.xlsx"))
# ind_effects %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "residency index",
#                                   "lmer_ind_effects_RI_m_basin_season.xlsx"))

# multiple comparissions ----


Anova(comp)

comp <- emmeans(object = m, ~ fish_basin * receiver_basin * season)

mult_comp <- contrast(comp, method = "pairwise",adjust = "Tukey")
mult_comp


basin_season_contrast <- tidy(mult_comp) %>% 
  janitor::clean_names() %>% 
  arrange(adj_p_value, contrast) 


basin_season_contrast


print(basin_season_contrast, n = 66)



# 
# 
# basin_season_contrast %>%
#   # filter(adj_p_value < 0.05) %>% 
#   arrange(contrast, adj_p_value) %>%  
#   
#   openxlsx::write.xlsx(here::here("results",
#                                   "residency index",
#                                   "lmer_multi_comp_basin_season.xlsx"))
# 



# ---- plot -----

glimpse(ri)
p <- ggplot(data = ri, aes(x = fish_basin, y = ri_w)) + 
  geom_jitter(aes(colour = fish_basin), alpha = 0.5, width = 0.15, size = 2) + 
  stat_summary(fun.data = mean_se,
               geom = "errorbar", aes(x = fish_basin, y = ri_w, 
                                      group = fish_basin), width = 0.05,
               colour = "black") + 
  stat_summary(fun = mean,
               geom = "point", aes(x = fish_basin, y = ri_w), 
               fill = "white", 
               stroke = 1, 
               shape = 21,
               colour = "black", 
               size = 3) + 
  
  geom_text(data = ri_n, aes(x = fish_basin, y = -0.05, label = n)) +
  facet_grid(receiver_basin_a ~ season) + 
  scale_colour_viridis_d(begin = 0.35, end = 0.75, 
                         option = "G", name = "Basin") +
  theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank(), 
    strip.background = element_blank(), 
    legend.position = c(0.04, 0.58),
    legend.background = element_blank(),
    legend.title.align = 0.5
  ) + 
  labs(
    x = "Basin",
    y = "Daily Residency Index"
  )

# p

ggsave(filename = here("Plots", 
                       "residency index", 
                       "season_fish_basin_receiver_basin_point.png"), 
       width = 11, height = 8.5)  


sum_ri <- ri %>% 
  group_by(fish_basin, season) %>% 
  summarise(
    mean_ri = mean(ri_w), 
    sd = sd(ri_w),
    sem = sd(ri_w) / sqrt(n())
  ) %>% 
  ungroup()


p1 <- ggplot(data = ri, aes(x = receiver_basin, y = ri_w)) + 
  geom_jitter(aes(colour = fish_basin), alpha = 0.5, width = 0.15, 
              # size = 2
  ) + 
  # stat_summary(fun.data = mean_se,
  #              geom = "errorbar", aes(x = fish_basin, y = ri_w), width = 0.05,
  #              colour = "black") + 
  # stat_summary(fun = mean,
  #              geom = "point", aes(x = fish_basin, y = ri_w), shape = 21, fill = "white", 
  #              stroke = 1, 
  #              colour = "black", 
  #              size = 3) + 
  
  # geom_text(data = ri_n_b, aes(x = fish_basin, y = -0.05, label = n)) +
  facet_wrap(. ~ season) + 
  scale_colour_viridis_d(
    begin = 0.35, end = 0.85,
    option = "G", name = "Basin") +
  theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank(), 
    strip.background = element_blank(), 
    # legend.position = c(0.04, 0.25),
    legend.background = element_blank(),
    legend.title.align = 0.5
  ) + 
  labs(
    x = "Receiver Basin",
    y = "Daily Residency Index"
  )
# p1
ggsave(filename = here("Plots", 
                       "residency index", 
                       "season_fish_basin_receiver_colour.png"), 
       width = 11, height = 8.5, plot = p1)  



p2 <-  ggplot(data = ri, aes(x = fish_basin, y = ri_w)) + 
  geom_jitter(aes(colour = rec_group), alpha = 0.5, width = 0.15, size = 2) + 
  stat_summary(fun = mean,
               geom = "point", aes(x = fish_basin, y = ri_w, 
                                   group = fish_basin), colour = "black", 
               size = 3) + 
  stat_summary(fun.data = mean_se,
               geom = "errorbar", aes(x = fish_basin, y = ri_w, 
                                      group = fish_basin), width = 0.05,
               colour = "black") + 
  # geom_text(data = ri_n_b, aes(x = fish_basin, y = -0.05, label = n)) +
  facet_rep_wrap(. ~ season) + 
  scale_colour_viridis_d(
    begin = 0.35, end = 0.85,
    option = "G", name = "Receiver Group") +
  theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank(), 
    strip.background = element_blank(), 
    # legend.position = c(0.04, 0.25),
    legend.background = element_blank(),
    legend.title.align = 0.5
  ) + 
  labs(
    x = "Basin",
    y = "Daily Residency Index"
  )
p2
ggsave(filename = here("Plots", 
                       "residency index", 
                       "season_fish_basin_receiver_basin_point_rec_group.png"), 
       width = 11, height = 8.5, plot = p2)  


p3 <- ggplot(data = ri, aes(x = fish_basin, y = ri_w)) + 
  geom_jitter(aes(colour = receiver_basin), alpha = 0.5, 
              width = 0.15, size = 2) +
  geom_errorbar(
    data = sum_ri, aes(
      x = fish_basin,
      y = mean_ri,
      ymin = mean_ri - sem, 
      ymax = mean_ri + sem
    ), 
    colour = "black", 
    width = 0.05
  ) + 
  geom_point(data = sum_ri, aes(x = fish_basin, y = mean_ri), 
             shape = 21, fill = "white", 
             stroke = 1, 
             colour = "black", 
             size = 3) + 
  
  geom_text(data = ri_n_b, aes(x = fish_basin, y = -0.05, label = n)) +
  facet_wrap(. ~ season) + 
  scale_colour_viridis_d(
    begin = 0.35, end = 0.85,
    option = "G", name = "Recevier Basin") +
  theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank(), 
    strip.background = element_blank(), 
    # legend.position = c(0.04, 0.25),
    legend.background = element_blank(),
    legend.title.align = 0.5
  ) + 
  labs(
    x = "Basin",
    y = "Daily Residency Index"
  )
p3
ggsave(filename = here("Plots", 
                       "residency index", 
                       "season_fish_basin_receiver_colour_b.png"), 
       width = 11, height = 8.5, plot = p3)  



p4 <- ggplot(data = ri, aes(x = fish_basin, y = ri_w)) + 
  geom_violin(aes(fill = receiver_basin), alpha = 0.55, 
              # width = 0.35,
              # size = 2
  ) + 
  # stat_summary(fun.data = mean_se,
  #              geom = "errorbar", aes(x = fish_basin, y = ri_w), width = 0.05,
  #              colour = "black") + 
  # stat_summary(fun = mean,
  #              geom = "point", aes(x = fish_basin, y = ri_w), shape = 21, fill = "white", 
  #              stroke = 1, 
  #              colour = "black", 
  #              size = 3) + 
  
  # geom_text(data = ri_n_b, aes(x = fish_basin, y = -0.05, label = n)) +
  facet_wrap(. ~ season) + 
  scale_fill_viridis_d(
    begin = 0.35, end = 0.85,
    option = "G", name = "Recevier Basin") +
  theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank(), 
    strip.background = element_blank(), 
    # legend.position = c(0.04, 0.25),
    legend.background = element_blank(),
    legend.title.align = 0.5
  ) + 
  labs(
    x = "Basin",
    y = "Daily Residency Index"
  )
# p4

ggsave(filename = here("Plots", 
                       "residency index", 
                       "season_fish_basin_violin.png"), 
       width = 11, height = 8.5, plot = p4) 
p5 <- ggplot(data = ri, aes(x = receiver_basin, y = ri_w)) + 
  geom_jitter(aes(colour = fish_basin), alpha = 0.5, 
              # width = 0.15, 
              # size = 2
  ) + 
  # stat_summary(fun.data = mean_se,
  #              geom = "errorbar", aes(x = fish_basin, y = ri_w), width = 0.05,
  #              colour = "black") + 
  # stat_summary(fun = mean,
  #              geom = "point", aes(x = fish_basin, y = ri_w), shape = 21, fill = "white", 
  #              stroke = 1, 
  #              colour = "black", 
  #              size = 3) + 
  
  # geom_text(data = ri_n_b, aes(x = fish_basin, y = -0.05, label = n)) +
  facet_wrap(. ~ season) + 
  scale_colour_viridis_d(
    begin = 0.35, end = 0.85,
    option = "G", name = "Basin") +
  theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank(), 
    strip.background = element_blank(), 
    # legend.position = c(0.04, 0.25),
    legend.background = element_blank(),
    legend.title.align = 0.5
  ) + 
  labs(
    x = "Receiver Basin",
    y = "Daily Residency Index"
  )
# p5
ggsave(filename = here("Plots", 
                       "residency index", 
                       "season_fish_basin_receiver_colour_wide.png"), 
       width = 11, height = 8.5, plot = p5)  
