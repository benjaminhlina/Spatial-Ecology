# load packages ----
{
  library(broom.mixed)
  library(dplyr)
  library(DHARMa)
  library(fitdistrplus)
  library(forcats)
  library(emmeans)
  library(ggplot2)
  library(glmmTMB)
  library(here)
  library(lubridate)
  library(lemon)
  library(janitor)
  library(openxlsx)
  library(patchwork)
  library(purrr)
  library(readr)
  library(tibble)
  library(tidyr)
  source(here::here("R", 
                    "functions",
                    "julian_date_reorder.r"))
}
# bring in RDS -----

ful_depth <- read_rds(here("Saved Data",
                           "daily_depth.rds"))

tl_d <- ful_depth %>% 
  group_by(fish_basin, floy_tag, year) %>% 
  summarise(doy_min = min(doy_id)) %>% 
  ungroup()

tl_d

ful_depth <-  ful_depth %>% 
  group_by(floy_tag, year) %>% 
  arrange(floy_tag, year, doy_id) %>% 
  mutate(start_event = if_else(doy_id == min(doy_id), true = TRUE, 
                               false = FALSE)) %>% 
  ungroup() %>% 
  arrange(date, start_event) %>% 
  dplyr::select(-month, -week, -date_2) %>% 
  mutate(
    season = forcats::fct_relevel(season, 
                                  "Spring", "Summer", 
                                  "Fall", "Winter")
  )



ggplot(data = ful_depth, aes(x = mean_depth)) + 
  geom_histogram()


descdist(ful_depth$mean_depth)

gamma_dist <- fitdist(ful_depth$mean_depth, distr = "gamma", method = "mme")
plot(gamma_dist)



n_distinct(ful_depth$year)
n_distinct(ful_depth$floy_tag)


### ---- model with glmmTMB -----


# ------ lme ------
m <- glmmTMB(mean_depth ~ fish_basin * season + (1 | floy_tag) + 
               (1| year), 
             data = ful_depth, 
             family = Gamma(link = "log")
)

m1 <- update(m, . ~ fish_basin + (1 | floy_tag) + 
               (1| year),  REML = FALSE)

m2 <- update(m, . ~ season + (1 | floy_tag) + 
               (1| year),  REML = FALSE)



# check model fit ----

res <- simulateResiduals(m)
plot(res)
residuals(res)
hist(resid(m))

res_m1 <- simulateResiduals(m1)
plot(res_m1)
res_m2 <- simulateResiduals(m2)
plot(res_m2)



# create model list for model selection ------
model_list <- list(m, m1, m2
)
# give the elements useful names
names(model_list) <- c("m", 
                       "m1", "m2"
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

# view model selection ------ 
glance_summary
glance_summary %>% 
  openxlsx::write.xlsx(here::here("results",
                                  "Depth results",
                                  "lmer_model_selection_basin_season.xlsx"))

# create specific stuff for model saving -----
car::Anova(m)
summary(m)

main_effects <- tidy(car::Anova(m))



ind_effects <- tidy(m)


# main_effects %>% 
main_effects %>% 
  openxlsx::write.xlsx(here::here("results",
                                  "Depth results",
                                  "lmer_main_effect_m_basin_season.xlsx"))
ind_effects %>%
  openxlsx::write.xlsx(here::here("results",
                                  "Depth results",
                                  "lmer_ind_effects_m_basin_season.xlsx"))

# multiple comparissions ----

multi_comp <- emmeans(m,  ~ fish_basin * season, 
                      adjust = "Tukey", type = "response", 
                      # pbkrtest.limit = 3353, 
                      # lmerTest.limit = 3353
)
# contrast(multi_comp, method = "pairwise", adjust = "bonferroni")





contrast_effects <- contrast(multi_comp, method = "pairwise",
                             adjust = "Tukey")

basin_season_contrast <- tidy(contrast_effects) %>% 
  clean_names() %>% 
  arrange(adj_p_value, contrast) 


basin_season_contrast


print(basin_season_contrast, n = 66)



basin_season_contrast %>%
  # filter(adj_p_value < 0.05) %>% 
  arrange(contrast, adj_p_value) %>%  
  
  openxlsx::write.xlsx(here::here("results",
                                  "Depth Results",
                                  "lmer_multi_comp_basin_season.xlsx"))




# ---- plot ---- 
ggplot(data = ful_depth, aes(x = season, y = mean_depth)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 1, 
             colour = "black") + 
  geom_violin(aes(fill = fish_basin), alpha = 0.5
  ) +
  stat_summary(fun = mean, 
               geom = "point",  
               size = 2, position = position_dodge(0.9), 
               colour = "black", 
               aes(group = fish_basin,
                   # colour = fish_basin, 
                   
                   x = season, y = mean_depth)) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", width = 0.15,
               position = position_dodge(0.9), 
               aes(x = season, group = fish_basin, 
                   y = mean_depth)) +
  # scale_y_continuous(breaks = seq(15, 20, 2)) +
  scale_fill_viridis_d(name = "Basin",
                       option = "G", begin = 0.35, end = 0.75) +
  scale_colour_viridis_d(name = "Basin",
                         option = "G", begin = 0.35, end = 0.75) +
  scale_y_reverse() + 
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.92, 0.15),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Season",
       y = "Depth Use (m)") -> p2

# p2
# ggsave(plot = p1, filename = here("plots",
#                                   "gamm_BioE_season_basin_boxplot.png"),
#        width = 11,
#        height = 7 )
#      
#      
#      


write_rds(p2, here("Plot Objects",
                   "daily_depth_GLMM_violin_plot.rds"))


ggsave(plot = p2, filename = here("plots",
                                  "depth use GAMM ",
                                  "season_basin_violin_depth_use.png"),
       width = 11,
       height = 7)




