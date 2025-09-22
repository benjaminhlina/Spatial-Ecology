
# load packages ----
# library(brms) # beta mixed mobels
library(broom)

library(dplyr)
library(dunn.test)
library(emmeans)
library(car)
library(fitdistrplus)
library(forcats)
library(ggplot2)
library(lemon)
library(glmmTMB) # beta mixed models
library(glatos)
library(here)
library(lme4)
library(lemon)
library(lubridate)
library(janitor)
library(moments)
library(tidyr)
library(openxlsx)
library(readr)
library(sf)
library(sp)
library(stringr)
library(stringi)
library(qdap)
# remotes::install_github("glmmTMB/glmmTMB/glmmTMB")

# or possibly install.packages("glmmTMB", repos="https://glmmTMB.github.io/glmmTMB/repos)



# bring in clean downloaded ----

ri <- read_rds(here("Saved Data", 
                    "season_ri_lt.rds"))


glimpse(ri)

ri



length(unique(ri$floy_tag))




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


ri$ri_w[ri$ri_w == 1] <- 0.99
ri$ri_w_f[ri$ri_w_f == 1] <- 0.99
fit_beta_ri <- fitdist(data = ri$ri_w, distr = "beta", method = "mme")

plot(fit_beta_ri)

# 
fit_beta_roi <- fitdist(data = ri$roi_w, distr = "beta", method = "mme")

plot(fit_beta_roi)




# test season_year vs season 


glimpse(ri)

# no interaction 
m <- glmmTMB(ri_w ~ rec_group + season_year + (1|floy_tag),
             data = ri,
             # zi = ~ 1,
             # verbose = TRUE,
             family = beta_family())


# interaction with season_yaer doesn't converge 
m1 <- glmmTMB(ri_w ~ rec_group * season_year + (1|floy_tag), 
              data = ri, 
              family = beta_family())

# just season no interaction  converges 
m2 <- glmmTMB(ri_w ~ rec_group + season + (1|floy_tag),
              data = ri,
              # zi = ~ 1,
              # verbose = TRUE,
              family = beta_family())


glimpse(ri)
# season interaction converges and is the model we are looking for 
# 
m3 <- glmmTMB(ri_w ~ rec_group * season + (1|floy_tag) + (1|year),
              data = ri,
              # zi = ~ rec_group * season,
              # verbose = TRUE,
              family = beta_family())
m4 <- glmmTMB(ri_w ~ rec_group * season + (1|floy_tag/fish_basin) + (1|year),
              data = ri,
              # zi = ~ rec_group * season,
              # verbose = TRUE,
              family = beta_family())
# m3 is our one *******--------
m3b <- update(m3, control = glmmTMBControl(optimizer = optim, 
                                           optArgs = list(method = "BFGS")))


ri_sum <- ri %>% 
  group_by(floy_tag, rec_group, fish_basin, 
           year) %>%
  summarise(
    mean_ri = mean(ri_w), 
    median_ri = median(ri_w),
    sem_ri = sd(ri_w) / sqrt(n()), 
    n = n()
  ) %>% 
  ungroup()


ri_sum

m5 <- glmmTMB(ri_w ~ rec_group + fish_basin + season +
                (1|floy_tag), 
              data = ri,
              # zi = ~ rec_group * season,
              # verbose = TRUE,
              family = beta_family())
m6 <- glmmTMB(ri_w ~ rec_group + fish_basin * season +
                (1|floy_tag), 
              data = ri,
              # zi = ~ rec_group * season,
              # verbose = TRUE,
              family = beta_family())
# m7 is the one! 

m7 <- glmmTMB(ri_w ~ fish_basin * season +
                (1|floy_tag/rec_group) +
                (1|year), 
              data = ri,
              # zi = ~ rec_group * season,
              # verbose = TRUE,
              family = beta_family(link = "probit"))


res <- DHARMa::simulateResiduals(m7)

plot(res)

anova(m3, m3b, m4, m5, m6, m7)

Anova(m5, type = "II")
Anova(m6, type = "II")
Anova(m7, type = "II")
summary(m7)

Anova(m5, type = "III")
Anova(m3b, type = "III")
Anova(m3, type = "III")
Anova(m4, type = "III")
summary(m4)
summary(m5)



# summary(m)
# summary(m1)
# 
# 
# Anova(m, type = "III")
# Anova(m1, type = "III")
# model season + rec group, 
# model season * rec group, takes a lot of the main effect into account 
# interactions
# Anova(m2, type = "III")
Anova(m3, type = "II")




anova(m2, m3)



ri_compare <- emmeans(m3, pairwise ~ rec_group * season)
ri_compare_b <- emmeans(m3b, ~ rec_group * season)






ksy <- kruskal.test(ri_w ~ season_year, data = ri)
ks <- kruskal.test(ri_w ~ season, data = ri)

ds <- dunn.test(x = ri$ri_w, g = ri$season_year,method = "BH", kw = TRUE, list = TRUE)

str(ri_compare_b)
str(ri_compare)

ri_contrasts <- ri_compare$contrasts %>% 
  as_tibble() %>%
  clean_names() %>% 
  arrange(contrast)
ri_contrasts_b <- ri_compare_b$contrasts %>% 
  as_tibble() %>%
  clean_names() %>% 
  arrange(contrast)



ri_contrasts
ri_contrasts <- ri_contrasts %>% 
  separate(contrast, c("loc_a", "loc_b"), sep = " - ")

# View(ri_contrasts)

ri_contrasts <- ri_contrasts %>% 
  mutate(loc_a = str_remove(loc_a, pattern = "([(])"), 
         loc_a = str_remove(loc_a, pattern = "([)])"), 
         loc_b = str_remove(loc_b, pattern = "([(])"), 
         loc_b = str_remove(loc_b, pattern = "([)])")) %>% 
  arrange(loc_a)




ri_contrasts <- ri_contrasts %>% 
  mutate(loc_a = str_replace_all(loc_a, pattern = c("Fall" = "1",
                                                    "Winter" = "2",
                                                    "Spring" = "3", 
                                                    "Summer" = "4")),
         loc_b = str_replace_all(loc_b, pattern = c("Fall" = "1",
                                                    "Winter" = "2",
                                                    "Spring" = "3", 
                                                    "Summer" = "4")))


ri_contrasts <- ri_contrasts %>% 
  separate(loc_a, c("loc_a", "loc_a_season"), 
           "(?<=[a-zA-Z])\\s*(?=[0-9])") %>% 
  separate(loc_b, c("loc_b", "loc_b_season"), "(?<=[a-zA-Z])\\s*(?=[0-9])")


ri_contrasts <- ri_contrasts %>% 
  mutate(loc_a_season = str_replace_all(loc_a_season, pattern = c("1" = "Fall",
                                                                  "2" = "Winter",
                                                                  "3" = "Spring", 
                                                                  "4" = "Summer")),
         loc_b_season = str_replace_all(loc_b_season, pattern = c("1" = "Fall",
                                                                  "2" = "Winter",
                                                                  "3" = "Spring", 
                                                                  "4" = "Summer")))




ri_contrasts <- ri_contrasts %>% 
  arrange(loc_a, loc_a_season, 
          loc_b_season, loc_b)



ri_contrasts_season <- ri_contrasts %>% 
  filter(loc_a_season == loc_b_season) %>% 
  arrange(loc_a_season, loc_b_season, p_value)


ri_contrasts_season
View(ri_contrasts_season)

ri_contrasts_loc <- ri_contrasts %>% 
  filter(loc_a == loc_b) %>% 
  arrange(p_value)

write.xlsx(ri_contrasts_season,
           here::here("Fish and tagging data",
                      "Receiver Downloads",
                      "RI model results",
                      "ri_beta_mixed_model_pairwise_1_sept_21_season.xlsx"))
write.xlsx(ri_contrasts_loc,
           here::here("Fish and tagging data",
                      "Receiver Downloads",
                      "RI model results",
                      "ri_beta_mixed_model_pairwise_1_sept_21_loc.xlsx"))


# ROI comparisons -------

m4 <- glmmTMB(roi_w ~ rec_group + season_year + (1|floy_tag),
              data = ri,
              # zi = ~ 1,
              # verbose = TRUE,
              family = beta_family())


m5 <- glmmTMB(roi_w ~ rec_group * season_year + (1|floy_tag), 
              data = ri, 
              family = beta_family())

m6 <- glmmTMB(roi_w ~ rec_group + season + (1|floy_tag),
              data = ri,
              # zi = ~ 1,
              # verbose = TRUE,
              family = beta_family())
m7 <- glmmTMB(roi_w ~ rec_group * season + (1|floy_tag),
              data = ri,
              # zi = ~ 1,
              # verbose = TRUE,
              family = beta_family())


Anova(m6, type = "III")
Anova(m7, type = "III")


ksy_roi <- kruskal.test(roi_w ~ season_year, data = ri)
ks_roi <- kruskal.test(roi_w ~ season, data = ri)

ds <- dunn.test(x = ri$roi_w, g = ri$season_year,method = "BH", kw = TRUE, list = TRUE)


roi_compare <- emmeans(m7, pairwise ~ rec_group * season)


roi_contrasts <- roi_compare$contrasts %>% 
  as_tibble() %>% 
  clean_names() %>% 
  arrange(p_value)





roi_contrasts
roi_contrasts <- roi_contrasts %>% 
  separate(contrast, c("loc_a", "loc_b"), sep = " - ")

# View(ri_contrasts)

roi_contrasts <- roi_contrasts %>% 
  mutate(loc_a = str_remove(loc_a, pattern = "([(])"), 
         loc_a = str_remove(loc_a, pattern = "([)])"), 
         loc_b = str_remove(loc_b, pattern = "([(])"), 
         loc_b = str_remove(loc_b, pattern = "([)])")) %>% 
  arrange(loc_a)




roi_contrasts <- roi_contrasts %>% 
  mutate(loc_a = str_replace_all(loc_a, pattern = c("Fall" = "1",
                                                    "Winter" = "2",
                                                    "Spring" = "3", 
                                                    "Summer" = "4")),
         loc_b = str_replace_all(loc_b, pattern = c("Fall" = "1",
                                                    "Winter" = "2",
                                                    "Spring" = "3", 
                                                    "Summer" = "4")))


roi_contrasts <- roi_contrasts %>% 
  separate(loc_a, c("loc_a", "loc_a_season"), "(?<=[a-zA-Z])\\s*(?=[0-9])") %>% 
  separate(loc_b, c("loc_b", "loc_b_season"), "(?<=[a-zA-Z])\\s*(?=[0-9])")


roi_contrasts <- roi_contrasts %>% 
  mutate(loc_a_season = str_replace_all(loc_a_season, pattern = c("1" = "Fall",
                                                                  "2" = "Winter",
                                                                  "3" = "Spring", 
                                                                  "4" = "Summer")),
         loc_b_season = str_replace_all(loc_b_season, pattern = c("1" = "Fall",
                                                                  "2" = "Winter",
                                                                  "3" = "Spring", 
                                                                  "4" = "Summer")))
roi_contrasts



roi_contrasts_season <- roi_contrasts %>% 
  filter(loc_a_season == loc_b_season) %>% 
  arrange(p_value)


ri_contrasts_season

roi_contrasts_loc <- roi_contrasts %>% 
  filter(loc_a == loc_b) %>% 
  arrange(p_value)



write.xlsx(roi_contrasts_season, 
           here::here("Fish and tagging data", 
                      "Receiver Downloads", 
                      "RI model results", 
                      "roi_beta_mixed_model_pairwise_1_sept_21_season.xlsx"))
write.xlsx(roi_contrasts_loc, 
           here::here("Fish and tagging data", 
                      "Receiver Downloads", 
                      "RI model results", 
                      "roi_beta_mixed_model_pairwise_1_sept_21_loc.xlsx"))
