
# load packages ----
library(boot)
library(car)
library(compiler)
library(dplyr)
library(emmeans)
library(glmmTMB)
library(ggplot2)
library(GGally)
library(here)
library(janitor)
library(lattice)
library(lemon)
library(lme4)
library(parallel)
library(openxlsx)
library(readr)
library(tidyr)





# bring in clean downloaded ----

ri <- read_rds(here("fish and tagging data", 
                    "Receiver Downloads", 
                    "Residency Index Data", 
                    "season_ri_lt.rds"))



glimpse(ri)
ri <- ri %>% 
  arrange(floy_tag, season_year)
tail(ri)

ri_sum <- ri %>% 
  group_by(fish_basin, receiver_basin) %>% 
  summarise(n = n_distinct(floy_tag)) %>% 
  ungroup()



ri_sum



ri_wide <- ri_sum %>% 
  pivot_wider(id_cols = fish_basin, names_from = receiver_basin, 
              values_from = n) %>% 
  clean_names()


ri_wide
ri_matrix <- as.matrix(ri_wide)[,-c(1)]

ri_matrix <- apply(ri_matrix, 2, as.numeric)


rownames(ri_matrix) <- c("f_north_basin", 
                         "f_west_basin", 
                         "f_east_basin")


colnames(ri_matrix) <- c("r_north_basin", 
                         "r_west_basin", 
                         "r_east_basin")


ri_matrix



fisher.test(ri_matrix)


chi_test <- chisq.test()

chi_test_sim <- chisq.test(ri_matrix, simulate.p.value = TRUE)



chi_test_sim

chi_test_sim$expected
chi_test_sim$observed


# use fishers extat test due to to small of expected valuses 
# less than 5 or even less than 10 ------

fisher_test <- janitor::fisher.test(ri_matrix)


fisher_test
# chi square test isn't going to cut it so make binnary and run logistic ----

# fix this ----

ri_basin_binomial <- ri %>% 
  group_by(fish_basin, .drop = FALSE) %>% 
  group_by(floy_tag, receiver_basin) %>% 
  summarise(n = n_distinct(floy_tag)) %>% 
  ungroup()


ri_basin_binomial

ri_basin <- ri %>% 
  group_by(floy_tag, fish_basin) %>% 
  summarise(n = n()) %>% 
  ungroup()


ri_basin <- ri_basin %>% 
  select(floy_tag, fish_basin)

ri_basin_binomial <- ri_basin_binomial %>% 
  left_join(ri_basin, by = "floy_tag")


ri_basin_binomial <- ri_basin_binomial %>% 
  select(floy_tag, fish_basin, receiver_basin, n)

ri_basin_binomial

ri_sums <- ri_basin_binomial %>% 
  group_by(fish_basin, receiver_basin) %>% 
  summarise(present = sum(n)) %>% 
  ungroup()
ri_sums
ri_sum

ri_basin_binomial <- ri_basin_binomial %>% 
  mutate(floy_tag = as.factor(floy_tag))

ri_basin_binomial 

glimpse(ri_basin_binomial)

ri_basin_binomial <- ri_basin_binomial %>% 
  mutate(fish_basin = case_when(fish_basin %in% "North Basin" ~ "f_north_basin",
                                fish_basin %in% "West Basin" ~ "f_west_basin",
                                fish_basin %in% "East Basin" ~ "f_east_basin"), 
         receiver_basin = case_when(receiver_basin %in% "North Basin" ~ "r_north_basin",
                                    receiver_basin %in% "West Basin" ~ "r_west_basin",
                                    receiver_basin %in% "East Basin" ~ "r_east_basin")
  )



glimpse(ri_basin_binomial)



m <- glmer(n ~ fish_basin * receiver_basin + (1 | floy_tag),
           data = ri_basin_binomial, family = binomial)

m1 <- glmer(n ~ fish_basin * receiver_basin + (1 | floy_tag),
            data = ri_basin_binomial, family = binomial, 
            control = glmerControl(optimizer = "bobyqa", 
                                   optCtrl = list(maxfun = 2e5)),
            nAGQ = 10)





?isSingular
summary(m)
summary(m1)

anova(m, m1)
Anova(m1, type = "III")

m3 <- glm(n ~ fish_basin + receiver_basin, data = ri_basin_binomial, 
          family = binomial)


summary(m3)

Anova(m3, type = "III")


anova(m, m3)


comparisons <- emmeans(m3, pairwise ~ fish_basin + receiver_basin)

tidy_results <- broom::tidy(comparisons$contrasts) %>% 
  clean_names() %>% 
  arrange(adj_p_value)

tidy_results
View(tidy_results)

# 
# write.xlsx(tidy_results, here("Fish and tagging data", 
#                               "Receiver Downloads", 
#                               "Basin Results", 
#                               "binomial_glm_contrast_table_mixed_effects.xlsx"))




m4 <- glmmTMB(n ~ fish_basin * receiver_basin + (1|floy_tag), 
              data = ri_basin_binomial, family = binomial,
              control = glmmTMBControl(optimizer = optim,
              optArgs = list(method = "BFGS"))
)




summary(m4)
Anova(m4, type = "III")
Anova(m1, type = "III")

# anova(m1)


comp <- emmeans(m4, pairwise ~ fish_basin * receiver_basin)

comps <- broom::tidy(comp$contrasts) %>% 
  arrange(adj.p.value)



comps
# ggplot(data = ri_basin_binomial, aes(x = interaction(fish_basin, receiver_basin), 
#                                      y = n)) + 
#   geom_smooth(method = "glm", method.args = list(family = binomial))
# 
# 
# 
# 
#                  
# predicted_basin <- predict(m3, newdata = ri_basin_binomial, 
#                           type = "response", 
#                           interval = "confidence",
#                           # se = TRUE
#                           )
# df <- cbind(ri_basin_binomial, data.frame(predicted_basin)) %>% 
#   as_tibble()
# 
# df
# 
# 
# ggplot(data = df, aes(x = fish_basin, group = receiver_basin,
#                       y = predicted_basin)) + 
#   geom_boxplot(aes(group = receiver_basin, fill = receiver_basin), 
#                alpha = 0.3)
