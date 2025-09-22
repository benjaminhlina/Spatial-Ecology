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

# ---- bring in data ----- 

dat <- read_rds(here("Saved Data", 
                     "daily_mean_sensor_sp.rds"))


dat_depth <- dat %>% 
  filter(sensor_unit == "m" & mean_value > 0)

glimpse(dat_depth)


# ---- create starting point ---- 
dat_depth <- dat_depth %>% 
  group_by(floy_tag) %>% 
  arrange(floy_tag, year, doy) %>% 
  mutate(start_event = if_else(doy == min(doy), true = TRUE, 
                               false = FALSE), 
         floy_tag = factor(floy_tag), 
         species = factor(species), 
         year = factor(year)) %>% 
  ungroup() %>% 
  arrange(doy, start_event)

glimpse(dat_depth)
# ---- look at the distribution ---- 

ggplot(data = dat_depth, aes(x = mean_value)) + 
  geom_histogram()


# ---- create gam models -----

m <- bam(mean_value ~ species +
           ti(doy, fish_basin, by = species, bs = c("cc", "fs"), 
             k = c(15, 3)) + 
           s(doy, by = species, k = 15, bs = "cc") +
           s(floy_tag, year, bs = c("re", "re")),
         method = "fREML", 
         data = dat_depth, 
         family = Gamma(link = "log"), 
         select = TRUE)

r1 <- start_value_rho(m, plot = TRUE)

r1
m1 <- bam(
  mean_value ~ species +
    ti(doy, fish_basin, by = species, bs = c("cc", "fs"), 
       k = c(15, 3)) + 
    s(doy, by = species, k = 15, bs = "cc") +
    s(floy_tag, year, bs = c("re", "re")),
  method = "fREML", 
  data = dat_depth, 
  family = Gamma(link = "log"), 
  select = TRUE, 
  discrete = TRUE, 
  rho = r1, 
  AR.start = dat_depth$start_event
)

gam.check(m)
gam.check(m1)
appraise(m)
appraise(m1)

write_rds(m1, here("model objects", 
                   "lt_smb_gamm_depth.rds"))


