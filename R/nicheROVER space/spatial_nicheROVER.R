# ----load packages ----
library(data.table)
library(dplyr)
library(forcats)
library(glatos)
library(ggplot2)
library(ggtext)
library(here)
library(lubridate)
library(magrittr)
library(nicheROVER)
library(purrr)
library(readr)
library(stringr)
library(suncalc)
library(sf)

# ---- bring in clean downloaded ----

lt <- read_rds(here::here("Saved data", 
                          "kenauk lake trout 2017 - 2020.rds")) %>% 
  as_tibble()

p_map <- st_read(dsn = here::here("Shapefiles",
                                  "."),
                 layer = "plake_edit_wo_link")


glimpse(lt)
lt_update <- lt %>% 
  select(floy_tag, detection_timestamp_utc, date, season, name, 
         lat_mean, long_mean, fish_basin, receiver_basin, 
         passed_filter) %>% 
  mutate(rec_group = case_when(name %in% c(3, 4, 11, 19, 23)  ~ "Central East-Basin",
                               name %in% 12 ~ "Black Bay",
                               name %in% c(2, 18) ~ "South East-Basin",
                               name %in% c(1, 9, 10) ~ "North East-Basin",
                               name %in% c(5, 6) ~ "Central West-Basin",
                               name %in% c(21, 22) ~ "Monaco Bay",
                               name %in% c(7, 20) ~ "Sucker Creek",
                               name %in% c(8, 17) ~ "North West-Basin",
                               name %in% 16 ~ "Hidden Bay",
                               name %in% 15 ~ "Central North-Basin",
                               name %in% c(13, 14) ~ "North North-Basin")
  )




glimpse(lt_update)

results <- lt_update %>% 
  filter(passed_filter == 1) %>% 
  arrange(floy_tag, date) %>%
  group_by(floy_tag, date, season, fish_basin, receiver_basin, rec_group) %>% 
  summarise(
    mean_lat = mean(lat_mean),
    mean_long = mean(long_mean), 
    rec = n_distinct(name), 
    num_hits = n()
  ) %>% 
  ungroup() %>% 
  arrange(floy_tag, date)



glimpse(results)
results_sf <- results %>% 
  st_as_sf(., coords = c("mean_long", "mean_lat"), 
           crs = st_crs(p_map))


ggplot() + 
  geom_sf(data = p_map) + 
  geom_sf(data = results_sf, aes(colour = fish_basin), 
          size = 3, alpha = 0.5) + 
  facet_wrap(.~ season) + 
  scale_colour_viridis_d(name = "Basin",
                       option = "G", begin = 0.25, end = 0.75, 
                       alpha = 0.5)


nsample <- 1000



fish_par <- results %>% 
  group_split(season, fish_basin) %>% 
  map(~ select(., mean_long, mean_lat)) %>% 
  map(~niw.post(nsample = nsample, X = .))

glimpse(fish_par)
df_mu <- map(fish_par, pluck, 1) %>% 
  imap(~ as_tibble(.x) %>% 
         mutate( 
           metric = "mu", 
           id = .y
         )
  ) %>%
  bind_rows() %>%
  mutate(
    season = factor(case_when(
      id %in% c(1, 2, 3) ~ "Fall",
      id %in% c(4, 5, 6) ~ "Winter",
      id %in% c(7:9) ~ "Spring",
      id %in% c(10:12) ~ "Summer",
    ), levels = c("Fall", 
                  "Winter",
                  "Spring",
                  "Summer")
  ), 
  fish_basin = factor(case_when(
    id %in% c(1, 4, 7, 10) ~ "North",
    id %in% c(2, 5, 8, 11) ~ "West",
    id %in% c(3, 6, 9, 12) ~ "East"
    ), levels = c("East", "West",
                  "North")
  )
) %>% 
  group_by(id) %>% 
  mutate(
    sample_number = 1:1000
  ) %>% 
  ungroup()


df_mu
# 
df_mu_long <- df_mu %>%
  pivot_longer(cols = -c(metric, id, season, fish_basin, sample_number),
               names_to = "location",
               values_to = "mu_est") %>% 
  mutate(
    type = case_when(
      location == "mean_long" ~ "Longitude",
      location == "mean_lat" ~ "Latitude",
      
    )
  )



df_sigma <- map(fish_par, pluck, 2) %>%
  imap(~ as_tibble(.x) %>%
         mutate(
           metric = "sigma",
           id_location = c("mean_long", "mean_lat"),
           id = .y
         )
  ) %>%
  bind_rows() %>% 
  pivot_longer(cols = -c(id_location, id, metric),
               names_to = "location",
               values_to = "post_sample"
  ) %>% 
  separate(location, into = c("location", "sample_number"), sep = "\\.") %>% 
  mutate(
    season = factor(case_when(
      id %in% c(1, 2, 3) ~ "Fall",
      id %in% c(4, 5, 6) ~ "Winter",
      id %in% c(7:9) ~ "Spring",
      id %in% c(10:12) ~ "Summer",
    ), levels = c("Fall", 
                  "Winter",
                  "Spring",
                  "Summer")
    ), 
    fish_basin = factor(case_when(
      id %in% c(1, 4, 7, 10) ~ "North",
      id %in% c(2, 5, 8, 11) ~ "West",
      id %in% c(3, 6, 9, 12) ~ "East"
    ), levels = c("East", "West",
                  "North")
    )
  )

unique(df_sigma$id)



df_sigma_cn <- df_sigma %>%
  filter(id_location != location)


#| out-width: 100%
#| fig-format: svg
posterior_plots <- df_mu_long %>%
  split(.$location) %>%
  imap(
    ~ ggplot(data = ., aes(x = mu_est)) +
      geom_density(aes(fill = fish_basin), alpha = 0.5) +
      scale_fill_viridis_d(begin = 0.25, end = 0.75,
                           option = "G", name = "Species") +
      facet_wrap(.~ season) + 
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.title.x =  element_markdown(),
            axis.title.y =  element_markdown(),
            legend.position = "none"
      ) +
  
      labs(
        x = paste("\u00b5", "<sub><sup>",
                  unique(.$type), "</sup></sub>", sep = ""),
        y = paste0("p(\u00b5","<sub><sup>",
                   unique(.$type), "</sub></sup>",
                   " | X)", sep = "")
  )
  )

p3 <- posterior_plots[[1]] / posterior_plots[[2]]

p3

# 0.35, end = 0.75
p4 <- ggplot(data = df_sigma_cn, aes(x = post_sample)) +
  geom_density(aes(fill = fish_basin), alpha = 0.5) + 
  scale_fill_viridis_d(begin = 0.35, end = 0.75, 
                       option = "D", name = "Basin") + 
  facet_wrap(.~ season) + 
  # scale_y_continuous(breaks = seq(0, 2, 0.5), 
  #                    # limits = c(0, 3)
  # ) +  
  theme_bw(base_size = 15) + 
  theme(
    # legend.position = "none",
        panel.grid = element_blank()) +
  labs(x = expression(paste(Sigma[~Longitude], " ", ""[Latitude])),
       y = expression(paste("p(",Sigma[~Longitude], " ", 
                            ""[~Latitude],""," ", "| X)")))
p4




df_sigma_wide_1 <- df_sigma %>%
  pivot_wider(names_from = id_location,
              values_from = post_sample) %>% 
  mutate(
    split_id = paste(season, fish_basin, sep = "_")
  )

df_sigma_wide
df_sigma_wide_1
p.ell <- 0.95



species_name <- unique(df_sigma_wide_1$split_id)


length(species_name)
all_ellipses <- list()



for (i in 1:length(species_name)) {
  
  sigma_species <- df_sigma_wide_1 %>% 
    filter(split_id %in% species_name[i])
  
  mu_species <- df_mu %>% 
    filter(id %in% id[i])
  
  ell <- NULL
  post.id <- NULL
  for(j in 1:length(unique(sigma_species$sample_number))) {
    # j <- 1
    sigma_ind <- sigma_species %>%
      filter(sample_number %in% sample_number[j]) %>% 
      dplyr::select(mean_long, mean_lat) 
    
    Sigma <- as.matrix(sigma_ind, 2, 2)
    row.names(Sigma) <- c("mean_long", "mean_lat")
    
    mu <- mu_species %>%
      filter(sample_number %in% sample_number[j]) %>% 
      dplyr::select(sample_number, mean_long, mean_lat) %>% 
      pivot_longer(cols = -sample_number, 
                   names_to = "location", 
                   values_to = "mu") %>% 
      .$mu
    
    out <- ellipse::ellipse(Sigma, centre = mu, which = c(1, 2), level = p.ell)
    
    ell <- rbind(ell, out)
    post.id <- c(post.id, rep(j, nrow(out)))
  }
  ell <- as.data.frame(ell)
  ell$rep <- post.id
  all_ellipses[[i]] <- ell
}

ellipse_df <- bind_rows(all_ellipses, .id = "id") %>% 
  mutate(
    season = factor(case_when(
      id %in% c(1, 2, 3) ~ "Fall",
      id %in% c(4, 5, 6) ~ "Winter",
      id %in% c(7:9) ~ "Spring",
      id %in% c(10:12) ~ "Summer",
    ), levels = c("Fall", 
                  "Winter",
                  "Spring",
                  "Summer")
    ), 
    fish_basin = factor(case_when(
      id %in% c(1, 4, 7, 10) ~ "North",
      id %in% c(2, 5, 8, 11) ~ "West",
      id %in% c(3, 6, 9, 12) ~ "East"
    ), levels = c("East", "West",
                  "North")
    )
  )

ellipse_df %>% 
  group_by(season, fish_basin, rep) %>% 
  nest() %>%
  group_by(season, fish_basin) %>% 
  slice_sample(n = 10, replace = TRUE) %>% 
  ungroup() %>% 
  unnest(cols = c(data)) -> random_ellipse   


ellipse_plots <- ggplot() + 
  geom_polygon(data = random_ellipse,
               mapping = aes(x = mean_long, y = mean_lat,
                             group = interaction(rep, fish_basin),
                             color = fish_basin),
               fill = NA,
               linewidth = 0.5) + 
  facet_wrap(.~ season) + 
  scale_colour_viridis_d(begin = 0.25, end = 0.75, 
                         option = "D", name = "Basin",
  ) + 
  # scale_x_continuous(breaks = rev(seq(-20, -40, -2))) +
  # scale_y_continuous(breaks = seq(6, 16, 2)) +
  theme_bw(base_size = 10) +
  theme(axis.text = element_text(colour = "black"),
        panel.grid = element_blank(), 
        # legend.position = "none", 
        legend.title.align = 0.5,
        legend.background = element_blank()) + 
  labs(x = "Longitude", 
       y = "Latitude")
ellipse_plots


glimpse()


results_long <- results %>% 
  pivot_longer(cols = -c(floy_tag, date, season, fish_basin, receiver_basin,
                         rec_group, rec, num_hits),
               names_to = "location", 
               values_to = "value")
results_long %>% 
  group_split(location) %>% 
  imap(
    ~ ggplot(data = .) + 
      geom_density(aes(x = value, 
                       fill = fish_basin), 
                   alpha = 0.35, 
                   linewidth = 0.8) +
      scale_fill_viridis_d(begin = 0.35, end = 0.75,
                           option = "G", name = "fish_basin") +
      theme_bw(base_size = 15) +
      facet_wrap(.~ season) + 
      theme(axis.text = element_text(colour = "black"),
            panel.grid = element_blank(), 
            legend.position = c(0.08, 0.9), 
            legend.title.align = 0.5,
            legend.background = element_blank(), 
            axis.title.x = element_markdown(family = "sans")) + 
      labs(x =   unique(.$location), 
           y = "Density")
  )



over_stat <- overlap(fish_par, nreps = nsample, nprob = 1000, 
                    alpha = 0.95)



over_stat_df <- over_stat %>% 
  as_tibble(rownames = "season_id") %>% 
  mutate(
    id = 1:nrow(.), 
    season_id = as.numeric(season_id), 
    season_a = factor(case_when(
      season_id %in% c(1, 2, 3) ~ "Fall",
      season_id %in% c(4, 5, 6) ~ "Winter",
      season_id %in% c(7:9) ~ "Spring",
      season_id %in% c(10:12) ~ "Summer",
    ), levels = c("Fall", 
                  "Winter",
                  "Spring",
                  "Summer")
    ), 
    fish_basin_a = factor(case_when(
      season_id %in% c(1, 4, 7, 10) ~ "North",
      season_id %in% c(2, 5, 8, 11) ~ "West",
      season_id %in% c(3, 6, 9, 12) ~ "East"
    ), levels = c("East", "West",
                  "North")
    )
  ) %>%  
  pivot_longer(cols = -c(id, season_id, season_a, fish_basin_a), 
               names_to = "season_b", 
               values_to = "mc_nr") %>% 
  mutate(
    season_b = as.numeric(str_remove(season_b, "V")),
    season_c = factor(case_when(
      season_b %in% c(1:3000) ~ "Fall",
      season_b %in% c(3001:6000) ~ "Winter",
      season_b %in% c(6001:9000) ~ "Spring",
      season_b %in% c(9001:12000) ~ "Summer", 
    ), 
    levels = c("Fall", 
               "Winter",
               "Spring",
               "Summer")),
    fish_basin_b = factor(case_when(
      season_b %in% c(1:1000, 3001:4000, 6001:7000, 9001:10000) ~ "North",
      season_b %in% c(1001:2000, 4001:5000, 7001:8000, 10001:11000) ~ "West",
      season_b %in% c(2001:3000, 5001:6000, 8001:9000, 11001:12000) ~ "East"
    ), levels = c("East", "West",
                  "North")
  )) %>% 
  # rename(season_b = season_c) %>% 
  mutate(
    mc_nr_perc = mc_nr * 100
  )

unique(over_stat_df$fish_basin_b)
over_sum <- over_stat_df %>% 
  group_by(season_a, fish_basin_a, season_c, fish_basin_b) %>% 
  summarise(
    mean_mc_nr = round(mean(mc_nr_perc, na.rm = TRUE), digits = 2),
    qual_2.5 = round(quantile(mc_nr_perc, probs = 0.025, na.rm = TRUE), digits = 2), 
    qual_97.5 = round(quantile(mc_nr_perc, probs = 0.975, na.rm = TRUE), digits = 2)
  ) %>% 
  ungroup() %>% 
  pivot_longer(cols = -c(season_a, fish_basin_a, season_c, fish_basin_b, mean_mc_nr),
               names_to = "percentage",
               values_to = "mc_nr_qual") %>%
  mutate(
    percentage = as.numeric(str_remove(percentage, "qual_"))
  )


#| out-width: 100%
#| fig-format: svg
ggplot(data = over_stat_df, aes(x = mc_nr_perc)) + 
  geom_density(aes(fill = fish_basin_a)) + 
  # geom_vline(data = over_sum, aes(xintercept = mean_mc_nr),
  #            colour = "black", linewidth = 1) +
  # geom_vline(data = over_sum, aes(xintercept = mc_nr_qual),
  #            colour = "black", linewidth = 1, linetype = 6) +
  scale_fill_viridis_d(begin = 0.25, end = 0.75,
                       option = "D", name = "Species", 
                       alpha = 0.35) + 
  facet_grid(season_a ~ fish_basin_b, 
                     # independent = "y",
                     scales = "free_y") + 
  theme_bw() + 
  theme(
    panel.grid = element_blank(), 
    axis.text = element_text(colour = "black"), 
    legend.background = element_blank(),
    strip.background = element_blank()
  ) +
  labs(x = paste("Overlap Probability (%)", "\u2013", 
                 "Niche Region Size: 95%"), 
       y = "p(Percent Overlap | X)")



