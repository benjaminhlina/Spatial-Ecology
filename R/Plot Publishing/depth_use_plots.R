# load packages ----

library(dplyr)
library(ggplot2)
# library(ggh4x)
library(here)
# library(lemon)
library(patchwork)
library(readr)


# bring in both plots -----

gam <- read_rds(here("Plot Objects", 
                     "daily_depth_GAMM_plot.rds"))


vi <- read_rds(here("Plot Objects", 
                    "daily_depth_GLMM_violin_plot.rds"))


gam$layers
vi
gam

# gam$layers[[3]] <- NULL
# gam$layers[[3]] <- NULL

# max_rmr <- vi$data %>% 
#   group_by(fish_basin, season) %>% 
#   summarise(
#     max = min(mean_depth)
#   ) %>% 
#   ungroup() %>% 
#   arrange(season, fish_basin)

sig_let <- tibble(
  letter = c(rep("A", 3), rep("B", 3), 
             rep("C", 3), "DE", "E", "DF"),
  x = c(0.7, 1, 1.3, 1.7, 2, 2.3, 
        2.7, 3, 3.3, 3.7, 4, 4.3),
  y = rep(-2.5, 12)
)


gam <- gam + 
  theme(
    legend.position = c(0.95, 0.10),
    # plot.tag.position = c(0.1, 0.99),
    plot.tag = element_text(face = "bold")
  )


vi_2 <- vi + 
  geom_text(data = sig_let, aes(x = x, 
                                y = y, 
                                label = letter), 
            size = 5) + 
  theme(
    legend.position = "none", 
    # plot.tag.position = c(0.1, 0.99),
    plot.tag = element_text(face = "bold")
  )

p3 <- gam / vi_2 + 
  plot_annotation(tag_levels = "a", 
                  tag_suffix = ")")

# p3

ggsave(plot = p3, filename = here("plots",
                                  "Combined GAMM and Violin", 
                                  "gamm_violin_depth_doy.png"), 
       width = 11,
       height = 7 * 2)

