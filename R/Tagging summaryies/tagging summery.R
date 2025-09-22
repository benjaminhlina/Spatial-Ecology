library(dplyr)
library(here)
library(lubridate)
library(readr)

df <- read_csv(here("Data", 
                    "all fish tagged kenauk.csv"))


df 

df <- df %>% 
  mutate(
    tag_date = as.POSIXct(tag_date, format = "%d-%b-%y"), 
    year = year(tag_date)

)
summary <- df %>% 
  group_by(
    species, vemco_type, vemco_tag, year,min_delay, max_delay 
  ) %>% 
  summarise(
    n = n()
  ) %>% 
  ungroup()
summary
  