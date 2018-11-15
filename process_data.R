library(tidyverse)
library(janitor)
library(dplyr)

JS <- read.csv("mt_2_results.csv")

JS <- JS %>%
  mutate(state_district = paste(tolower(state), district, sep = "")) %>%
  mutate(dem_votes = as.numeric(str_remove(dem_votes, ",")), rep_votes = as.numeric(str_remove(rep_votes, ",")), other_votes = as.numeric(str_remove(other_votes, ",")))
  mutate(dem_margin = (dem_votes - rep_votes) / (dem_votes + rep_votes + other_votes))

upshot <- read_rds("upshot.rds")

race <- upshot %>%
  filter(senate == FALSE, gov == FALSE, wave == 3) %>%
  group_by(state_district) %>%
  count(file_race) %>%
  mutate(n = n / sum(n) * 100) %>%
  spread(file_race, n)

final <- race %>%
  left_join(JS, by = "state_district")
