library(tidyverse)
library(janitor)
library(dplyr)

JS <- read.csv("mt_2_results.csv")

JS <- JS %>%
  mutate(state_district = paste(tolower(state), district, sep = "")) %>%
  mutate(dem_votes = as.numeric(str_remove(dem_votes, ",")), rep_votes = as.numeric(str_remove(rep_votes, ",")), other_votes = as.numeric(str_remove(other_votes, ","))) %>%
  replace(is.na(.), 0) %>%
  mutate(dem_margin = (dem_votes - rep_votes) / (dem_votes + rep_votes + other_votes) * 100)

upshot <- read_rds("upshot.rds")

race <- upshot %>%
  filter(senate == FALSE, gov == FALSE, wave == 3) %>%
  group_by(state_district) %>%
  count(file_race) %>%
  spread(file_race, n) %>%
  replace(is.na(.), 0) %>%
  mutate(non_white = (Asian + Black + Hispanic + Other + Unknown) / (Asian + Black + Hispanic + Other + Unknown + White) * 100) %>%
  left_join(JS, by = "state_district") %>%
  select(state_district, dem_margin, non_white)

write_rds(race, "race.rds")

education <- upshot %>%
  filter(senate == FALSE, gov == FALSE, wave == 3) %>%
  group_by(state_district) %>%
  count(educ) %>%
  spread(educ, n) %>%
  mutate(higher_ed = "Graduate or Professional Degree" / ("Bachelors' degree" + "Grade school" + "Graduate or Professional Degree" + "High school" + "Some college or trade school") * 100) %>%
  left_join(JS, by = "state_district") %>%
  select(state_district, dem_margin, non_white)