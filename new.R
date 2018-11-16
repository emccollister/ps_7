library(tidyverse)
library(janitor)
library(dplyr)

JS <- read.csv("mt_2_results.csv")

upshot <- read_rds("upshot.rds")

dem_pred <- upshot %>%
  filter(senate == FALSE, gov == FALSE, wave == 3) %>%
  group_by(state_district) %>%
  count(response) %>%
  spread(response, n) %>%
  mutate(dem_predicted = Dem / (Dem + Rep + Und) * 100)

JS <- JS %>%
  mutate(state_district = paste(tolower(state), district, sep = "")) %>%
  mutate(dem_votes = as.numeric(str_remove(dem_votes, ",")), rep_votes = as.numeric(str_remove(rep_votes, ",")), other_votes = as.numeric(str_remove(other_votes, ","))) %>%
  replace(is.na(.), 0) %>%
  mutate(dem_actual = (dem_votes) / (dem_votes + rep_votes + other_votes) * 100) %>%
  left_join(dem_pred, by = "state_district") %>%
  mutate(dem_margin = abs(dem_actual - dem_predicted))

race <- upshot %>%
  filter(senate == FALSE, gov == FALSE, wave == 3) %>%
  group_by(state_district) %>%
  count(file_race) %>%
  spread(file_race, n) %>%
  replace(is.na(.), 0) %>%
  mutate(non_white = (Asian + Black + Hispanic + Other + Unknown) / (Asian + Black + Hispanic + Other + Unknown + White) * 100) %>%
  left_join(JS, by = "state_district") %>%
  select(state_district, dem_margin, non_white)

write_rds(race, "new_shiny/race.rds")

education <- upshot %>%
  filter(senate == FALSE, gov == FALSE, wave == 3) %>%
  group_by(state_district) %>%
  count(educ) %>%
  spread(educ, n) %>%
  replace(is.na(.), 0) %>%
  mutate(higher_ed = `Graduate or Professional Degree` / (`Bachelors' degree` + `Grade school` + `Graduate or Professional Degree` + `High school` + `Some college or trade school`) * 100) %>%
  left_join(JS, by = "state_district") %>%
  select(state_district, dem_margin, higher_ed)

write_rds(education, "new_shiny/education.rds")

genballot <- upshot %>%
  filter(senate == FALSE, gov == FALSE, wave == 3) %>%
  group_by(state_district) %>%
  count(genballot) %>%
  spread(genballot, n) %>%
  replace(is.na(.), 0) %>%
  mutate(genballot = `Dems. take House` / (`Dems. take House` + `Don't know` + `Reps. keep House`) * 100) %>%
  left_join(JS, by = "state_district") %>%
  select(state_district, dem_margin, genballot)

write_rds(genballot, "new_shiny/genballot.rds")

women <- upshot %>%
  filter(senate == FALSE, gov == FALSE, wave == 3) %>%
  group_by(state_district) %>%
  count(gender_combined) %>%
  spread(gender_combined, n) %>%
  replace(is.na(.), 0) %>%
  mutate(women = (`Female` - `Male`) / (`Male` + `Female`) * 100) %>%
  left_join(JS, by = "state_district") %>%
  select(state_district, dem_margin, women)

write_rds(genballot, "new_shiny/women.rds")

landline <- upshot %>%
  filter(senate == FALSE, gov == FALSE, wave == 3) %>%
  group_by(state_district) %>%
  count(phone_type) %>%
  spread(phone_type, n) %>%
  replace(is.na(.), 0) %>%
  mutate(landline = (`Landline`) / (`Landline` + `Cell`) * 100) %>%
  left_join(JS, by = "state_district") %>%
  select(state_district, dem_margin, landline)

write_rds(landline, "new_shiny/landline.rds")