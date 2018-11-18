library(tidyverse)
library(janitor)
library(dplyr)
library(fs)

download.file(url = "https://github.com/TheUpshot/2018-live-poll-results/archive/master.zip",
              destfile = "upshot.zip",
              mode = "wb")

unzip("upshot.zip")

file_delete("upshot.zip")

file_names <- dir_ls("2018-live-poll-results-master/data/")

upshot <- map_dfr(file_names, read_csv, .id = "source")

upshot <- upshot %>%
  mutate(wave = as.numeric(str_extract(source, "\\d+(?=\\.csv)"))) %>% # Extracts wave number
  mutate(state = str_extract(source, "(?<=elections-poll-).[a-z]")) %>% # Extracts state
  mutate(state_district = str_extract(source, "(?<=elections-poll-).\\w+")) %>% # Extracts state-district combo
  mutate(state = str_to_upper(state)) %>% # Capitalizes state
  mutate(district = as.numeric(str_extract(source, "(?<=elections-poll-.[a-z]).[0-9]"))) %>% # Extracts district number
  mutate(senate = str_detect(source, "(?<=elections-poll-.[a-z])sen")) %>% # Extracts TRUE or FALSE for senate race
  mutate(gov = str_detect(source, "(?<=elections-poll-.[a-z])gov")) # Extracts TRUE or FALSE for gov race

JS <- read.csv("mt_2_results.csv")

JS <- JS %>%
  mutate(state_district = paste(tolower(state), district, sep = "")) %>% # Creates variable to join with upshot
  mutate(dem_votes = as.numeric(str_remove(dem_votes, ",")), rep_votes = as.numeric(str_remove(rep_votes, ",")), other_votes = as.numeric(str_remove(other_votes, ","))) %>%
  replace(is.na(.), 0) %>%
  mutate(dem_margin = (dem_votes - rep_votes) / (dem_votes + rep_votes + other_votes) * 100) # Calculates margin by which Dems won

race <- upshot %>% # Creates percentage of nonwhite voters per district
  filter(senate == FALSE, gov == FALSE, wave == 3) %>%
  group_by(state_district) %>%
  count(file_race) %>%
  spread(file_race, n) %>%
  replace(is.na(.), 0) %>%
  mutate(non_white = (Asian + Black + Hispanic + Other + Unknown) / (Asian + Black + Hispanic + Other + Unknown + White) * 100) %>%
  left_join(JS, by = "state_district") %>%
  select(state_district, dem_margin, non_white)

write_rds(race, "ps_7_shiny/race.rds")

education <- upshot %>% # Creates percentage of those with degree greather than BA per district
  filter(senate == FALSE, gov == FALSE, wave == 3) %>%
  group_by(state_district) %>%
  count(educ) %>%
  spread(educ, n) %>%
  replace(is.na(.), 0) %>%
  mutate(higher_ed = `Graduate or Professional Degree` / (`Bachelors' degree` + `Grade school` + `Graduate or Professional Degree` + `High school` + `Some college or trade school`) * 100) %>%
  left_join(JS, by = "state_district") %>%
  select(state_district, dem_margin, higher_ed)

write_rds(education, "ps_7_shiny/education.rds")

genballot <- upshot %>% # Creates percentage of people that thought Dems would take the house per district
  filter(senate == FALSE, gov == FALSE, wave == 3) %>%
  group_by(state_district) %>%
  count(genballot) %>%
  spread(genballot, n) %>%
  replace(is.na(.), 0) %>%
  mutate(genballot = `Dems. take House` / (`Dems. take House` + `Don't know` + `Reps. keep House`) * 100) %>%
  left_join(JS, by = "state_district") %>%
  select(state_district, dem_margin, genballot)

write_rds(genballot, "ps_7_shiny/genballot.rds")

women <- upshot %>% # Creates number of women per district
  filter(senate == FALSE, gov == FALSE, wave == 3) %>%
  group_by(state_district) %>%
  count(gender_combined) %>%
  spread(gender_combined, n) %>%
  replace(is.na(.), 0) %>%
  mutate(women = (`Female` - `Male`) / (`Male` + `Female`) * 100) %>%
  left_join(JS, by = "state_district") %>%
  select(state_district, dem_margin, women)

write_rds(women, "ps_7_shiny/women.rds")

landline <- upshot %>% # Creates percentage of landline users per district
  filter(senate == FALSE, gov == FALSE, wave == 3) %>%
  group_by(state_district) %>%
  count(phone_type) %>%
  spread(phone_type, n) %>%
  replace(is.na(.), 0) %>%
  mutate(landline = (`Landline`) / (`Landline` + `Cell`) * 100) %>%
  left_join(JS, by = "state_district") %>%
  select(state_district, dem_margin, landline)

write_rds(landline, "ps_7_shiny/landline.rds")

