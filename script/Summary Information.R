data_path <- file.path(
  getwd(),
  "data",
  "COVID-19_Case_Surveillance_Public_Use_Data.csv"
)
COVID <- read.csv(data_path)
library(tidyverse)
library(dplyr)

# Organizing data
covid_hosp <- COVID %>%
  filter(hosp_yn != "Unknown") %>%
  filter(hosp_yn != "Missing") %>%
  select(Race.and.ethnicity..combined., hosp_yn)

hosp_by_race_y <- covid_hosp %>%
  filter(Race.and.ethnicity..combined. != "Unknown") %>%
  group_by(Race.and.ethnicity..combined.) %>%
  filter(hosp_yn == "Yes") %>%
  summarize(sum_y = n())

hosp_by_race_sum <- covid_hosp %>%
  filter(Race.and.ethnicity..combined. != "Unknown") %>%
  group_by(Race.and.ethnicity..combined.) %>%
  summarize(sum = n())

hosp_race_rate <- left_join(hosp_by_race_y, hosp_by_race_sum,
                            by = "Race.and.ethnicity..combined."
) %>%
  mutate(y_rate = sum_y / sum) %>%
  select(Race.and.ethnicity..combined., y_rate)

basic <- COVID %>%
  select(current_status, age_group, sex)

confirmed_case <- basic %>%
  filter(current_status == "Laboratory-confirmed case") %>%
  filter(sex != "Missing") %>%
  filter(sex != "NA") %>%
  filter(sex != "Unknown") %>%
  filter(age_group != "Unknown") %>%
  filter(age_group != "NA")

age_cases <- confirmed_case %>%
  group_by(age_group) %>%
  summarise(cases = n()) %>%
  mutate(percent = cases / nrow(confirmed_case))

# Summary List
summary_info <- list()

#The highest hospital treatment rate among different race & ethnicity groups
summary_info$hosp_y_max <- hosp_race_rate %>%
  filter(y_rate == max(y_rate)) %>%
  pull(y_rate)

##The lowest hospital treatment rate among different race & ethnicity groups
summary_info$hosp_y_min <- hosp_race_rate %>%
  filter(y_rate == min(y_rate)) %>%
  pull(y_rate)

##The overall hospital treatment rate among all patients
summary_info$hosp_y <- covid_hosp %>%
  filter(hosp_yn == "Yes") %>%
  nrow() / nrow(covid_hosp)

##The most cases are found in this age group
summary_info$most_age <- age_cases %>%
  filter(percent == max(percent)) %>%
  pull(age_group)

##The most confirmed age group takes up this percentage
summary_info$most_percent <- age_cases %>%
  filter(percent == max(percent)) %>%
  pull(percent)

##The most confirmed age group takes up this percentage
summary_info$least_age <- age_cases %>%
  filter(percent == min(percent)) %>%
  pull(age_group)

##The least confirmed age group takes up this percentage
summary_info$least_percent <- age_cases %>%
  filter(percent == min(percent)) %>%
  pull(percent)

