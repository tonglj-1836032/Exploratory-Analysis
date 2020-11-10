covid <- read.csv("data/COVID-19_Case_Surveillance_Public_Use_Data.csv")

# Organizing data
covid_hosp <- covid %>%
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

# Summary List
summary_info <- list()

summary_info$hosp_y_max <- hosp_race_rate %>%
  filter(y_rate == max(y_rate)) %>%
  pull(y_rate)

summary_info$hosp_y_min <- hosp_race_rate %>%
  filter(y_rate == min(y_rate)) %>%
  pull(y_rate)

summary_info$hosp_y <- covid_hosp %>%
  filter(hosp_yn == "Yes") %>%
  nrow() / nrow(covid_hosp)




# Style check
lint("Summary Information.R")
style_file("Summary Information.R")