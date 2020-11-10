covid <- read.csv("data/COVID-19_Case_Surveillance_Public_Use_Data.csv")
colnames(covid)
library(tidyverse)
library(dplyr)
library(ggplot2)

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

hosp_y <- covid_hosp %>%
  filter(hosp_yn == "Yes") %>%
  nrow() / nrow(covid_hosp)

race_hosp_chart <- ggplot(hosp_race_rate) +
  geom_col(
    mapping = aes(x = Race.and.ethnicity..combined., y = y_rate),
    fill = "#58B2DC"
  ) +
  coord_flip() +
  geom_hline(yintercept = hosp_y, color = "#656765") +
  ggtitle("COVID-19 Percentage Treated By Race in the U.S.") +
  labs(
    y = "Proportion treated by hospital", x = "Race & Ethnicity", caption =
      "Grey vertical line indicates overall hospital treatment rate"
  )


# Style check
library(lintr)
library(styler)
lint("Chart_race_hosp.R")
style_file("Chart_race_hosp.R")
