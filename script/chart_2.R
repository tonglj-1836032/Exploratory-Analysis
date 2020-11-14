## Chart 3
## Created by Tonglu Ji
## Percentage of confirmed cases by age
library(tidyverse)
library(ggplot2)

data_path <- file.path(
  getwd(),
  "data",
  "COVID-19_Case_Surveillance_Public_Use_Data.csv"
)
COVID <- read.csv(data_path)

basic <- COVID %>%
  select(current_status, age_group, sex)

confirmed_case <- basic %>%
  filter(current_status == "Laboratory-confirmed case") %>%
  filter(sex != "Missing") %>%
  filter(sex != "NA") %>%
  filter(sex != "Unknown") %>%
  filter(age_group != "Unknown") %>%
  filter(age_group != "NA")

all_cases <- confirmed_case %>%
  group_by(age_group, sex) %>%
  summarise(cases = n())

age_plot <- ggplot(data = all_cases) +
  geom_bar(
    mapping = aes(fill = age_group, y = cases, x = sex),
    position = "fill", stat = "identity"
  ) +
  labs(
    x = "Gender", y = "Confirmed Cases Percetage",
    title = "Percentage of Confirmed Cases by Age Group"
  )

age_cases <- confirmed_case %>%
  group_by(age_group) %>%
  summarise(cases = n()) %>%
  mutate(percent = cases / nrow(confirmed_case))

most_age <- age_cases %>%
  filter(percent == max(percent)) %>%
  pull(age_group)

most_percent <- age_cases %>%
  filter(percent == max(percent)) %>%
  pull(percent)

least_age <- age_cases %>%
  filter(percent == min(percent)) %>%
  pull(age_group)

least_percent <- age_cases %>%
  filter(percent == min(percent)) %>%
  pull(percent)

# Style check
library(lintr)
library(styler)
lint("script/chart_2.R")
style_file("script/chart_2.R")
