# Student: Theodore Kim
# 11.13.2020
# INFO 201 Group BB-2
# TA: Azim Abdul Wahid
# Aggregate_Table
# A table of aggregate information.

library("dplyr")

# Loads a dataset.
dataset <- read.csv("script/data/COVID-19_Case_Surveillance_Public_Use_Data.csv")

# Renames columns.
colnames(dataset)[1] <- "cdc_report_date"
colnames(dataset)[7] <- "race_ethnicity_combined"
colnames(dataset)[8] <- "hospitalized"
colnames(dataset)[10] <- "death"

# Selects relevant columns.
relevant_dataset <- dataset %>%
  select(
    cdc_report_date, 
    current_status, 
    sex, 
    age_group, 
    race_ethnicity_combined, 
    hospitalized, 
    death
    )

# Sorts dataset in the order of date.
relevant_dataset <- arrange(relevant_dataset, cdc_report_date)

# Groups dataset by current_status.
relevant_dataset <- relevant_dataset %>%
  group_by(current_status)

