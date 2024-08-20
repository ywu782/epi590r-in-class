library(tidyverse)
library(gtsummary)

nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
               "id", "nsibs", "samp", "race_eth", "sex", "region",
               "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
                 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
                 skip = 1, col_names = nlsy_cols) |>
  mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
         sex_cat = factor(sex, labels = c("Male", "Female")),
         race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
         eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
         glasses_cat = factor(glasses, labels = c("No", "Yes")))


tbl_summary(
  nlsy,
  by = sex_cat,
  include = c(region_cat, race_eth_cat, 
              income, starts_with("sleep")),
  label = list(
    region_cat ~ "Region",
    race_eth_cat ~ "Race/ethnicity",
    income ~ "Income",
    sleep_wkdy ~ "Hours of sleep during weekdays",
    sleep_wknd ~ "Hours of sleep during Weekends"
  ),
  statistic = list(
    income ~ "{p10},{p90}",
    starts_with("sleep") ~ "min = {min}; max = {max}"), 
  digits = list(
    income ~ c(3, 3),
    starts_with("sleep") ~ c(1, 1)
  ),
  missing_text = "Missing") |>
  add_overall() |>
  add_p() |>
  bold_labels() |>
  modify_table_styling(
    columns = label,
    rows = label == "Race/ethnicity",
    footnote = "https://www.nlsinfo.org/content/cohorts/nlsy79/topical-guide/household/race-ethnicity-immigration-data"
  )



