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


tbl_uvregression(
  nlsy,
  x = sex_cat,
  include = c(nsibs, income,
              starts_with('sleep')),
  method = lm)

tbl_uvregression(
  nlsy,
  y = nsibs,
  include = c(sex_cat, race_eth_cat, region_cat),
  method = glm,
  method.args = list(family = poisson()),
  exponentiate = TRUE)

tbl_uvregression(
  nlsy,
  y = glasses,
  include = c(sex_cat, eyesight_cat),
  method = glm,
  method.args = list(family = binomial(link = 'log')),
  exponentiate = TRUE,
  conf.int = TRUE)









