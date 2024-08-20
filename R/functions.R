# start out with a number to test
x <- 3
# you'll want your function to return this number
x^2

square <- function(x) {
	multiply <- x * x
	return(multiply)
}
# test it out
square(x)
square(53)
53^2 # does this match?

raise <- function(x, power) {
	pow <- x ^ power
	return(pow)
}

# test with
raise(x = 2, power = 4)
# should give you
2^4

raise_default <- function(x, power = 2) {
	pow <- x ^ power
	return(pow)
}

# test
raise_default(5)
# should give you
5^2


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


logistic_model <- glm(glasses ~ eyesight_cat + sex_cat,
											data = nlsy, family = binomial()
)
poisson_model <- glm(nsibs ~ eyesight_cat + sex_cat,
										 data = nlsy, family = poisson()
)
logbinomial_model <- glm(glasses ~ eyesight_cat + sex_cat,
												 data = nlsy, family = binomial(link = "log")
)

tbl_regression(
	poisson_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight",
	)
)

new_table_function <- function(model) {
	tbl_regression(
		model,
		exponentiate = TRUE,
		label = list(
			sex_cat ~ "Sex",
			eyesight_cat ~ "Eyesight"
		)
	)
}

new_table_function(logistic_model)
new_table_function(poisson_model)
new_table_function(logbinomial_model)


new_table_function_2 <- function(model) {
	tbl_regression(
		model,
		exponentiate = TRUE,
		tidy_fun = partial(tidy_robust, vcov = "HC1"),
		label = list(
			sex_cat ~ "Sex",
			eyesight_cat ~ "Eyesight"
		)
	)
}

new_table_function_2(logistic_model)
new_table_function_2(poisson_model)
new_table_function_2(logbinomial_model)
