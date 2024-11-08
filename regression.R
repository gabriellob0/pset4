library(dplyr)
library(sandwich)

nlsy <- haven::read_dta("ec423ps4/nlsy_2014_data.dta")

regression_data <- nlsy |>
  mutate(
    hrp1 = if_else(hrp1 >= 0, hrp1, NA),
    log_hrp1 = log(hrp1 + 1),
    sampweight = sampweight / 100,
    q3_4 = if_else(q3_4 >= 0, q3_4, NA),
    yschl = if_else(q3_4 == 95, 0, q3_4),
    age = 114 - dob_year,
    age_sqd = age^2,
    mom_schl = if_else(hgc_mother >= 0, hgc_mother, NA),
    pop_schl = if_else(hgc_father >= 0, hgc_father, NA),
    female = sex - 1,
    afqt = if_else(afqt_3 >= 0, afqt_3 / 1000, NA),
    health_problems = q11_5a
  ) |>
  select(
    hrp1, log_hrp1, sampweight, yschl, age, age_sqd,
    mom_schl, pop_schl, female, afqt, health_problems
  )

base_model <- lm(log_hrp1 ~ yschl, regression_data, weights = sampweight)
summary(base_model)

controls_model <- lm(
  log_hrp1 ~ yschl + female + age + age_sqd + mom_schl + pop_schl,
  regression_data, weights = sampweight
)
summary(controls_model)

ability_model <- lm(
  log_hrp1 ~ yschl + female + age + age_sqd + afqt,
  regression_data, weights = sampweight
)
summary(ability_model)

health_model <- lm(
  log_hrp1 ~ yschl + female + age + age_sqd + afqt + health_problems,
  regression_data, weights = sampweight
)
summary(health_model)

school_dummy_model <- lm(
  log_hrp1 ~ factor(yschl) + female + age + age_sqd + afqt,
  regression_data, weights = sampweight
)
summary(school_dummy_model)


postgrad_model <- lm(
  log_hrp1 ~ factor(yschl) + female + age + age_sqd + afqt,
  filter(regression_data, yschl >= 8), weights = sampweight
)
summary(postgrad_model)
