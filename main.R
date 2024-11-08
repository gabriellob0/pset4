library(dplyr)
library(broom)

nlsy <- haven::read_dta("~/University/LSE/Papers/Labour Economics/pset4/ec423ps4/nlsy_2014_data.dta")

names(nlsy)

# 3.a
length(nlsy$hrp1)
sum(ifelse(nlsy$hrp1 > 1, 1, 0)) # some ppl dont work or dont disclose pay

length(nlsy$hrp2)
sum(ifelse(nlsy$hrp2 > 1, 1, 0)) # probably fewer ppl work a second job


# 3.b
nlsy |>
  select(hrp1) |>
  filter(hrp1 >= 0) |>
  summary() # values look high

plot(density(filter(nlsy, hrp1 > 0)$hrp1))

#TODO: check the unit


# 3.c
plot(density(log(filter(nlsy, hrp1 > 0)$hrp1)))
hist(log(filter(nlsy, hrp1 > 0)$hrp1))

#TODO: create ln_hr_wage


# 4.a
nlsy |>
  select(sampweight) |>
  summary()

regression_data <- nlsy |>
  filter(hrp1 > 0) |>
  mutate(
    ln_hr_wage = log(hrp1),
    sampweight = sampweight / 100
  )

sampweight_summary <- regression_data |>
  select(id, sampweight) |>
  group_by(id) |>
  summarise(
    mean = mean(sampweight),
    count = n(),
    min = min(sampweight), max = max(sampweight)
  )
