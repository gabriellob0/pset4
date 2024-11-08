library(dplyr)
library(tidyr)

nlsy <- haven::read_dta("ec423ps4/nlsy_2014_data.dta")

names(nlsy)


# 3.a
nlsy |>
  select(hrp1, hrp2) |>
  pivot_longer(everything()) |>
  mutate(positive_dummy = if_else(value >= 0, 1, 0)) |>
  group_by(name, positive_dummy) |>
  summarise(count = n())
# some ppl dont work or dont disclose pay
# probably fewer ppl work a second job

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

# 4.a
nlsy |>
  select(sampweight) |>
  summary()

corrected_sample_weights <- nlsy |>
  filter(hrp1 > 0) |>
  mutate(
    ln_hr_wage = log(hrp1),
    sampweight = sampweight / 100
  )

mean(corrected_sample_weights$sampweight)
# it represents the mean 100 number of people represented by each individual

sampweight_summary <- corrected_sample_weights |>
  select(id, sampweight) |>
  group_by(id) |>
  summarise(
    mean = mean(sampweight),
    count = n(),
    min = min(sampweight),
    max = max(sampweight)
  )

plot(sampweight_summary$mean, sampweight_summary$count)
# supposed to represent how many individuals each respondent represents
# there is a lot of variation between groups

# 5.a

corrected_sample_weights |>
  group_by(q3_4) |>
  count() |>
  arrange(desc(n))

# 5.b

schooling <- corrected_sample_weights |>
  filter(q3_4 >= 0) |>
  mutate(
    yschl = if_else(q3_4 == 95, 0, q3_4)
  )

nrow(corrected_sample_weights) - nrow(schooling)

# 6.a
age_data <- schooling |>
  mutate(
    age = 114 - dob_year,
    age_sqd = age^2
  )

range(age_data$age)

# 6.c
parent_schooling <- age_data |>
  filter(hgc_mother >= 0, hgc_father >= 0) |>
  mutate(
    mom_schl = hgc_mother,
    pop_schl = hgc_father
  )

sum(parent_schooling$sampweight * parent_schooling$mom_schl) / sum(parent_schooling$sampweight)
sum(parent_schooling$sampweight * parent_schooling$pop_schl) / sum(parent_schooling$sampweight)

parent_schooling |>
  mutate(wmom = sampweight * mom_schl, wpop = sampweight * pop_schl) |>
  select(sampweight, wmom, wpop) |>
  summarise(sum())


women <- parent_schooling |>
  mutate(female = sex - 1)

women |>
  mutate(wschl = sampweight * yschl) |>
  group_by(female) |>
  summarise(sum(wschl) / sum(sampweight))

# 6.e
weighted_schooling <- women |>
  mutate(
    wschl = sampweight * yschl,
    wfam = sampweight * (mom_schl + pop_schl)
  )

cor(weighted_schooling$wschl, weighted_schooling$wfam)

# 6.f
afqt_data <- weighted_schooling |>
  filter(afqt_3 >= 0) |>
  mutate(afqt = afqt_3 / 1000) |>
  mutate(health_problems = q11_5a)

afqt_data |>
  group_by(health_problems) |>
  summarise(count = n())
