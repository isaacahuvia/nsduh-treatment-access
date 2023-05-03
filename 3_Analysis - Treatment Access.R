###################################
##  Analysis - Treatment Access  ##
###################################

####  Startup  ####
## Load necessary packages
library(tidyverse)
library(survey)
library(stats)
library(here)
source(here("0_Custom Functions.R"))


## Load data
data_treatment_access <- readRDS(here("Data", "Clean Data.rds")) %>%
  filter(perceived_need) %>%
  drop_na(treatment_access)


## Set survey design, necessary for svyglm (see ?svydesign)
design_treatment_access <- svydesign(ids = ~ 1,
                                   weights = ~ ANALWT_C,
                                   data = data_treatment_access)



####  Analysis  ####
## Perceived need by race alone
# Visual inspection
svyby(~ treatment_access, by = ~ race, design = design_treatment_access, FUN = svymean, na.rm = T) %>%
  ggplot(aes(x = race, y = treatment_accessTRUE, fill = race)) +
  geom_col() +
  geom_errorbar(aes(ymin = treatment_accessTRUE - (se.treatment_accessTRUE * 1.96),
                    ymax = treatment_accessTRUE + (se.treatment_accessTRUE * 1.96)))

# Logistic regression
treatment_access_by_race <- svyglm(formula = treatment_access ~ race,
                                 family = "quasibinomial",
                                 design = design_treatment_access)
custom_summary(treatment_access_by_race)


## Perceived need by age alone
# Visual inspection
svyby(~ treatment_access, by = ~ age, design = design_treatment_access, FUN = svymean, na.rm = T) %>%
  ggplot(aes(x = age, y = treatment_accessTRUE, fill = age)) +
  geom_line() +
  geom_errorbar(aes(ymin = treatment_accessTRUE - (se.treatment_accessTRUE * 1.96),
                    ymax = treatment_accessTRUE + (se.treatment_accessTRUE * 1.96)))

# Logistic regression
treatment_access_by_age <- svyglm(formula = treatment_access ~ age,
                                family = "quasibinomial",
                                design = design_treatment_access)
custom_summary(treatment_access_by_age)


## Perceived need by race and age
# Visual inspection
svyby(~treatment_access, by = ~ age+race, design = design_treatment_access, FUN = svymean, na.rm = T) %>%
  ggplot(aes(x = age, y = treatment_accessTRUE, color = race)) +
  geom_line() +
  geom_errorbar(aes(ymin = treatment_accessTRUE - (se.treatment_accessTRUE * 1.96),
                    ymax = treatment_accessTRUE + (se.treatment_accessTRUE * 1.96))) +
  facet_wrap(~ race)

# Logistic regression without interaction
treatment_access_by_race_and_age <- svyglm(formula = treatment_access ~ race + age,
                                         family = "quasibinomial",
                                         design = design_treatment_access)
custom_summary(treatment_access_by_race_and_age)

# Logistic regression with interaction
treatment_access_by_race_by_age <- svyglm(formula = treatment_access ~ race + age + race*age,
                                        family = "quasibinomial",
                                        design = design_treatment_access)
custom_summary(treatment_access_by_race_by_age) %>% print(n = 99)
