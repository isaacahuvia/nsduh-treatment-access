#############################
##  Analysis - Unmet Need  ##
#############################

####  Startup  ####
## Load necessary packages
library(tidyverse)
library(survey)
library(stats)
library(here)
source(here("0_Custom Functions.R"))


## Load data
data_unmet_need <- readRDS(here("Data", "Clean Data.rds")) %>%
  filter(perceived_need) %>%
  drop_na(unmet_need)


## Set survey design, necessary for svyglm (see ?svydesign)
design_unmet_need <- svydesign(ids = ~ 1,
                                     weights = ~ ANALWT_C,
                                     data = data_unmet_need)



####  Analysis  ####
## Perceived need by race alone
# Visual inspection
svyby(~ unmet_need, by = ~ race, design = design_unmet_need, FUN = svymean, na.rm = T) %>%
  ggplot(aes(x = race, y = unmet_needTRUE, fill = race)) +
  geom_col() +
  geom_errorbar(aes(ymin = unmet_needTRUE - (se.unmet_needTRUE * 1.96),
                    ymax = unmet_needTRUE + (se.unmet_needTRUE * 1.96)))

# Logistic regression
unmet_need_by_race <- svyglm(formula = unmet_need ~ race,
                                   family = "quasibinomial",
                                   design = design_unmet_need)
custom_summary(unmet_need_by_race)


## Perceived need by age alone
# Visual inspection
svyby(~ unmet_need, by = ~ age, design = design_unmet_need, FUN = svymean, na.rm = T) %>%
  ggplot(aes(x = age, y = unmet_needTRUE, fill = age)) +
  geom_line() +
  geom_errorbar(aes(ymin = unmet_needTRUE - (se.unmet_needTRUE * 1.96),
                    ymax = unmet_needTRUE + (se.unmet_needTRUE * 1.96)))

# Logistic regression
unmet_need_by_age <- svyglm(formula = unmet_need ~ age,
                                  family = "quasibinomial",
                                  design = design_unmet_need)
custom_summary(unmet_need_by_age)


## Perceived need by race and age
# Visual inspection
svyby(~unmet_need, by = ~ age+race, design = design_unmet_need, FUN = svymean, na.rm = T) %>%
  ggplot(aes(x = age, y = unmet_needTRUE, color = race)) +
  geom_line() +
  geom_errorbar(aes(ymin = unmet_needTRUE - (se.unmet_needTRUE * 1.96),
                    ymax = unmet_needTRUE + (se.unmet_needTRUE * 1.96))) +
  facet_wrap(~ race)

# Logistic regression without interaction
unmet_need_by_race_and_age <- svyglm(formula = unmet_need ~ race + age,
                                           family = "quasibinomial",
                                           design = design_unmet_need)
custom_summary(unmet_need_by_race_and_age)

# Logistic regression with interaction
unmet_need_by_race_by_age <- svyglm(formula = unmet_need ~ race + age + race*age,
                                          family = "quasibinomial",
                                          design = design_unmet_need)
custom_summary(unmet_need_by_race_by_age) %>% print(n = 99)
