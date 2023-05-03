#################################
##  Analysis - Perceived Need  ##
#################################

####  Startup  ####
## Load necessary packages
library(tidyverse)
library(survey)
library(stats)
library(here)
source(here("0_Custom Functions.R"))


## Load data
data_perceived_need <- readRDS(here("Data", "Clean Data.rds")) %>%
  drop_na(perceived_need)


## Set survey design, necessary for svyglm (see ?svydesign)
design_perceived_need <- svydesign(ids = ~ 1,
                                   weights = ~ ANALWT_C,
                                   data = data_perceived_need)



####  Analysis  ####
## Perceived need by race alone
# Visual inspection
svyby(~ perceived_need, by = ~ race, design = design_perceived_need, FUN = svymean, na.rm = T) %>%
  ggplot(aes(x = race, y = perceived_needTRUE, fill = race)) +
  geom_col() +
  geom_errorbar(aes(ymin = perceived_needTRUE - (se.perceived_needTRUE * 1.96),
                    ymax = perceived_needTRUE + (se.perceived_needTRUE * 1.96)))

# Logistic regression
perceived_need_by_race <- svyglm(formula = perceived_need ~ race,
                                 family = "quasibinomial",
                                 design = design_perceived_need)
custom_summary(perceived_need_by_race)


## Perceived need by age alone
# Visual inspection
svyby(~ perceived_need, by = ~ age, design = design_perceived_need, FUN = svymean, na.rm = T) %>%
  ggplot(aes(x = age, y = perceived_needTRUE, fill = age)) +
  geom_line() +
  geom_errorbar(aes(ymin = perceived_needTRUE - (se.perceived_needTRUE * 1.96),
                    ymax = perceived_needTRUE + (se.perceived_needTRUE * 1.96)))

# Logistic regression
perceived_need_by_age <- svyglm(formula = perceived_need ~ age + age_squared,
                                family = "quasibinomial",
                                design = design_perceived_need)
custom_summary(perceived_need_by_age)


## Perceived need by race and age
# Visual inspection
svyby(~perceived_need, by = ~ age+race, design = design_perceived_need, FUN = svymean, na.rm = T) %>%
  ggplot(aes(x = age, y = perceived_needTRUE, color = race)) +
  geom_line() +
  geom_errorbar(aes(ymin = perceived_needTRUE - (se.perceived_needTRUE * 1.96),
                    ymax = perceived_needTRUE + (se.perceived_needTRUE * 1.96))) +
  facet_wrap(~ race)

# Logistic regression without interaction
perceived_need_by_race_and_age <- svyglm(formula = perceived_need ~ race + age + age_squared,
                                         family = "quasibinomial",
                                         design = design_perceived_need)
custom_summary(perceived_need_by_race_and_age)

# Logistic regression with interaction
perceived_need_by_race_by_age <- svyglm(formula = perceived_need ~ race + age + age_squared + race*age + race*age_squared,
                                        family = "quasibinomial",
                                        design = design_perceived_need)
custom_summary(perceived_need_by_race_by_age) %>% print(n = 99)


## Perceived need by race and age - Asian non-Hispanic vs White non-Hispanic
data_White_Asian <- data_perceived_need %>%
  filter(race %in% c("White non-Hispanic", "Asian non-Hispanic"))

design_White_Asian <- svydesign(ids = ~1,
                                weights = ~ANALWT_C,
                                data = data_White_Asian)

svyby(~perceived_need, by = ~ age+race, design = design_White_Asian, FUN = svymean, na.rm = T) %>%
  ggplot(aes(x = age, y = perceived_needTRUE, color = race)) +
  geom_line(alpha = .5) +
  geom_errorbar(aes(ymin = perceived_needTRUE - (se.perceived_needTRUE * 1.96),
                    ymax = perceived_needTRUE + (se.perceived_needTRUE * 1.96))) +
  geom_smooth(data = data_White_Asian,
              aes(x = age, y = as.numeric(perceived_need), color = race),
              method = "glm",
              method.args = list(family = "binomial"),
              formula = y ~ x + I(x^2),
              size = 2,
              se = F)
