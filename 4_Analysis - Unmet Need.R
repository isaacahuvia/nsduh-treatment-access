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
data_unmet_need <- readRDS(here("Clean Data.rds")) %>%
  filter(perceived_need) %>%
  drop_na(unmet_need)


## Set survey design, necessary for svyglm (see ?svydesign)
design_unmet_need <- svydesign(ids = ~ 1,
                               weights = ~ ANALWT_C,
                               data = data_unmet_need)



####  Analysis  ####
## Unmet need for the population
svymean(~ unmet_need, design = design_unmet_need) %>%
  as.data.frame() %>%
  mutate(lower = mean - (SE * 1.96),
         upper = mean + (SE * 1.96)) %>%
  round(3)

## Unmet need by race alone
# Table
svyby(~ unmet_need, by = ~ race, design = design_unmet_need, FUN = svymean, na.rm = T) %>%
  mutate(est = paste0(
    round(unmet_needTRUE, 2), " (",
    round(unmet_needTRUE - (se.unmet_needTRUE * 1.96), 2), ", ",
    round(unmet_needTRUE + (se.unmet_needTRUE * 1.96), 2), ")")
  )%>%
  select(est)

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


## Unmet need by age alone
# Table
svyby(~ unmet_need, by = ~ age, design = design_unmet_need, FUN = svymean, na.rm = T) %>%
  mutate(est = paste0(
    round(unmet_needTRUE, 2), " (",
    round(unmet_needTRUE - (se.unmet_needTRUE * 1.96), 2), ", ",
    round(unmet_needTRUE + (se.unmet_needTRUE * 1.96), 2), ")")
  )%>%
  select(est)

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


## Unmet need by race and age
# Table
svyby(~ unmet_need, by = ~ age + race, design = design_unmet_need, FUN = svymean, na.rm = T) %>%
  mutate(est = paste0(
    round(unmet_needTRUE, 2), " (",
    round(unmet_needTRUE - (se.unmet_needTRUE * 1.96), 2), ", ",
    round(unmet_needTRUE + (se.unmet_needTRUE * 1.96), 2), ")")
  )%>%
  pivot_wider(id_cols = race,
              names_from = age,
              values_from = est) %>%
  View()

# Visual inspection
svyby(~ unmet_need, by = ~ age + race, design = design_unmet_need, FUN = svymean, na.rm = T) %>%
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
custom_summary(unmet_need_by_race_by_age) %>% 
  print(n = 99)
