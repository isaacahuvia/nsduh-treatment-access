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
data_perceived_need <- readRDS(here("Clean Data.rds")) %>%
  drop_na(perceived_need)


## Create (weighted) mean-centered square term
mean_age <- weighted.mean(data_perceived_need$age, data_perceived_need$ANALWT_C)
data_perceived_need$age_squared <- (data_perceived_need$age - mean_age)^2


## Set survey design, necessary for svyglm (see ?svydesign)
design_perceived_need <- svydesign(ids = ~ 1,
                                   weights = ~ ANALWT_C,
                                   data = data_perceived_need)



####  Analysis  ####
## Perceived need for the population
svymean(~ perceived_need, design = design_perceived_need) %>%
  as.data.frame() %>%
  mutate(lower = mean - (SE * 1.96),
         upper = mean + (SE * 1.96)) %>%
  round(3)

## Perceived need by race alone
# Table
svyby(~ perceived_need, by = ~ race, design = design_perceived_need, FUN = svymean, na.rm = T) %>%
  mutate(est = paste0(
    round(perceived_needTRUE, 2), " (",
    round(perceived_needTRUE - (se.perceived_needTRUE * 1.96), 2), ", ",
    round(perceived_needTRUE + (se.perceived_needTRUE * 1.96), 2), ")")
  )%>%
  select(est)

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
# Table
svyby(~perceived_need, by = ~ age, design = design_perceived_need, FUN = svymean, na.rm = T) %>%
  mutate(est = paste0(
    round(perceived_needTRUE, 2), " (",
    round(perceived_needTRUE - (se.perceived_needTRUE * 1.96), 2), ", ",
    round(perceived_needTRUE + (se.perceived_needTRUE * 1.96), 2), ")")
  )%>%
  select(est)

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
# Table
svyby(~perceived_need, by = ~ age + race, design = design_perceived_need, FUN = svymean, na.rm = T) %>%
  mutate(est = paste0(
    round(perceived_needTRUE, 2), " (",
    round(perceived_needTRUE - (se.perceived_needTRUE * 1.96), 2), ", ",
    round(perceived_needTRUE + (se.perceived_needTRUE * 1.96), 2), ")")
  )%>%
  pivot_wider(id_cols = race,
              names_from = age,
              values_from = est) %>%
  View()

# Visual inspection
svyby(~perceived_need, by = ~ age + race, design = design_perceived_need, FUN = svymean, na.rm = T) %>%
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
custom_summary(perceived_need_by_race_by_age) %>% 
  print(n = 99)

# Model comparison
regTermTest(perceived_need_by_race_by_age, ~ race:age + race:age_squared, method = "Wald")
anova(perceived_need_by_race_and_age, perceived_need_by_race_by_age, method = "LRT")
