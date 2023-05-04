####  Startup  ####
## Load necessary packages
library(tidyverse)
library(here)


## Load data
load(here("Raw Data", "NSDUH_2010.RData"))
load(here("Raw Data", "NSDUH_2011.RData"))
load(here("Raw Data", "NSDUH_2012.RData"))
load(here("Raw Data", "NSDUH_2013.RData"))
load(here("Raw Data", "NSDUH_2014.RData"))
load(here("Raw Data", "NSDUH_2015.RData"))
load(here("Raw Data", "NSDUH_2016.RData"))
load(here("Raw Data", "NSDUH_2017.RData"))
load(here("Raw Data", "NSDUH_2018.RData"))
load(here("Raw Data", "NSDUH_2019.RData"))



####  Clean Data  ####
## Combine datasets
combined_data <- map(.x = 
                       list(
                         PUF2010_090718,
                         PUF2011_090718,
                         PUF2012_090718,
                         PUF2013_090718,
                         PUF2014_090718,
                         PUF2015_021518,
                         PUF2016_022818,
                         PUF2017_100918,
                         PUF2018_100819,
                         PUF2019_100920),
                     .f = ~ .x %>%
  select(amdeyr, NEWRACE2, AGE2, AMHTXRC3, AMHTXND2, ANALWT_C)) %>%
  bind_rows(.id = "svy")


## Examine weights
# The person-level sampling weight is ANALWT_C
# For details, see "Sample Weights" https://www.datafiles.samhsa.gov/sites/default/files/field-uploads-protected/studies/NSDUH-2019/NSDUH-2019-datasets/NSDUH-2019-DS0001/NSDUH-2019-DS0001-info/NSDUH-2019-DS0001-info-codebook.pdf
# Since the NSDUH covers "the general civilian population aged 12 and older in the U.S.", 
# each year's weights should sum to like 250-300 million
combined_data %>%
  group_by(svy) %>%
  summarize(population = sum(ANALWT_C))

# Let's look at the distribution of this variable in the combined dataset
summary(combined_data$ANALWT_C) # The average response represents 4,866 members of the overall population


## Clean combined data (Perceived need)
clean_data <- combined_data %>%
  # filtering for clinical depressive symptoms
  filter(amdeyr == 1) %>%
  mutate(
    perceived_need = AMHTXRC3 == 1 | AMHTXND2 == 1,
    treatment_access = AMHTXRC3 == 1,
    unmet_need = AMHTXND2 == 1,
    age = case_when(
      AGE2 %in% c(7, 8, 9, 10, 11, 12) ~ 1,
      AGE2 %in% c(13, 14) ~ 2,
      AGE2 == 15 ~ 3,
      AGE2 == 16 ~ 4,
      AGE2 == 17 ~ 5
    ),
    race = factor(NEWRACE2,
                  levels = c(1, 2, 3, 4, 5, 6, 7),
                  labels = c("White non-Hispanic",
                             "Black or African American non-Hispanic",
                             "Native American or Alaska Native non-Hispanic",
                             "Native Hawaiian or Other Pacific Islander non-Hispanic",
                             "Asian non-Hispanic",
                             "More than one race non-Hispanic",
                             "Hispanic"))) %>%
  select(svy, age, age_squared, race, perceived_need, treatment_access, unmet_need, ANALWT_C)



####  Save data  ####
saveRDS(clean_data, here("Clean Data.rds")) # .rds files are faster than .RData
