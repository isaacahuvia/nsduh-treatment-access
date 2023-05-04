## Extra big plot
library(scales)

perceived_need <- svyby(~perceived_need, by = ~ age+race, design = design_perceived_need, FUN = svymean, na.rm = T)
treatment_access <- svyby(~treatment_access, by = ~ age+race, design = design_treatment_access, FUN = svymean, na.rm = T)
unmet_need <- svyby(~unmet_need, by = ~ age+race, design = design_unmet_need, FUN = svymean, na.rm = T)

combined <- bind_rows(lst(perceived_need, treatment_access, unmet_need)) %>%
  pivot_longer(-c(age, race)) %>%
  filter(grepl("TRUE", name),
         !is.na(value)) %>%
  mutate(variable = gsub("se.|TRUE", "", name),
         statistic = if_else(grepl("se.", name), "se", "mean")) %>%
  pivot_wider(id_cols = c(age, race, variable),
              values_from = value,
              names_from = statistic)

combined %>%
  rowwise() %>%
  mutate(lower = max(mean - (se * 1.96), 0),
         upper = min(mean + (se * 1.96), 1),
         variable = recode(
           variable,
           "perceived_need" = "Perceived Need",
           "treatment_access" = "Treatment Access",
           "unmet_need" = "Unmet Need"
         ),
         race = recode(
           race, 
           "White non-Hispanic" = "White\nnon-Hispanic",
           "Black or African American non-Hispanic" = "Black or\nAfrican American\nnon-Hispanic",
           "Native American or Alaska Native non-Hispanic" = "Native American\nor Alaska Native\nnon-Hispanic",
           "Native Hawaiian or Other Pacific Islander non-Hispanic" = "Native Hawaiian or\nOther Pacific Islander\nnon-Hispanic",
           "Asian non-Hispanic" = "Asian\nnon-Hispanic",
           "More than one race non-Hispanic" = "More than one\nrace non-Hispanic"
         )) %>%
  ungroup() %>%
  ggplot(aes(x = age, y = mean)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  scale_y_continuous(name = "Mean", labels = percent_format()) +
  scale_x_continuous(name = "Age", breaks = 1:5, labels = c("18-25", "26-34", "35-49", "50-64", "65+")) +
  facet_grid(variable ~ race, switch = "y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(here("Full-Page Outcome Plot.png"),
       width = 11,
       height = 6,
       units = "in",
       dpi = 300)
