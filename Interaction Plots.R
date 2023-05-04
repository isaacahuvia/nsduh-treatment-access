## Interaction plots
library(scales)
library(gridExtra)


## Perceived need by race and age - Black non-Hispanic vs White non-Hispanic
data_White_Black <- data_perceived_need %>%
  filter(race %in% c("White non-Hispanic", "Black or African American non-Hispanic"))

design_White_Black <- svydesign(ids = ~1,
                                weights = ~ANALWT_C,
                                data = data_White_Black)

interaction_1 <- svyby(~perceived_need, by = ~ age+race, design = design_White_Black, FUN = svymean, na.rm = T) %>%
  ggplot(aes(x = age, y = perceived_needTRUE, linetype = race)) +
  geom_line(alpha = .5) +
  geom_errorbar(aes(ymin = perceived_needTRUE - (se.perceived_needTRUE * 1.96),
                    ymax = perceived_needTRUE + (se.perceived_needTRUE * 1.96)),
                alpha = .5) +
  geom_smooth(data = data_White_Black,
              aes(x = age, y = as.numeric(perceived_need), linetype = race),
              method = "glm",
              method.args = list(family = "binomial"),
              formula = y ~ x + I(x^2),
              color = "black",
              se = F) +
  scale_y_continuous(name = "Perceive Need", labels = percent_format()) +
  scale_x_continuous(name = "Age", breaks = 1:5, labels = c("18-25", "26-34", "35-49", "50-64", "65+")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.direction = "vertical")


## Perceived need by race and age - Asian non-Hispanic vs White non-Hispanic
data_White_Asian <- data_perceived_need %>%
  filter(race %in% c("White non-Hispanic", "Asian non-Hispanic"))

design_White_Asian <- svydesign(ids = ~1,
                                weights = ~ANALWT_C,
                                data = data_White_Asian)

interaction_2 <- svyby(~perceived_need, by = ~ age+race, design = design_White_Asian, FUN = svymean, na.rm = T) %>%
  ggplot(aes(x = age, y = perceived_needTRUE, linetype = race)) +
  geom_line(alpha = .5) +
  geom_errorbar(aes(ymin = perceived_needTRUE - (se.perceived_needTRUE * 1.96),
                    ymax = perceived_needTRUE + (se.perceived_needTRUE * 1.96)),
                alpha = .5) +
  geom_smooth(data = data_White_Asian,
              aes(x = age, y = as.numeric(perceived_need), linetype = race),
              method = "glm",
              method.args = list(family = "binomial"),
              formula = y ~ x + I(x^2),
              color = "black",
              se = F) +
  scale_y_continuous(name = "Perceive Need", labels = percent_format()) +
  scale_x_continuous(name = "Age", breaks = 1:5, labels = c("18-25", "26-34", "35-49", "50-64", "65+")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.direction = "vertical")


interactions <- grid.arrange(interaction_1, interaction_2, nrow = 1)

ggsave(plot = interactions,
       here("Interaction Plot.png"),
       width = 6,
       height = 4,
       units = "in",
       dpi = 300)
