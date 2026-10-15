library(ggplot2)
library(tidyverse)
library(haven)
library(viridis)

load("/Users/sarat/Desktop/School Doc/Fall 2026/Econometrics/R Studio Data/Lab 5/acs2021_recoded.RData")

acs_subgroup <- acs2021 %>%
  filter(
    AGE >= 25,
    AGE <= 55,
    LABFORCE == 2,
    WKSWORK2 > 4,
    UHRSWORK >= 35,
    Hispanic == 1,
    female == 1,
    (educ_college == 1 | educ_advdeg == 1)
  )

summary(acs_subgroup)

table(acs_subgroup$educ_hs)

model_hs <- lm(
  INCWAGE ~ AGE + I(AGE^2) + educ_hs,
  data = acs_subgroup
)

summary(model_hs)

model_college <- lm(
  INCWAGE ~ AGE + I(AGE^2) + educ_college,
  data = acs_subgroup
)

summary(model_college)

model_both <- lm(
  INCWAGE ~ AGE + I(AGE^2) + educ_college + educ_advdeg,
  data = acs_subgroup
)

summary(model_both)

table(acs_subgroup$female)
table(acs_subgroup$Hispanic)
table(acs_subgroup$educ_hs)
table(acs_subgroup$educ_college)
table(acs_subgroup$educ_advdeg)

model_dummy_poly <- lm(
  INCWAGE ~ female + I(female^2),
  data = acs2021
)

summary(model_dummy_poly)

cps_pos <- subset(
  acs2021,
  !is.na(INCWAGE) & INCWAGE > 0
)

model_interaction <- lm(
  INCWAGE ~ AGE + I(AGE^2) +
    female +
    female:AGE +
    female:I(AGE^2) +
    educ_college,
  data = cps_pos
)

summary(model_interaction)

b <- coef(model_interaction)

male_peak <- -b["AGE"] /
  (2 * b["I(AGE^2)"])

female_peak <- -(b["AGE"] + b["AGE:female"]) /
  (2 * (b["I(AGE^2)"] + b["I(AGE^2):female"]))

male_peak
female_peak

interaction_predictions <- data.frame(
  AGE = c(30, 40, 50, 30, 40, 50),
  female = c(0, 0, 0, 1, 1, 1),
  educ_college = 1
)

interaction_predictions$predicted_wage <- predict(
  model_interaction,
  newdata = interaction_predictions
)

interaction_predictions

interaction_graph <- expand.grid(
  AGE = 18:65,
  female = c(0, 1),
  educ_college = 1
)

interaction_graph$predicted_wage <- predict(
  model_interaction,
  newdata = interaction_graph
)

interaction_graph$Gender <- ifelse(
  interaction_graph$female == 1,
  "Female",
  "Male"
)

ggplot(
  interaction_graph,
  aes(x = AGE, y = predicted_wage, color = Gender)
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Predicted Wage by Age and Gender",
    x = "Age",
    y = "Predicted Wage",
    color = "Gender"
  ) +
  theme_minimal()