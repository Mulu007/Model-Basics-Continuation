library(tidyverse)
library(dplyr)
library(modelr)

df <- tribble(
  ~y, ~x1, ~x2,
  4, 2, 5,
  5, 1, 6
)

model_matrix(df, y ~ x1 + x2)

# Categorical Variables
df <- tribble(
  ~ sex, ~ response,
  "male", 1,
  "female", 2,
  "male", 1
)
view(sim2)

model_matrix(df, response ~ sex)

ggplot(sim2) +
  geom_point(aes(x, y))

# Fit a linear model that predicts y using x
mod2 <- lm(y ~ x, data = sim2)

grid <- sim2 %>%
  data_grid(x) %>%
  add_predictions(mod2)

grid

ggplot(sim2, aes(x)) +
  geom_point(aes(y = y)) +
  geom_point(
    data = grid,
    aes(y = pred),
    color= "red",
    size = 4
  )

