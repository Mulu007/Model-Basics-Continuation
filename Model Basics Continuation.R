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

# Continuous and Categorical Data
view(sim3)
ggplot(sim3, aes(x1, y)) +
  geom_point(aes(color = x2))

# Model 1 one slope for all groups
mod1 <- lm(y ~ x1 + x2, data = sim3)
# Model 2 has different slopes per group
mod2 <- lm(y ~ x1 * x2, data = sim3)

grid <- sim3 %>%
  data_grid(x1, x2) %>%
  gather_predictions(mod1, mod2)
grid

ggplot(sim3, aes(x1, y, color = x2)) +
  geom_point() +
  geom_line(data = grid, aes(y = pred)) +
  facet_wrap(~ model)

# Residual analysis
sim3 <- sim3 %>%
  gather_residuals(mod1, mod2)

ggplot(sim3, aes(x1, resid, color = x2)) +
  geom_point() +
  facet_grid(model ~ x2)

anova(mod1, mod2)
# Model 2 is decisively more accurate than model 1
