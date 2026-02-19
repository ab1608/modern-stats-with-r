library(openintro)
library(tidyverse)
library(broom)

# Simple linear regression
ggplot(data = possum, aes(y = total_l, x = tail_l)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(data = bdims, aes(x = hgt, y = wgt)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# # View summary of model
# summary(wgt_hgt_mod)

# # Compute the mean of the residuals
# mean(residuals(wgt_hgt_mod))

# # Compute RMSE
# sqrt(sum(residuals(wgt_hgt_mod)^2) / df.residual(wgt_hgt_mod))

lm(formula = poverty_2010 ~ hs_grad_2010, data = county_complete) |>
  summary()

# Baseball dataset
regulars <- mlbbat10 |>
  filter(at_bat > 400)

ggplot(data = regulars, aes(x = stolen_base, y = home_run)) +
  geom_point() +
  geom_smooth(method = "lm", se = 0)

###########################
# --- MODEL FIT --- #
###########################

# Obtaining "leverage" which is a measure of how far
# a point x_i is from x_mean
mod_hr_sb <- lm(home_run ~ stolen_base, data = regulars)

mod_hr_sb |>
  augment() |>
  select(home_run, stolen_base, .hat) |>
  slice_max(order_by = .hat, n = 5)

# We can obtained Cook's distance which is quantity
# that combines the leverage and residual to measure
# *influence* on the regression line.

# Points with high influence will
# pull the slope of the regression line higher
mod_hr_sb |>
  augment() |>
  select(home_run, stolen_base, .fitted, .resid, .hat, .cooksd) |>
  slice_max(order_by = .cooksd, n = 5)


# Create nontrivial_players
nontrivial_players <- mlbbat10 |>
  filter(at_bat >= 10 & obp < 0.500)

# Visualize new model
ggplot(data = nontrivial_players, mapping = aes(x = obp, y = at_bat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Visualize old model
ggplot(data = mlbbat10, aes(x = obp, y = at_bat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Fit a model to new data
mod_cleaner <- lm(at_bat ~ obp, data = nontrivial_players)

# Fit a model to the original data
mod_original <- lm(at_bat ~ obp, data = mlbbat10)

# View the new model's summary
summary(mod_cleaner)
print("--------")
# View the original model's summary
summary(mod_original)

###########################
# --- PARALLEL SLOPES --- #
###########################
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point()

ggplot(data = mpg, aes(x = factor(year), y = hwy)) +
  geom_boxplot()

# Multiple linear regression
mod_hwy <- lm(hwy ~ displ + factor(year), data = mpg)
