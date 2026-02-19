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

# add interaction term manually
# interaction is the product of
# displ * year(treated as a categorical var)

#
# Interpreting the coefficients in an interaction model becomes more complicated.
# Note the difference between the fitted coefficients of the parallel slopes model
# with the interaction model. The original slope of -3.61 mpg per liter for all cars
# is now separated into two slopes: -3.77 mpg per liter for older cars,
# and -3.46 (-3.77 + 0.31 = -3.46) mpg per liter for newer cars.
# Thus, fuel economy for the older cars is not only lower overall,
# but it also declines more rapidly as a function of engine size.
# This importantly changes the interpretation of the model.
# It suggests that the greater fuel economy of the 2008 cars is not just related to
# the fact that they had smaller engines,
# on average – a function mainly of consumer choice.
# It suggests that the 2008 cars were also engineered better,
# in that they were able to maintain fuel economy slightly better even with larger engine sizes.

lm(hwy ~ displ + factor(year) + displ:factor(year), data = mpg)

# No interaction between displ and year
lm(hwy ~ displ + factor(year), data = mpg)

###############################
# --- LOGISTIC REGRESSION --- #
###############################
