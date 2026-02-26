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

# Turn a caterogircal variable into a binary numerical value
heart_transplant <- heart_transplant |>
  mutate(is_alive = ifelse(survived == "alive", 1, 0))
data_space <- ggplot(data = heart_transplant, aes(x = age, y = is_alive)) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.5)

# Visualizing logistic regression
# We note that the logistic graph never reaches 0 nor 1
data_space +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(
    method = "glm",
    se = FALSE,
    color = "red",
    method.args = list(family = "binomial")
  )

# We can more clearly see the decline in probability
# with age if we bin the ages and compute probability
# per age group
heart_breaks <- heart_transplant |>
  pull(age) |>
  quantile(probs = 0:7 / 7)

data_binned_space <- data_space +
  stat_summary_bin(
    fun = "mean",
    color = "red",
    geom = "line",
    breaks = heart_breaks
  )

data_binned_space

# The logistic regression model where age is function of is_alive
mod_heart <- glm(is_alive ~ age, data = heart_transplant, family = binomial)
data_binned_space +
  geom_line(
    data = augment(mod_heart, type.predict = "response"),
    aes(y = .fitted),
    color = "blue"
  )

heart_transplant_plus <- mod_heart |>
  augment(type.predict = "response") |>
  mutate(y_hat = .fitted)

heart_transplant_plus

# This probability scale plot is has an intuitive scale,
# it not linear so we cannot say that one unit increase in the explanatory (age)
# is associated with some change in the response (is_alive)
ggplot(heart_transplant_plus, aes(x = age, y = y_hat)) +
  geom_point() +
  geom_line() +
  scale_y_continuous("Probability of being alive", limits = c(0, 1))


# Odds are defined as [y / (1-y)], i.e. probability positive / probability negative case
# The odds-scale plot let's us see that a person who is about 10 years old
# is 3 times more likely to survive than someone approaching 30
heart_transplant_plus <- heart_transplant_plus |>
  mutate(odds_hat = y_hat / (1 - y_hat))
ggplot(heart_transplant_plus, aes(x = age, y = odds_hat)) +
  geom_point() +
  geom_line() +
  scale_y_continuous("Odds of being alive")

# The log-odds scale lets us see logistic regression as a
# line but the scale is difficult to interpret
heart_transplant_plus <- heart_transplant_plus |>
  mutate(log_odds_hat = log(odds_hat))


# The simplest way to make probabilistic predictions
# is to round the predictions
mod_binary <- augment(mod_heart, type.predict = "response") |>
  mutate(alive_hat = round(.fitted))

mod_binary |>
  select(is_alive, age, .fitted, alive_hat)

mod_binary |> select(is_alive, alive_hat) |> table()

###############################
# --- CASE STUDY PRACTICE --- #
# Using the nyc dataset
###############################

# We can see that Price and Decor are correlated
# We see that Price and Food are correlated
# while Price and Decor are not
nyc |> select(-restaurant) |> pairs()

# With slope = 2.939, we can say that
# Each additional point of food quality is associated with
# 2.939 change in price
lm(price ~ food, data = nyc)


# Let's explore how location affects price
# East Side is about $4 more expensive
nyc |>
  group_by(east) |>
  summarize(mean_price = mean(price))

# Let's explore what might be associated with this greater price
# on the East Side
lm(price ~ food + east, data = nyc)
# fit model

library(plotly)
# draw 3D scatterplot
p <- plot_ly(data = nyc, z = ~price, x = ~food, y = ~service, opacity = 0.6) |>
  add_markers()

# draw a plane
p |>
  add_surface(x = ~x, y = ~y, z = ~plane, showscale = FALSE)
