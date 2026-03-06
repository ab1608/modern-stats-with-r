library("openintro")
library("tidyverse")

# These bootstrap conidence interval methods work for any statistic and parameter,
# as long as the following technical conditions hold:
# (1) the distribution of the statistic is reasonably symmetric and bell-shaped
# (2) the sample size is reasonably large,
# (3) the sample was representative of the population.

# From previous exercises
one_poll <- all_polls |>
  filter(poll == 1) |>
  select(vote)

one_poll_boot <- one_poll |>
  specify(response = vote, success = "yes") |>
  generate(reps = 1000, type = "bootstrap") |>
  calculate(stat = "prop")

p_hat <- one_poll |>
  # Calculate proportion of yes votes
  summarize(stat = mean(vote == "yes")) |>
  # Pull out as numeric vector
  pull()

# Create an interval of plausible values
one_poll_boot |>
  summarize(
    # Lower bound is p_hat minus 2 std errors
    lower = p_hat - 2 * sd(stat),
    # Upper bound is p_hat plus 2 std errors
    upper = p_hat + 2 * sd(stat)
  )

# Manually calculate a 95% percentile interval
# This is a good method to use when the data does not follow a
# Normal distribution
one_poll_boot |>
  summarize(
    lower = quantile(stat, p = 0.025),
    upper = quantile(stat, p = 0.975)
  )

# Because we don’t know whether the sample is close to the population or far from it,
# we don’t know whether the confidence interval actually captures the true parameter.
# To that end, we interpret the interval using a confidence percentage.
# That is, we say we are 95% confident that the true parameter is bewtween [upper, lower] range

##### Example produced by Gemini  #####
library(openintro)
data(births14)

# Response: weight
# Clean data: extract weights and remove missing values
weights <- na.omit(births14$weight)

# 1. Calculate the Point Estimate (Sample Mean)
x_bar <- mean(weights)

# 2. The Bootstrap Loop
set.seed(42)
n_reps <- 1000
boot_means <- numeric(n_reps) # Pre-allocate a vector for speed

for (i in 1:n_reps) {
  # Resample WITH replacement, same size as original
  sample_i <- sample(weights, size = length(weights), replace = TRUE)
  # Store the mean of this specific resample
  boot_means[i] <- mean(sample_i)
}

# 3. Calculate Standard Error (SE)
# The SE is the standard deviation of our bootstrap distribution
se_boot <- sd(boot_means)

# 4. Calculate the Confidence Interval
z_star <- 1.96
lower <- x_bar - (z_star * se_boot)
upper <- x_bar + (z_star * se_boot)

# Results
cat("Point Estimate (Mean):", round(x_bar, 3), "\n")
cat("Standard Error:", round(se_boot, 4), "\n")
cat("95% CI:", round(lower, 3), "to", round(upper, 3))
