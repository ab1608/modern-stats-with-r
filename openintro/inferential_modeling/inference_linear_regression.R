library("tidyverse")
library("openintro")
library("ggplot2")

#########################################################################
###  Performing a Hypothesis test that weight and age are independent ###
##########################################################################
# H_0: the slope = 0
# H_A: the slope != 0

# 1. Calculate the observed slope (p_0)
clean_data <- births14 %>% filter(!is.na(weight), !is.na(mage))
obs_model <- lm(weight ~ mage, data = clean_data)
obs_slope <- coef(obs_model)[2]

# 2. Permutate the data
set.seed(42)
n_reps <- 10000
null_slopes <- numeric(n_reps)

for (i in 1:n_reps) {
  # Shuffle response: Weight
  shuffled_weights <- sample(clean_data$weight, replace = FALSE)
  # Get simulated slope
  model_i <- lm(shuffled_weights ~ clean_data$mage)
  null_slopes[i] <- coef(model_i)[2]
}

# 3. Visualize
null_slopes <- data.frame(null_values = null_slopes)
null_slopes |>
  ggplot(aes(x = null_values)) +
  geom_histogram(binwidth = 0.0005) +
  geom_vline(xintercept = obs_slope)

# 4. Calculate the P-value
# The proportion of null slopes that are at least as extreme than our observed slops
# for a two sided p-test
p_value <- mean(abs(null_slopes) >= abs(obs_slope))

cat("Observed Slope:", round(obs_slope, 4), "\n")
cat("P-value:", p_value)

#################################################################
### Using Bootstrapping to find Confidence Interval for Slope ###
##################################################################
# 1. Calculate the Point Estimate (Sample slope)
clean_data <- births14 %>%
  filter(!is.na(weight), !is.na(mage))
weights <- clean_data$weight
mages <- clean_data$mage
sample_length <- nrow(clean_data)
sample_model <- lm(weights ~ mages)
sample_slope <- coef(sample_model)[2]
sample_length <- length(weights)


# 2. Bootstrap multiple slopes
n_reps = 10000
boot_slopes <- numeric(n_reps)

for (i in 1:n_reps) {
  # Sample indices to keep pairs the same
  indices <- sample(1:sample_length, size = sample_length, replace = TRUE)
  weights_i <- weights[indices]
  mages_i <- mages[indices]

  # Store simulated slope
  model_i <- lm(weights_i ~ mages_i)
  boot_slopes[i] <- coef(model_i)[2]
}


# 3. Calculate Standard Error (SE) to build SE Interval
se_boot <- sd(boot_slopes)
z_star <- 1.96
lower <- sample_slope - (z_star * se_boot)
upper <- sample_slope + (z_star * se_boot)

# Alternative: Percentile Interval
pct_int <- quantile(boot_slopes, probs = c(0.025, 0.975))

# 4. Results
cat("Point Estimate (Slope):", round(sample_slope, 4), "\n")
cat("Standard Error:", round(se_boot, 4), "\n")
cat("95% with SE Interval:", round(lower, 4), "to", round(upper, 4), "\n")
cat("95% CI with Percentile Inteval:", pct_int)

# 5. Visualize
slopes_df <- data.frame(value = boot_slopes)
slopes_df |>
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = 0.0005) +
  geom_vline(xintercept = sample_slope) +
  geom_vline(xintercept = pct_int[1]) +
  geom_vline(xintercept = pct_int[2])
# We see that the 95% confidence interval does contain the sample slope in this case
