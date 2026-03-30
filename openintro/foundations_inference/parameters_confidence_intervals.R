library("openintro")
library("tidyverse")
library("infer")


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
n_reps <- 5000
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

# 4. Calculate the Confidence Interval by finding
# the 2.5th and 97.5th percentiles of the bootstrap distribution
lower_pct <- quantile(boot_means, 0.025)
upper_pct <- quantile(boot_means, 0.975)

# Results
cat("Point Estimate (Mean):", round(x_bar, 3), "\n")
cat("Standard Error:", round(se_boot, 4), "\n")
cat("95% CI:", round(lower_pct, 3), "to", round(upper_pct, 3))

# Build a data frame for ggplot
boot_df <- data.frame(mean = boot_means) |>
  mutate(in_ci = mean >= lower_pct & mean <= upper_pct)

# Plot
ggplot(boot_df, aes(x = mean, fill = in_ci)) +
  geom_histogram(bins = 60, color = "white", linewidth = 0.2) +

  # CI boundary lines
  geom_vline(
    xintercept = lower_pct,
    color = "#e63946",
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = upper_pct,
    color = "#e63946",
    linewidth = 0.8,
    linetype = "dashed"
  ) +

  # Point estimate line
  geom_vline(
    xintercept = x_bar,
    color = "#2b2d42",
    linewidth = 1,
    linetype = "solid"
  ) +

  # Annotations
  annotate(
    "text",
    x = x_bar,
    y = Inf,
    label = paste("x̄ =", round(x_bar, 3)),
    vjust = 2,
    hjust = -0.15,
    size = 3.5,
    color = "#2b2d42",
    fontface = "bold"
  ) +

  annotate(
    "text",
    x = lower_pct,
    y = Inf,
    label = paste("2.5%\n", round(lower_pct, 3)),
    vjust = 2,
    hjust = 1.1,
    size = 3,
    color = "#e63946"
  ) +

  annotate(
    "text",
    x = upper_pct,
    y = Inf,
    label = paste("97.5%\n", round(upper_pct, 3)),
    vjust = 2,
    hjust = -0.1,
    size = 3,
    color = "#e63946"
  ) +

  scale_fill_manual(
    values = c("TRUE" = "#457b9d", "FALSE" = "#a8dadc"),
    labels = c("TRUE" = "Inside 95% CI", "FALSE" = "Outside 95% CI")
  ) +

  labs(
    title = "Bootstrap Distribution of Sample Means",
    subtitle = paste0(
      "Birth weights (lbs) | n = ",
      length(weights),
      " | 5,000 bootstrap resamples"
    ),
    x = "Bootstrap Sample Mean",
    y = "Count",
    fill = NULL
  ) +

  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "gray40", size = 11),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )
