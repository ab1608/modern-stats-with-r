library("NHANES")
library("tidyverse")
library("infer")
library("openintro")

##################
#### EXAMPLES ####
##################
# 1. Specify the explanatory and response relationship
# 2. Specify null and alterantive hypothesis
# 3. Generate 1000 permutations (randomization)
# 4. Calculate difference in proportions as (West East)
# 5. Count the number of times the null data is least as extreme as the null hypothesis,
# i.e. these permutated differences are more extreme than the observed difference

# Many Random Permutations
soda_perm <- soda |>
  rep_sample_n(size = nrow(soda), reps = 100) |>
  mutate(drink_perm = sample(drink)) |>
  group_by(replicate, location) |>
  summarize(
    prop_cola_perm = mean(drink_perm == "Cola"),
    prop_cola = mean(drink == "Cola")
  ) |>
  summarize(diff_perm = diff(prop_cola_perm), diff_orig = diff(prop_cola)) # West - East

# In this case, we are doing a one-sided test in which (P_West - P_east < 0)
soda_perm |>
  summarize(
    count = sum(diff_perm <= diff_orig),
    proportion = mean(diff_perm <= diff_orig)
  )

##### Example produced by Gemini  #####

library(openintro)
data(births14)

# 1. Clean data: focus on weight and smoking habit
df <- births14[!is.na(births14$habit) & !is.na(births14$weight), ]

# 2. Calculate Observed Difference
obs_diff <- mean(df$weight[df$habit == "smoker"]) -
  mean(df$weight[df$habit == "nonsmoker"])

# 3. The Permutation Loop
set.seed(42)
n_reps <- 1000
null_dist <- numeric(n_reps)

# Explanatory: habit
# Response: weight
for (i in 1:n_reps) {
  # Shuffle the labels (habit) without replacement
  shuffled_labels <- sample(df$habit, replace = FALSE)

  # Calculate mean difference using shuffled labels
  mean_smoker <- mean(df$weight[shuffled_labels == "smoker"])
  mean_nonsmoker <- mean(df$weight[shuffled_labels == "nonsmoker"])

  null_dist[i] <- mean_smoker - mean_nonsmoker
}

# 4. Calculate the p-value
# Proportion of "fake" differences as extreme or more extreme than observed
p_value <- mean(null_dist <= obs_diff)

# Results
cat("Observed Difference:", round(obs_diff, 4), "lbs\n")
cat("P-value:", p_value)
