library("openintro")
library("tidyverse")

# Randomizing Opportunity Cost
# The study:
# 75 students were assigned to the control group
# and were presented with two options.
# Each student could either buy the DVD or not buy the DVD.
# Another 75 students were assigned to the treatment group
# and were given a slight variation on the same two options.
# The first option was also to buy the DVD
# but the second option was to not buy the DVD
# while being reminded that the money could also be saved.

# Tabulate the data
opportunity_cost |>
  count(decision, group)

# Find the proportion who bought the DVD in each group
opportunity_cost |>
  group_by(group) |>
  summarize(buy_prop = mean(decision == "buy video"))
# Plot group, filled by decision
ggplot(opportunity_cost, aes(x = group, fill = decision)) +
  # Add a bar layer, with position "fill"
  geom_bar(position = "fill")


# Calculate the observed difference in purchase rate
diff_obs <- opportunity_cost |>
  # Group by group
  group_by(group) |>
  # Calculate proportion deciding to buy a DVD
  summarize(prop_buy = mean(decision == "buy video")) |>
  # Calculate difference between groups
  summarize(stat = diff(prop_buy)) |>
  pull() # -0.2

# Create data frame of permuted differences in purchase rates
opp_perm <- opportunity_cost |>
  # Specify decision vs. group, where success is buying a DVD
  specify(decision ~ group, success = "buy video") |>
  # Set the null hypothesis to independence
  hypothesize(null = "independence") |>
  # Generate 1000 reps of type permute
  generate(reps = 1000, type = "permute") |>
  # Calculate the summary stat difference in proportions
  calculate(stat = "diff in props", order = c("treatment", "control"))


# Using the permuation data, plot stat
ggplot(opp_perm, aes(x = stat)) +
  # Add a histogram layer with binwidth 0.005
  geom_histogram(binwidth = 0.005) +
  # Add a vline layer with intercept diff_obs
  geom_vline(aes(xintercept = diff_obs), color = "red")

# Calculate p-value: the proportion of permuted differences less than or equal to the observed difference.
# In this case, we are doing a one-tailed test
# Calculate the p-value using `summarize`
opp_perm |>
  summarize(p_value = mean(stat <= diff_obs))
# p-value = 0.01 which is less than signficance level of 0.05
# The small p-value indicates that the observed data are inconsistent with the null hypothesis.
# We should reject the null claim and conclude that financial advice does affect the likelihood of purchase.
# We can confidently say the different messaging caused the students to change their buying habits
#  since they were randomly assigned to treatment and control groupls

# Causation
# Because the p-value is substantially less than 0.05
# we conclude that it was not simply random variability that
# led to fewer students buying the DVD when being reminded to save.
# Because the study was randomized, that is, the individuals were randomly
#  assigned the choices, there was nothing systematically different about
# the participants in the treatment and control groups.
# The only difference in the two groups was the set of options they received.
# Therefore, any difference in DVD buying rates is due to the option of being reminded to save.
# A causal inference can be made in this setting.
# Importantly, however, the 150 individuals in the sample were not randomly sampled from all people.
# Indeed, they were said to be students. Students are certainly different from the adult population in many ways.
# In order to generalize to a larger population
#  we would need more information about the students and who they represented.
