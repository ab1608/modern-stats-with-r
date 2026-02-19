library(openintro)
library(tidyverse)

# 1: Box plot and jitterplots
possum <- possum |> mutate(tail_cut = cut(tail_l, breaks = 5))

ggplot(data = possum, aes(y = total_l, x = tail_cut)) + geom_point()

ggplot(data = possum, aes(y = total_l, x = tail_cut)) +
  geom_boxplot(outlier.alpha = 0) +
  geom_jitter(color = "skyblue", width = 0.2)

# 2. Applying transformations to coordinates

ggplot(data = mammals, aes(x = body_wt, y = brain_wt)) +
  geom_point()

ggplot(data = mammals, aes(x = body_wt, y = brain_wt)) +
  geom_point() +
  coord_trans(x = "log10", y = "log10")

# 3. Identifying outliers
ggplot(data = mlbbat10, aes(x = stolen_base, y = home_run)) +
  geom_point(alpha = 0.5)
ggplot(data = mlbbat10, aes(x = stolen_base, y = home_run)) +
  geom_jitter(alpha = 0.5)
