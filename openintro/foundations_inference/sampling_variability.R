library("NHANES")
library("tidyverse")

names(NHANES)

NHANES |>
  ggplot(aes(x = Gender, fill = HomeOwn)) +
  geom_bar(position = "fill") +
  ylab("Relative frequencies")
