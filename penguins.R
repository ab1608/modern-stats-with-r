library(tidyverse)
library(palmerpenguins)

glimpse(penguins)


#########################
# SCATTER PLOTS
#########################

# Creating a scatter plot that also plots a linear regression model
# for each species

# here, the color is a global arg in aes()
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point() +
  geom_smooth(method = "lm")

# Creating a scatter plot that plots a linear regression model
# for the entire dataset
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  # here, the color is a local arg
  # each species is also assigned a different shape
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap and Gentoo Penguins",
    x = "Flipper length (mm)",
    y = "Body mass (g)",
    color = "Species",
    shape = "Species",
    caption = "Data come from the palmerpenguins package"
  )

# Bill Depth (mm) vs Bill Length (mm)
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(mapping = aes(color = species), na.rm = TRUE)

# Body mass vs flipper length (using the |> operator)
penguins |>
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(mapping = aes(color = bill_depth_mm)) +
  geom_smooth(method = "auto") +
  # scale_color_viridis_b() # Legend as Binned colors
  scale_color_viridis_c() # Legend as Continuous color

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
) +
  geom_point() +
  geom_smooth(se = FALSE, method = 'auto')


ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point() +
  geom_smooth()

ggplot() +
  geom_point(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  ) +
  geom_smooth(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  )

#########################
# BAR CHARTS
#########################

# fct_infreq() convergts the 'species' variable
# into an unordered factor (categorical) data type
# which let's us display the bars in descending order
penguins |> ggplot(aes(x = fct_infreq(species))) + geom_bar() # Vertical bar
penguins |> ggplot(aes(y = fct_infreq(species))) + geom_bar() # Horizontal bar

# Plotting the count of each penguin per island
penguins |> ggplot(aes(x = island, fill = species)) + geom_bar()

# Plotting the proportion of each penguin per island
penguins |>
  ggplot(aes(x = island, fill = species)) +
  geom_bar(position = "fill") +
  labs(x = "Island", y = "Proportion")

#########################
# BOXPLOT
#########################

penguins |> ggplot(aes(x = species, y = body_mass_g)) + geom_boxplot()

ggplot(penguins, aes(x = body_mass_g, color = species, fill = species)) +
  geom_density(linewidth = 0.75, alpha = 0.5)

#########################
# --- USING FACETS (SMALL MULTIPLES) ---
#########################
penguins |>
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  # for facet, the syntax is `row ~ column` though if only
  # one variable is passed, it is simply `~variable`
  facet_wrap(~island, ncol = 1)

#########################
# ------ EXERCISES ------
#########################

mpg |>
  ggplot(aes(hwy, displ)) +
  geom_point()

penguins |>
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = species, shape = species)) +
  labs(color = "Species", shape = "Species")
