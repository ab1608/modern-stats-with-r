# %%
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns

# Load a classic dataset
penguins = sns.load_dataset("penguins")

# Set a colorblind-friendly palette
sns.set_theme(style="whitegrid", palette="colorblind")


# %%
# ─────────────────────────────────────────────────────────────────────────────
# 1. BAR CHART — frequency of a categorical variable
#    R: ggplot(df, aes(x=species)) + geom_bar()
# ─────────────────────────────────────────────────────────────────────────────
# Frequency plot
fig, ax = plt.subplots(1, 2, figsize=(12, 5))
sns.countplot(data=penguins, x="species", ax=ax[0])
ax[0].set_title("Frequency of Species")

# Frequency by sex
sns.countplot(data=penguins, x="species", hue="sex", ax=ax[1])
ax[1].set_title("Species by Sex")


# %%
# ─────────────────────────────────────────────────────────────────────────────
# 2. STACKED BAR — proportion by group  (position="fill")
#    R: ggplot(df, aes(x=species, fill=sex)) + geom_bar(position="fill")
#
#    Note: seaborn has no native stacked-proportion bar, so we keep the
#    pandas normalisation step — but it's still much shorter than raw matplotlib.
# ─────────────────────────────────────────────────────────────────────────────
# Create a proportion table
prop = pd.crosstab(penguins["species"], penguins["sex"], normalize="index")
# Plotting
fig, ax = plt.subplots(figsize=(6, 4))
prop.plot(
    kind="bar",
    stacked=True,
    color=sns.color_palette("colorblind"),
    ax=ax,
    edgecolor="white",
    width=0.6,
)

ax.set(
    title="Proportion of Sex by Species",
    xlabel="Species",
    ylabel="Proportion",
)
ax.set_xticklabels(ax.get_xticklabels(), rotation=0, ha="center")  # Rotate x-ticks
sns.move_legend(ax, title="Sex", loc="center left", bbox_to_anchor=(1.02, 0.5))
fig.tight_layout()
plt.show()

# %%
# ─────────────────────────────────────────────────────────────────────────────
# 3. SCATTER PLOT
#    R: ggplot(df, aes(x=bill_length_mm, y=flipper_length_mm, color=species))
#         + geom_point(alpha=0.6)
# ─────────────────────────────────────────────────────────────────────────────
fig, ax = plt.subplots(figsize=(6, 5))
sns.scatterplot(
    data=penguins,
    x="bill_length_mm",
    y="bill_depth_mm",
    hue="species",
    style="species",
    ax=ax,
)
ax.set(
    title="Bill Length vs. Depth",
    xlabel="Bill Length (mm)",
    ylabel="Bill Depth (mm)",
)
plt.xticks(rotation=0, ha="center")
sns.move_legend(ax, "upper left", bbox_to_anchor=(1.02, 1), title="Species")
fig.tight_layout()
plt.show()

# %%
# ─────────────────────────────────────────────────────────────────────────────
# 4. LINE GRAPH — time × numeric
#    R: ggplot(annual, aes(x=year, y=mean_mass, color=species)) + geom_line()
# ─────────────────────────────────────────────────────────────────────────────
# Using a subset of gapminder-like data
# Note: Usually you'd use df.plot() or sns.lineplot(x='year', y='val')
df_taxis = sns.load_dataset("taxis").dropna()
fig, ax = plt.subplots(figsize=(12, 4))
sns.lineplot(
    data=df_taxis,
    x="pickup",
    y="total",
    marker="o",
    ax=ax,
)
ax.set(
    title="Total Fare by Pickup Time",
    xlabel="Pickup Time",
    ylabel="Total Fare",
)
fig.tight_layout()
plt.show()
plt.close()

# %%
# ─────────────────────────────────────────────────────────────────────────────
# 5. HISTOGRAM
#    R: ggplot(df, aes(x=body_mass_g, fill=species)) + geom_histogram(bins=30)
# ─────────────────────────────────────────────────────────────────────────────
fig, ax = plt.subplots(figsize=(6, 4))
sns.histplot(
    data=penguins,
    x="body_mass_g",
    hue="species",
    bins=30,
    alpha=0.6,
    ax=ax,
    label="Species",
)
ax.set(
    title="Distribution of Body Mass by Species",
    xlabel="Body Mass (g)",
    ylabel="Count",
)
sns.move_legend(ax, "upper left", bbox_to_anchor=(1.02, 1), title="Species")
fig.tight_layout()
plt.show()

# %%
# ─────────────────────────────────────────────────────────────────────────────
# 6. DENSITY  (geom_density equivalent)
#    R: ggplot(df, aes(x=body_mass_g, fill=species)) + geom_density(alpha=0.4)
# ─────────────────────────────────────────────────────────────────────────────
# 1. Create the objects
fig, ax = plt.subplots(figsize=(6, 4))

# 2. Plotting (Explicitly passing the ax)
sns.kdeplot(data=penguins, x="body_mass_g", hue="species", fill=True, alpha=0.4, ax=ax)

# 3. Use ax.set() to handle multiple labels at once
# This is the "clean" way to do what plt.title/xlabel/ylabel do
ax.set(
    title="Density of Body Mass by Species (geom_density)",
    xlabel="Body Mass (g)",
    ylabel="Density",
)

# 4. Legend & Layout
sns.move_legend(ax, "upper left", bbox_to_anchor=(1.02, 1), title="Species")
# tight_layout is a Figure method, so we call it on 'fig'
fig.tight_layout()

plt.show()


# %%
# ─────────────────────────────────────────────────────────────────────────────
# 7. FACET WRAP — scatter faceted by island
#    R: ggplot(df, aes(x=bill_length_mm, y=flipper_length_mm, color=species))
#         + geom_point(alpha=0.6) + facet_wrap(~island)
#
#    FacetGrid is seaborn's direct equivalent of facet_wrap.
# ─────────────────────────────────────────────────────────────────────────────
# FacetWrap equivalent: Splitting by Island
g = sns.FacetGrid(
    penguins,
    col="island",
    hue="species",
    height=4,
    aspect=1.1,
    sharex=True,
    sharey=True,
)
g.map(sns.scatterplot, "bill_length_mm", "flipper_length_mm", alpha=0.75)
g.add_legend(title="Species")
g.set_axis_labels("Bill Length (mm)", "Flipper Length (mm)")
g.figure.suptitle(
    "Bill vs. Flipper Length — facet_wrap(~island)", y=1.03, fontweight="bold"
)
plt.show()
