import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
import statsmodels.api as sm
import statsmodels.formula.api as smf

# -----------------------------
# 1) Simulate manufacturing QC data
# -----------------------------
rng = np.random.default_rng(42)
n_batches = 300

df = pd.DataFrame(
    {
        "line": rng.choice(["A", "B", "C"], size=n_batches, p=[0.35, 0.4, 0.25]),
        "shift": rng.choice(["day", "night"], size=n_batches, p=[0.6, 0.4]),
        "temp_c": rng.normal(22, 2.5, size=n_batches),  # process temperature
        "humidity_pct": rng.normal(45, 8, size=n_batches),  # ambient humidity
        "operator_exp_yrs": rng.uniform(0.5, 15, size=n_batches),  # operator experience
        "units_inspected": rng.integers(800, 2500, size=n_batches),  # exposure
    }
)

# True data-generating process (log-rate per inspected unit)
line_effect = df["line"].map({"A": 0.00, "B": 0.20, "C": -0.15}).to_numpy()
shift_effect = df["shift"].map({"day": 0.00, "night": 0.18}).to_numpy()

eta = (
    -7.2
    + 0.045 * (df["temp_c"] - 22)
    + 0.012 * (df["humidity_pct"] - 45)
    - 0.030 * df["operator_exp_yrs"]
    + line_effect
    + shift_effect
)

# Expected defects = rate * exposure
mu = np.exp(eta) * df["units_inspected"]
df["defects"] = rng.poisson(mu)

# -----------------------------
# 2) Fit Poisson regression
# -----------------------------
model = smf.glm(
    formula="defects ~ temp_c + humidity_pct + operator_exp_yrs + C(line) + C(shift)",
    data=df,
    family=sm.families.Poisson(),
    offset=np.log(df["units_inspected"]),
).fit()

df["pred_defects"] = model.predict(df, offset=np.log(df["units_inspected"]))
df["defects_per_1000"] = 1000 * df["defects"] / df["units_inspected"]
df["pred_per_1000"] = 1000 * df["pred_defects"] / df["units_inspected"]
df["pearson_resid"] = (df["defects"] - df["pred_defects"]) / np.sqrt(df["pred_defects"])

# -----------------------------
# 3) Seaborn visualizations for QC
# -----------------------------
sns.set_theme(style="whitegrid", context="notebook")

# A) Defect rate vs temperature by line
fig1, ax1 = plt.subplots(figsize=(9, 5))
sns.scatterplot(
    data=df,
    x="temp_c",
    y="defects_per_1000",
    hue="line",
    alpha=0.7,
    ax=ax1,
)
sns.lineplot(
    data=df.sort_values("temp_c"),
    x="temp_c",
    y="pred_per_1000",
    hue="line",
    estimator=None,
    legend=False,
    linewidth=2,
    ax=ax1,
)
ax1.set_title("Observed and Predicted Defect Rate by Temperature")
ax1.set_xlabel("Temperature (°C)")
ax1.set_ylabel("Defects per 1,000 units")
plt.tight_layout()

# B) Quality comparison by production line
fig2, ax2 = plt.subplots(figsize=(8, 5))
sns.boxplot(data=df, x="line", y="defects_per_1000", ax=ax2)
ax2.set_title("Defect Rate Distribution by Manufacturing Line")
ax2.set_xlabel("Line")
ax2.set_ylabel("Defects per 1,000 units")
plt.tight_layout()

# C) Residual check (basic model diagnostics)
fig3, ax3 = plt.subplots(figsize=(9, 5))
sns.scatterplot(data=df, x="pred_defects", y="pearson_resid", hue="shift", alpha=0.75, ax=ax3)
ax3.axhline(0, color="black", linestyle="--", linewidth=1)
ax3.set_title("Pearson Residuals vs Predicted Defects")
ax3.set_xlabel("Predicted defects per batch")
ax3.set_ylabel("Pearson residual")
plt.tight_layout()

# Keep useful outputs available in one object
analysis_artifacts = {
    "data": df,
    "model": model,
    "model_summary_text": model.summary().as_text(),
    "overdispersion_ratio": (model.pearson_chi2 / model.df_resid),
}
analysis_artifacts
