# Supervised Learning in Action: Five Techniques

---

## Linear Regression — Predicting Employee Salary from Experience

### Business Problem

An HR team at a mid-sized tech company wants to build a **compensation benchmarking tool**. Given a candidate's years of experience and role seniority, the model should estimate a fair market salary. This enables recruiters to make data-driven offers and flag internal pay inequities.

### Why Linear Regression?

The target variable (salary) is **continuous**, the relationship between experience and compensation is approximately linear, and interpretability is paramount — HR and legal stakeholders need to understand and defend every predicted value. Linear regression delivers a direct, auditable formula with no black-box behavior. Coefficient signs and magnitudes map directly to business intuition (e.g., "each additional year of experience adds $X to base salary").

---

```python
# /// script
# requires-python = ">=3.13"
# dependencies = [
#   "scikit-learn>=1.5",
#   "pandas>=2.2",
#   "numpy>=2.0",
# ]
# ///

"""Salary prediction using Linear Regression.

Business goal: Estimate fair market salary from years of experience
and seniority level for HR compensation benchmarking.
"""

import numpy as np
import pandas as pd
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_absolute_error, r2_score
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler


def generate_salary_data(n_samples: int = 300, seed: int = 42) -> pd.DataFrame:
    """Generate synthetic employee compensation data.

    Args:
        n_samples: Number of records to generate.
        seed: Random seed for reproducibility.

    Returns:
        DataFrame with features and target salary column.
    """
    rng = np.random.default_rng(seed)
    years_experience = rng.uniform(0, 20, n_samples)
    seniority = rng.integers(1, 5, n_samples)  # 1=Junior, 4=Principal
    noise = rng.normal(0, 8_000, n_samples)

    salary = (
        45_000
        + (years_experience * 4_500)
        + (seniority * 15_000)
        + noise
    )

    return pd.DataFrame({
        "years_experience": years_experience,
        "seniority_level": seniority,
        "salary_usd": salary,
    })


def main() -> None:
    """Train and evaluate salary prediction model."""
    df = generate_salary_data()

    X = df[["years_experience", "seniority_level"]]
    y = df["salary_usd"]

    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.2, random_state=42
    )

    scaler = StandardScaler()
    X_train_scaled = scaler.fit_transform(X_train)
    X_test_scaled = scaler.transform(X_test)

    model = LinearRegression()
    model.fit(X_train_scaled, y_train)

    y_pred = model.predict(X_test_scaled)
    mae = mean_absolute_error(y_test, y_pred)
    r2 = r2_score(y_test, y_pred)

    print("=== Linear Regression: Salary Benchmarking ===")
    print(f"R² Score:              {r2:.4f}")
    print(f"Mean Absolute Error:   ${mae:,.0f}")
    print("\nCoefficients (scaled):")
    for feature, coef in zip(X.columns, model.coef_, strict=True):
        print(f"  {feature:<22}: {coef:>10,.2f}")
    print(f"  {'Intercept':<22}: {model.intercept_:>10,.2f}")

    # Predict for a new candidate
    candidate = pd.DataFrame({"years_experience": [7.0], "seniority_level": [3]})
    candidate_scaled = scaler.transform(candidate)
    prediction = model.predict(candidate_scaled)[0]
    print(f"\nPredicted salary (7 yrs exp, seniority 3): ${prediction:,.0f}")


if __name__ == "__main__":
    main()
```

---

## Logistic Regression — Predicting Customer Churn

### Business Problem

A SaaS subscription business wants to identify customers at high risk of cancelling in the next 30 days. With a predicted churn probability for each account, the retention team can prioritize outreach and offer targeted incentives before revenue is lost.

### Why Logistic Regression?

This is a **binary classification** problem (churn vs. retain). Logistic regression outputs a calibrated **probability**, which is exactly what a prioritization queue needs — the team doesn't just need a yes/no, they need to rank accounts by risk level. It is also highly interpretable: each coefficient represents the log-odds contribution of a feature, allowing the team to understand *why* an account is flagged. It serves as an excellent baseline before investing in more complex models.

---

```python
# /// script
# requires-python = ">=3.13"
# dependencies = [
#   "scikit-learn>=1.5",
#   "pandas>=2.2",
#   "numpy>=2.0",
# ]
# ///

"""Customer churn prediction using Logistic Regression.

Business goal: Score each customer account by churn probability
so the retention team can prioritize outreach.
"""

import numpy as np
import pandas as pd
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import (
    classification_report,
    roc_auc_score,
)
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler


def generate_churn_data(n_samples: int = 1_000, seed: int = 42) -> pd.DataFrame:
    """Generate synthetic SaaS customer churn dataset.

    Args:
        n_samples: Number of customer records.
        seed: Random seed for reproducibility.

    Returns:
        DataFrame with behavioral features and churn label.
    """
    rng = np.random.default_rng(seed)

    days_since_login = rng.integers(1, 90, n_samples)
    support_tickets = rng.integers(0, 10, n_samples)
    monthly_spend = rng.uniform(50, 2_000, n_samples)
    contract_months_remaining = rng.integers(0, 24, n_samples)
    num_active_users = rng.integers(1, 50, n_samples)

    log_odds = (
        -2.0
        + (0.04 * days_since_login)
        + (0.3 * support_tickets)
        - (0.001 * monthly_spend)
        - (0.1 * contract_months_remaining)
        - (0.05 * num_active_users)
    )
    prob_churn = 1 / (1 + np.exp(-log_odds))
    churned = rng.binomial(1, prob_churn)

    return pd.DataFrame({
        "days_since_login": days_since_login,
        "support_tickets_30d": support_tickets,
        "monthly_spend_usd": monthly_spend,
        "contract_months_remaining": contract_months_remaining,
        "num_active_users": num_active_users,
        "churned": churned,
    })


def main() -> None:
    """Train and evaluate churn prediction model."""
    df = generate_churn_data()
    feature_cols = [c for c in df.columns if c != "churned"]

    X = df[feature_cols]
    y = df["churned"]

    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.2, stratify=y, random_state=42
    )

    scaler = StandardScaler()
    X_train_scaled = scaler.fit_transform(X_train)
    X_test_scaled = scaler.transform(X_test)

    model = LogisticRegression(class_weight="balanced", max_iter=1_000, random_state=42)
    model.fit(X_train_scaled, y_train)

    y_pred = model.predict(X_test_scaled)
    y_proba = model.predict_proba(X_test_scaled)[:, 1]
    auc = roc_auc_score(y_test, y_proba)

    print("=== Logistic Regression: Churn Prediction ===")
    print(f"ROC-AUC: {auc:.4f}\n")
    print(classification_report(y_test, y_pred, target_names=["Retained", "Churned"]))

    risk_df = X_test.copy()
    risk_df["churn_probability"] = y_proba
    top_risk = risk_df.nlargest(5, "churn_probability")[
        ["days_since_login", "contract_months_remaining", "churn_probability"]
    ]
    print("Top 5 At-Risk Accounts:")
    print(top_risk.to_string(index=False))


if __name__ == "__main__":
    main()
```

---

## 3. K-Nearest Neighbors — Diagnosing Fraudulent Insurance Claims

### Business Problem

An insurance company's claims operations team wants to flag potentially fraudulent claims for manual review before payout. Given claim-level features (claim amount, claimant history, type of incident), the model should classify each claim as likely legitimate or suspicious.

### Why K-Nearest Neighbors?

KNN is effective when the **decision boundary is locally complex but globally non-linear** — fraud patterns often cluster in feature space (e.g., a specific combination of high claim amounts, short policy tenure, and a particular incident type). KNN makes no parametric assumptions about the data distribution, which is valuable when fraud tactics evolve and the underlying patterns shift. It is also intuitive to explain to claims adjusters: "this claim resembles the N most similar past claims that were confirmed fraud." Caveat: KNN scales poorly with dataset size; for very large claim volumes, a tree-based ensemble would be preferred.

---

```python
# /// script
# requires-python = ">=3.13"
# dependencies = [
#   "scikit-learn>=1.5",
#   "pandas>=2.2",
#   "numpy>=2.0",
# ]
# ///

"""Insurance fraud detection using K-Nearest Neighbors.

Business goal: Flag suspicious insurance claims for manual review
before processing payouts, reducing fraudulent disbursements.
"""

import numpy as np
import pandas as pd
from sklearn.metrics import classification_report, roc_auc_score
from sklearn.model_selection import GridSearchCV, train_test_split
from sklearn.neighbors import KNeighborsClassifier
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler


def generate_claims_data(n_samples: int = 800, seed: int = 42) -> pd.DataFrame:
    """Generate synthetic insurance claims dataset.

    Args:
        n_samples: Number of claims to simulate.
        seed: Random seed for reproducibility.

    Returns:
        DataFrame with claim features and fraud label.
    """
    rng = np.random.default_rng(seed)

    claim_amount = rng.uniform(500, 50_000, n_samples)
    policy_tenure_months = rng.integers(1, 120, n_samples)
    prior_claims = rng.integers(0, 5, n_samples)
    injury_severity = rng.integers(1, 5, n_samples)

    fraud_score = (
        (claim_amount > 30_000).astype(int) * 2
        + (policy_tenure_months < 12).astype(int) * 2
        + (prior_claims >= 3).astype(int) * 1
    )
    prob_fraud = np.clip(fraud_score / 5 + rng.uniform(-0.1, 0.1, n_samples), 0, 1)
    is_fraud = rng.binomial(1, prob_fraud)

    return pd.DataFrame({
        "claim_amount_usd": claim_amount,
        "policy_tenure_months": policy_tenure_months,
        "prior_claims": prior_claims,
        "injury_severity": injury_severity,
        "is_fraud": is_fraud,
    })


def main() -> None:
    """Train, tune, and evaluate fraud detection model."""
    df = generate_claims_data()
    feature_cols = [c for c in df.columns if c != "is_fraud"]

    X = df[feature_cols]
    y = df["is_fraud"]

    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.2, stratify=y, random_state=42
    )

    pipeline = Pipeline([
        ("scaler", StandardScaler()),
        ("knn", KNeighborsClassifier()),
    ])

    param_grid = {
        "knn__n_neighbors": [3, 5, 7, 11, 15],
        "knn__weights": ["uniform", "distance"],
    }

    grid_search = GridSearchCV(
        pipeline, param_grid, cv=5, scoring="roc_auc", n_jobs=-1
    )
    grid_search.fit(X_train, y_train)

    best_model = grid_search.best_estimator_
    y_pred = best_model.predict(X_test)
    y_proba = best_model.predict_proba(X_test)[:, 1]
    auc = roc_auc_score(y_test, y_proba)

    print("=== K-Nearest Neighbors: Fraud Detection ===")
    print(f"Best params: {grid_search.best_params_}")
    print(f"ROC-AUC:     {auc:.4f}\n")
    print(classification_report(y_test, y_pred, target_names=["Legitimate", "Fraud"]))


if __name__ == "__main__":
    main()
```

---

## 4. Support Vector Machine — Credit Default Classification

### Business Problem

A retail bank's credit risk team needs a model to assess whether a loan applicant is likely to default within 12 months. The model outputs will gate loan approval decisions, so maximising the separation between creditworthy and high-risk applicants is the core objective.

### Why SVM?

SVMs are designed to find the **maximum-margin decision boundary** between classes — exactly the right objective when the cost of misclassification is asymmetric and you want the most confident separation possible. With an RBF kernel, SVMs handle non-linear feature interactions (e.g., high debt-to-income ratio combined with short credit history) without manually engineering interaction terms. SVMs also perform well in **high-dimensional, moderate-sample** settings typical of credit data where you may have dozens of engineered features but only thousands of applications. Compared to neural networks, SVMs are less prone to overfitting on small datasets.

---

```python
# /// script
# requires-python = ">=3.13"
# dependencies = [
#   "scikit-learn>=1.5",
#   "pandas>=2.2",
#   "numpy>=2.0",
# ]
# ///

"""Credit default prediction using Support Vector Machine.

Business goal: Classify loan applicants as likely to default or
remain in good standing to inform loan approval decisions.
"""

import numpy as np
import pandas as pd
from sklearn.metrics import classification_report, roc_auc_score
from sklearn.model_selection import StratifiedKFold, cross_val_score, train_test_split
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.svm import SVC


def generate_credit_data(n_samples: int = 600, seed: int = 42) -> pd.DataFrame:
    """Generate synthetic credit applicant data.

    Args:
        n_samples: Number of loan applications to simulate.
        seed: Random seed for reproducibility.

    Returns:
        DataFrame with applicant features and default label.
    """
    rng = np.random.default_rng(seed)

    credit_score = rng.integers(300, 850, n_samples)
    debt_to_income = rng.uniform(0.05, 0.80, n_samples)
    months_employed = rng.integers(0, 240, n_samples)
    num_open_accounts = rng.integers(1, 15, n_samples)
    loan_to_value = rng.uniform(0.5, 1.2, n_samples)

    log_odds = (
        3.0
        - (0.006 * credit_score)
        + (3.0 * debt_to_income)
        - (0.01 * months_employed)
        + (0.5 * loan_to_value)
    )
    prob_default = 1 / (1 + np.exp(-log_odds))
    defaulted = rng.binomial(1, prob_default)

    return pd.DataFrame({
        "credit_score": credit_score,
        "debt_to_income_ratio": debt_to_income,
        "months_employed": months_employed,
        "num_open_accounts": num_open_accounts,
        "loan_to_value_ratio": loan_to_value,
        "defaulted": defaulted,
    })


def main() -> None:
    """Train and evaluate credit default SVM classifier."""
    df = generate_credit_data()
    feature_cols = [c for c in df.columns if c != "defaulted"]

    X = df[feature_cols]
    y = df["defaulted"]

    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.2, stratify=y, random_state=42
    )

    pipeline = Pipeline([
        ("scaler", StandardScaler()),
        (
            "svm",
            SVC(
                kernel="rbf",
                C=1.0,
                gamma="scale",
                probability=True,
                class_weight="balanced",
                random_state=42,
            ),
        ),
    ])

    cv = StratifiedKFold(n_splits=5, shuffle=True, random_state=42)
    cv_auc = cross_val_score(pipeline, X_train, y_train, cv=cv, scoring="roc_auc")

    pipeline.fit(X_train, y_train)
    y_pred = pipeline.predict(X_test)
    y_proba = pipeline.predict_proba(X_test)[:, 1]
    test_auc = roc_auc_score(y_test, y_proba)

    print("=== SVM: Credit Default Classification ===")
    print(f"CV ROC-AUC (mean ± std): {cv_auc.mean():.4f} ± {cv_auc.std():.4f}")
    print(f"Test ROC-AUC:            {test_auc:.4f}\n")
    print(classification_report(y_test, y_pred, target_names=["Good Standing", "Default"]))


if __name__ == "__main__":
    main()
```

---

## 5. Decision Tree — Product Category Routing for E-Commerce Returns

### Business Problem

An e-commerce operations team processes thousands of product returns daily. Returns arrive with a text description and product metadata, and they need to be routed to the correct handling team (refurbishment, recycling, restocking, or disposal). Manual routing is slow and inconsistent. A decision tree can learn the routing logic and apply it instantly at scale.

### Why a Decision Tree?

Decision trees are ideal here for two reasons. First, the routing rules are **inherently hierarchical and conditional** ("if category is electronics AND condition is damaged, route to recycling") — exactly the kind of logic a decision tree learns naturally. Second, the operations team needs to **audit and override** the model's decisions; a printed decision tree can be handed to a floor manager who can verify and challenge specific branches. No other model offers this level of direct explainability. Additionally, decision trees require no feature scaling, handle mixed feature types cleanly, and train in milliseconds on this data volume.

---

```python
# /// script
# requires-python = ">=3.13"
# dependencies = [
#   "scikit-learn>=1.5",
#   "pandas>=2.2",
#   "numpy>=2.0",
# ]
# ///

"""Product return routing using Decision Tree Classifier.

Business goal: Automatically route e-commerce returns to the correct
handling team (restocking, refurbishment, recycling, or disposal)
to reduce manual processing time and routing errors.
"""

import numpy as np
import pandas as pd
from sklearn.metrics import classification_report
from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier, export_text

ROUTE_LABELS = ["Restock", "Refurbish", "Recycle", "Dispose"]
CATEGORY_MAP = {"Electronics": 0, "Clothing": 1, "Furniture": 2, "Toys": 3}
CONDITION_MAP = {"New": 0, "Good": 1, "Fair": 2, "Poor": 3, "Damaged": 4}


def generate_returns_data(n_samples: int = 1_000, seed: int = 42) -> pd.DataFrame:
    """Generate synthetic product returns dataset.

    Args:
        n_samples: Number of return records to generate.
        seed: Random seed for reproducibility.

    Returns:
        DataFrame with encoded product features and routing label.
    """
    rng = np.random.default_rng(seed)

    category_codes = rng.integers(0, len(CATEGORY_MAP), n_samples)
    condition_codes = rng.integers(0, len(CONDITION_MAP), n_samples)
    days_since_purchase = rng.integers(1, 365, n_samples)
    original_price = rng.uniform(5, 500, n_samples)

    route = np.where(
        condition_codes <= 1, 0,
        np.where(
            (condition_codes == 2) & (category_codes != 0), 1,
            np.where(condition_codes == 3, 2, 3),
        ),
    )
    noise_mask = rng.random(n_samples) < 0.08
    route[noise_mask] = rng.integers(0, 4, noise_mask.sum())

    return pd.DataFrame({
        "category_code": category_codes,
        "condition_code": condition_codes,
        "days_since_purchase": days_since_purchase,
        "original_price_usd": original_price,
        "route_label": route,
    })


def main() -> None:
    """Train and evaluate return routing decision tree."""
    df = generate_returns_data()
    feature_cols = [c for c in df.columns if c != "route_label"]

    X = df[feature_cols]
    y = df["route_label"]

    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.2, stratify=y, random_state=42
    )

    model = DecisionTreeClassifier(
        max_depth=4,
        min_samples_leaf=10,
        class_weight="balanced",
        random_state=42,
    )
    model.fit(X_train, y_train)

    y_pred = model.predict(X_test)

    print("=== Decision Tree: Returns Routing ===")
    print(classification_report(y_test, y_pred, target_names=ROUTE_LABELS))

    tree_rules = export_text(model, feature_names=feature_cols)
    print("Decision Tree Rules (sharable with operations team):")
    print(tree_rules)


if __name__ == "__main__":
    main()
```

---

## Quick Reference Summary

| Technique | Target Type | Key Strength | Watch Out For |
|---|---|---|---|
| **Linear Regression** | Continuous | Interpretable coefficients | Assumes linearity; sensitive to outliers |
| **Logistic Regression** | Binary / Multiclass | Calibrated probabilities | May underfit with complex interactions |
| **K-Nearest Neighbors** | Any | Non-parametric; no distribution assumptions | Slow at inference; degrades in high dimensions |
| **SVM (RBF kernel)** | Binary / Multiclass | Maximum-margin separation | Hard to interpret; slow to train at large scale |
| **Decision Tree** | Any | Fully transparent, auditable logic | Prone to overfitting; unstable to small data changes |