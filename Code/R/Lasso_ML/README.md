# Ghana DHS 2022 — Double‑Selection LASSO and Poverty

**Last updated:** 2026‑03‑25  
**Author:** Arthur Martin

---

## What’s going on here 
This bit of the repo looks at how a specific household constraint — in this run, **not having electricity** — is related to the **chance that a household is poor** using the 2022 Ghana DHS household data.  

Instead of using machine learning just to predict poverty, the whole point here is *causal inference*. To do that properly with lots of potential controls, I use a **double‑selection LASSO** approach. 
In practice, that means I let LASSO help decide which controls matter, but I’m careful about *how* it’s used so I don’t accidentally throw away important confounders.
The final treatment effect is then estimated with a plain, unpenalised regression using DHS weights and clustered standard errors.

---

## Why double selection instead of “just LASSO”?

A single LASSO run is great for prediction, but it’s risky for causal questions.

Here’s the intuition:

- A **naive LASSO** only cares about predicting the outcome (poverty).  
  If a variable is strongly correlated with the treatment (say, remoteness or infrastructure) but doesn’t add much extra predictive power *once the treatment is in the model*, LASSO will happily drop it.
- That’s a problem, because those dropped variables are often **exactly the things that create omitted‑variable bias**.

Double selection fixes this in a very simple way:

- I run LASSO once to see what predicts **poverty**.
- I run LASSO again to see what predicts **electricity access**. ( this is what came out in the niave lasso I ran as a the best determinant of poverty from file) 
- I keep the **union** of both sets of variables.
- Only then do I estimate the treatment effect, without any penalty.

The idea is: if a variable matters for *either* the outcome *or* the treatment, it gets kept. That makes the final estimate much more robust to selection mistakes.

---

## Data and key definitions

- **Data:** Ghana DHS 2022, Household Recode (`GHHR8CFL.DTA`).
- **Outcome:**  
  `poor = 1` if the household is in the bottom 40% of the wealth distribution (`hv270 <= 2`), and `0` otherwise.
- **Weights and IDs:**  
  - Sampling weights: `hv005`, rescaled to `wt = hv005 / 1e6`  
  - PSU (cluster): `hv001`  
  - Household ID: `hv002`
- **Treatment (example used here):**  
  `D = 1` if the household reports **no electricity** (`hv206`), `0` otherwise.
- **Candidate controls:**  
  A fairly rich set of geography, household composition, and asset / service variables (things like region, urban/rural status, household size, water, sanitation, cooking fuel, housing materials, asset ownership, etc.).  
  Importantly, the variable used to define the treatment itself is **excluded** from the control set.
- **Pre‑processing:**  
  Labelled variables are converted to factors, missing values are imputed (median for numeric, mode for categorical), categorical variables are one‑hot encoded, and any zero‑variance columns are dropped.

---

## Estimation recipe (what the code actually does)

1. Build a big design matrix `X` with all candidate controls, plus:
   - `y` = poverty indicator  
   - `D` = treatment indicator  
   - `wt` = DHS weights
2. **LASSO #1 (outcome equation):**  
   Regress `y` on `X` using LASSO (with cross‑validation).  
   Keep the variables with non‑zero coefficients → call this set `S_y`.
3. **LASSO #2 (treatment equation):**  
   Regress `D` on `X` using LASSO.  
   Keep the variables with non‑zero coefficients → call this set `S_d`.
4. Take the **union** of both sets:  
   `S = S_y ∪ S_d`.
5. **Final stage:**  
   Estimate  
   `y ~ D + X_S`  
   using an unpenalised linear regression (linear probability model), weighted by `wt`, with standard errors clustered at the PSU level.
6. Report:
   - the treatment coefficient (in percentage points),
   - its standard error,
   - and the 95% confidence interval,
   along with which variables were selected in each step.

The key point is that LASSO is only used for **selection**, not for estimating the treatment effect itself.

---

## Main result from the example run

- **Treatment:** no electricity.
- **Estimator:** post‑double‑selection, weighted OLS with PSU‑clustered standard errors.
- **Estimate:**  
  `α = 0.2308`  
  with a 95% CI of `[0.193, 0.269]`.

**Interpretation:**  
Households **without electricity** are around **23 percentage points more likely to be poor** than observably similar households, holding the selected controls constant. The estimate is very precise and economically large.

---

## How to think about this result

- This is a **conditional mean effect**: it compares households that look similar along a wide set of observed dimensions.
- The double‑selection step means the result is **robust to mistakes in variable selection** when dealing with lots of controls.
- It does **not** magically solve problems from unobserved confounders or reverse causality — it’s still “selection on observables” — but it’s much safer than a naive LASSO or hand‑picked controls.

---

## Files written by the script

- `ghana2022_hr_double_selection_electricity.rds`  
  This stores everything needed to reproduce the result: the preprocessing recipe, both LASSO fits, the selected variable sets, the final regression, and the variance–covariance matrix.

(Anything saved under `Outputs/` is deliberately ignored by git.)

---

## How to extend this

The same setup can be reused for other constraints:

- unsafe water  
- unimproved sanitation  
- dirty cooking fuel  
- rural vs urban location  

For each one, redefine `D`, drop the source variable from the control list, and rerun the same double‑selection pipeline. From there,
look at heterogeneity (urban/rural splits, female‑headed households, etc.).

---

## Mental checklist for writing this up

- Be clear that the outcome is **probability of poverty** (percentage points).
- Emphasise that controls are chosen using **double selection**, not ad hoc.
- Note that the final regression is **unpenalised** and properly weighted.
- Be explicit about what the method protects against (selection mistakes) and what it doesn’t (unobserved confounding).

That’s it — this section is basically a bridge from “ML for prediction” to “ML‑assisted causal inference, done carefully”.
