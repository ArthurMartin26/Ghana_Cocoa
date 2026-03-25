library(haven)
library(dplyr)
library(tidyr)
library(recipes)
library(glmnet)
library(janitor)
library(labelled)
library(Matrix)
library(readr)
library(sandwich)
library(lmtest)
library(stringr)

# --------------------- Load data ---------------------
hr_path <- "Data/Data_Raw/DHS/GH_2022_DHS_Standard/GHHR8CFL.DTA"
hr <- read_dta(hr_path)

# --------------------- IDs, weights, and outcome ---------------------
id_vars <- c("hv001","hv002")   # PSU & HH IDs
poverty_quintile_cutoff <- 2    # bottom 40% as "poor"
hr <- hr %>%
  filter(!is.na(hv270)) %>%
  mutate(
    poor = as.integer(hv270 <= poverty_quintile_cutoff),
    wt   = as.numeric(hv005) / 1e6
  )

# --------------------- TREATMENT ---------------------
# effect of NO ELECTRICITY on poverty
# hv206 is labelled, convert to factor and define D = 1(no electricity)
hr <- hr %>%
  mutate(hv206_f = haven::as_factor(hv206),
         D_treat = as.integer(hv206_f %in% c("no", "No", "not available")))

# checks
stopifnot(all(hr$D_treat %in% c(0,1,NA)))
hr <- hr %>% filter(!is.na(D_treat), !is.na(poor), !is.na(wt))

# --------------------- Candidate controls ---------------------
# Start from previous list, but EXCLUDE the treatment source (hv206)
cand <- c(
  "hv024","hv025","hv009","hv012","hv013","hv014","hv018","hv019",
  "hv201","hv202","hv204","hv205",         
  "hv207","hv208","hv209","hv210","hv211","hv212",
  "hv213","hv214","hv215","hv216","hv217","hv218","hv219","hv220","hv221","hv222","hv223",
  "hv226","hv230","hv231","hv232","hv243","hv244","hv245","hv246","hv247"
)
x_vars <- intersect(cand, names(hr))

# --------------------- Preprocess for double selection ---------------------
# Key differences from prediction recipe:
# - DO NOT standardize (glmnet can standardize internally)
# - DO NOT include the treatment in controls
# - make dummies from factors/labelled vars
rec_ds <- recipe(~ ., data = hr %>% select(all_of(c(id_vars, "poor","wt","D_treat", x_vars)))) %>%
  update_role(all_of(id_vars), new_role = "id") %>%
  update_role(wt, new_role = "case_weight") %>%
  # Convert labelled -> factor BEFORE dummying
  step_mutate(across(all_of(x_vars), ~ .x)) %>%
  step_mutate(across(where(is.labelled), ~ haven::as_factor(.x))) %>%
  # Impute missing controls (not outcome/treatment)
  step_impute_median(all_numeric_predictors(), -D_treat) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  # Create dummies for controls
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  # Drop zero variance
  step_zv(all_predictors())

prep_ds <- prep(rec_ds, training = hr, verbose = FALSE)

# Bake to dense dataframe (controls already numeric/dummies); then to sparse
baked <- bake(prep_ds, new_data = hr, composition = "data.frame")

# Construct design pieces
y <- baked$poor
w <- baked$wt
cluster_psu <- hr$hv001  # for clustering
D <- baked$D_treat

# X_controls: drop outcome, weight, treatment, and IDs
X_df <- baked %>%
  select(-poor, -wt, -D_treat, -any_of(id_vars))

# Sparse matrix for glmnet
X <- Matrix::Matrix(as.matrix(X_df), sparse = TRUE)
colnames_X <- colnames(X)

# --------------------- Double Selection: LASSO 1 (Outcome) ---------------------
set.seed(2026)
cv_y <- cv.glmnet(
  x = X, y = y,
  family = "gaussian",    
  alpha = 1,
  weights = w,
  nfolds = 10,
  standardize = TRUE
)
lambda_y <- cv_y$lambda.1se
coef_y   <- coef(cv_y, s = lambda_y)
S_y <- setdiff(rownames(coef_y)[which(coef_y[,1] != 0)], "(Intercept)")

# --------------------- Double Selection: LASSO 2 (Treatment) -------------------
set.seed(2026)
cv_d <- cv.glmnet(
  x = X, y = D,
  family = "binomial",       
  alpha = 1,
  weights = w,
  nfolds = 10,
  standardize = TRUE
)
lambda_d <- cv_d$lambda.1se
coef_d   <- coef(cv_d, s = lambda_d)
S_d <- setdiff(rownames(coef_d)[which(coef_d[,1] != 0)], "(Intercept)")

# --------------------- Union of selected controls -----------------------------
S_union <- union(S_y, S_d)
message(sprintf("Selected controls: y=%d, d=%d, union=%d",
                length(S_y), length(S_d), length(S_union)))


X_sel <- if (length(S_union) > 0) as.matrix(X[, match(S_union, colnames_X), drop = FALSE]) else NULL

# Build final estimation frame: y - D + X_sel, weighted OLS (LPM)
df_final <- if (is.null(X_sel)) {
  data.frame(y = y, D = D, wt = w, cluster = cluster_psu)
} else {
  cbind.data.frame(y = y, D = D, X_sel, wt = w, cluster = cluster_psu)
}

# Column names: ensure 'D' and no spaces
names(df_final) <- make.names(names(df_final))

# --------------------- Final OLS & cluster-robust SE (PSU) --------------------
fml <- as.formula(paste0("y ~ D", if (is.null(X_sel)) "" else " + . - wt - cluster"))
fit <- lm(fml, data = df_final, weights = wt)

# Cluster-robust vcov at PSU (hv001), HC1
Vcl <- sandwich::vcovCL(fit, cluster = ~ cluster, type = "HC1")
ct  <- lmtest::coeftest(fit, vcov = Vcl)

alpha_hat <- ct["D","Estimate"]
se_hat    <- ct["D","Std. Error"]
ci_lower  <- alpha_hat + qnorm(0.025) * se_hat
ci_upper  <- alpha_hat + qnorm(0.975) * se_hat

cat(sprintf("\nDouble-Selection (LPM) — Effect of NO ELECTRICITY on poverty\n"))
cat(sprintf("alpha_hat = %.4f (SE = %.4f), 95%% CI [%.4f, %.4f]\n", alpha_hat, se_hat, ci_lower, ci_upper))
cat(sprintf("Interpretation: households without electricity are %.1f pp more/less likely to be poor (holding selected controls constant).\n",
            100*alpha_hat))

# --------------------- Diagnostics: what was selected -------------------------
sel_table <- tibble::tibble(
  control = S_union,
  selected_in = case_when(
    control %in% S_y & control %in% S_d ~ "both",
    control %in% S_y ~ "outcome_only",
    control %in% S_d ~ "treatment_only",
    TRUE ~ "?"
  )
) %>% arrange(selected_in, control)

print(head(sel_table, 30))

# --------------------- Save objects ---------------------
saveRDS(list(
  prep_ds = prep_ds,
  cv_y = cv_y, lambda_y = lambda_y, S_y = S_y,
  cv_d = cv_d, lambda_d = lambda_d, S_d = S_d,
  S_union = S_union,
  fit = fit, vcov_cluster = Vcl,
  alpha_hat = alpha_hat, se_hat = se_hat,
  ci = c(ci_lower, ci_upper)
), file = "ghana2022_hr_double_selection_electricity.rds")