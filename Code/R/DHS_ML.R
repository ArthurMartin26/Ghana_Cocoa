# --------------------- Setup ---------------------
library(haven)
library(dplyr)
library(tidyr)
library(recipes)
library(rsample)
library(glmnet)
library(yardstick)
library(janitor)
library(labelled)
library(Matrix)
library(readr)

# --------------------- Load data ---------------------
hr_path <- "Data/Data_Raw/DHS/GH_2022_DHS_Standard/GHHR8CFL.DTA"   # <-- EDIT THIS
hr <- read_dta(hr_path)

# Keep IDs for exporting predictions later
id_vars <- c("hv001","hv002")  # cluster & household IDs (DHS standard)

# --------------------- Define target ---------------------
# Wealth quintile: hv270 (1=poorest ... 5=richest)
# We'll classify "poor" as bottom 40% (quintiles 1-2). Change to 1 for bottom 20%.
poverty_quintile_cutoff <- 2
hr <- hr %>%
  filter(!is.na(hv270)) %>%
  mutate(
    poor = as.integer(hv270 <= poverty_quintile_cutoff),
    wt   = as.numeric(hv005) / 1e6
  )

# --------------------- Predictor set (Route A) ---------------------
# Rich, phase-8 style list including geography, HH composition, and assets/housing.
# Not all variables exist in every survey; we'll intersect with actual columns.
cand <- c(
  # Geography & HH composition
  "hv024","hv025","hv009","hv012","hv013","hv014","hv018","hv019",
  # Assets & housing / services (common across DHS-7/8; names can vary slightly)
  "hv201","hv202","hv204","hv205","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
  "hv213","hv214","hv215","hv216","hv217","hv218","hv219","hv220","hv221","hv222","hv223",
  "hv226","hv230","hv231","hv232","hv243","hv244","hv245","hv246","hv247"
)

x_vars <- intersect(cand, names(hr))

# Build modelling data:
# - Keep IDs (for export), wt, poor, and predictors
# - Convert labelled -> factor for categoricals
dat0 <- hr %>%
  select(any_of(c(id_vars, "poor", "wt", x_vars))) %>%
  mutate(across(where(is.labelled), ~ haven::as_factor(.x)))

# Drop rows with missing outcome
dat0 <- dat0 %>% filter(!is.na(poor))

# --------------------- Train / test split ---------------------
set.seed(2026)
spl <- initial_split(dat0, prop = 0.8, strata = poor)
train <- training(spl)
test  <- testing(spl)

# --------------------- Preprocess (recipe) ---------------------
# - Keep IDs as "id" role so they are carried through but not used in X
# --------------------- Preprocess (recipe) ---------------------
rec <- recipe(poor ~ ., data = train) %>%
  update_role(all_of(id_vars), new_role = "id") %>%
  update_role(wt, new_role = "case_weight") %>%
  # Impute missing values
  step_impute_median(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  # Drop zero variance BEFORE dummies
  step_zv(all_predictors()) %>%
  # One-hot encode categoricals
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  # Drop zero variance AGAIN (dummies may introduce all-zero cols)
  step_zv(all_predictors()) %>%
  # Standardize numeric predictors (leave weights alone)
  step_normalize(all_numeric_predictors(), -has_role("case_weight"))

prep_rec <- prep(rec, training = train, verbose = FALSE)

# --------------------- Design matrices helper ---------------------
# Bake to a DATA FRAME (not sparse), then convert to sparse afterwards.
bake_to_matrix <- function(new_data) {
  baked_df <- bake(prep_rec, new_data = new_data, composition = "data.frame")
  
  # Keep a copy of ids from the original new_data
  ids_out <- new_data %>% dplyr::select(dplyr::any_of(id_vars))
  
  # Build X by dropping outcome, weight, and IDs
  x_df <- baked_df %>%
    dplyr::select(-poor, -wt, -dplyr::any_of(id_vars))
  
  # Convert to sparse matrix efficiently
  # model.matrix() builds a design matrix; ~ . - 1 avoids intercept
  # But we already have numeric/dummies, so just go via Matrix::Matrix
  X <- Matrix::Matrix(as.matrix(x_df), sparse = TRUE)
  
  y <- baked_df$poor
  w <- baked_df$wt
  
  list(X = X, y = y, w = w, ids = ids_out)
}

trn <- bake_to_matrix(train)
tst <- bake_to_matrix(test)
# --------------------- LASSO with CV ---------------------
set.seed(2026)
cvfit <- cv.glmnet(
  x = trn$X, y = trn$y,
  family = "binomial",
  alpha  = 1,               # LASSO
  weights = trn$w,
  nfolds  = 10,
  standardize = FALSE       # already standardized in recipe
)

lambda_min <- cvfit$lambda.min
lambda_1se <- cvfit$lambda.1se  # more parsimonious
lambda_star <- lambda_1se

# --------------------- Evaluate on test ---------------------
# --------------------- Evaluate on test (fixed) ---------------------
# --------------------- Evaluate on test (yardstick df API) ---------------------
# --------------------- Evaluate on test (robust, yardstick df API) ---------------------

# 1) Predicted probabilities on test set
p_test <- as.numeric(predict(cvfit, newx = tst$X, s = lambda_star, type = "response"))

# 2) yardstick expects factor truth; make "1" the positive class (second level)
y_test_fac <- factor(tst$y, levels = c(0, 1), labels = c("0","1"))

# 3) Build evaluation df
eval_df <- tibble::tibble(truth = y_test_fac, .pred = p_test)

# 4) AUC
auc_val <- yardstick::roc_auc(eval_df, truth = truth, .pred, event_level = "second")$.estimate
cat(sprintf("\nUnweighted AUC: %.3f\n", auc_val))

# 5) ROC curve + Youden's J threshold
roc_tbl <- yardstick::roc_curve(eval_df, truth = truth, .pred, event_level = "second")
roc_df  <- as.data.frame(roc_tbl)

# yardstick gives 'specificity' and 'sensitivity'; compute fallout = 1 - specificity
roc_df$fallout <- 1 - roc_df$specificity

# Some endpoints can have non-finite thresholds; drop them before argmax
roc_df_finite <- subset(roc_df, is.finite(.threshold))

# Compute Youden's J
roc_df_finite$youden <- roc_df_finite$sensitivity - roc_df_finite$fallout

# Choose threshold (break ties by taking the smallest threshold achieving max J)
j_max <- max(roc_df_finite$youden, na.rm = TRUE)
thr <- min(roc_df_finite$.threshold[which(roc_df_finite$youden == j_max)], na.rm = TRUE)

cat(sprintf("Chosen threshold (Youden's J): %.6f\n", thr))

# 6) Class predictions at chosen threshold
pred_test <- as.integer(p_test >= thr)

# 7) Weighted metrics (manual with DHS weights)
w <- tst$w
weighted_acc  <- sum(w * (pred_test == tst$y)) / sum(w)
weighted_prec <- sum(w * (pred_test == 1 & tst$y == 1)) / sum(w * (pred_test == 1))
weighted_rec  <- sum(w * (pred_test == 1 & tst$y == 1)) / sum(w * (tst$y == 1))

cat(sprintf("Weighted Accuracy: %.3f | Precision: %.3f | Recall: %.3f\n",
            weighted_acc, weighted_prec, weighted_rec))
# --------------------- Variable importance ---------------------
coef_sparse <- coef(cvfit, s = lambda_star)
nz <- which(abs(coef_sparse) > 0)
imp <- data.frame(
  feature = rownames(coef_sparse)[nz],
  coef    = as.numeric(coef_sparse[nz])
) %>%
  filter(feature != "(Intercept)") %>%
  arrange(desc(abs(coef)))

cat("\nTop 20 absolute coefficients:\n")
print(head(imp, 20), row.names = FALSE)

# --------------------- Export scored households ---------------------
# Attach predictions & labels back to test IDs
test_scored <- test %>%
  select(any_of(id_vars), poor, wt) %>%
  mutate(p_hat = p_test, poor_hat = as.integer(p_hat >= thr))

# Save CSV (edit path as needed)
write_csv(test_scored, "ghana2022_hr_lasso_scored_test.csv")

# Also save fitted objects for reuse / deployment
saveRDS(list(cvfit = cvfit, prep_rec = prep_rec, lambda_star = lambda_star, thr = thr),
        file = "ghana2022_hr_lasso_model_objects.rds")

cat("\nFiles written:\n - ghana2022_hr_lasso_scored_test.csv\n - ghana2022_hr_lasso_model_objects.rds\n")


# Join predictions back to the raw test rows so we have readable categories
test_raw <- test %>% 
  mutate(across(where(is.labelled), haven::as_factor)) %>% 
  select(hv001, hv002, hv206, hv208, hv209, hv212, hv222, hv226, hv201, hv205, hv213, hv214, hv244, hv247, hv025, hv204)

scored <- dplyr::bind_cols(test_raw, tibble::tibble(p_hat = p_test, poor = tst$y, wt = tst$w))

# Weighted poverty rate by a few key predictors
library(dplyr)
weighted_rate <- function(df, var){
  df %>%
    group_by({{var}}) %>%
    summarise(
      n = n(),
      w_pop = sum(wt, na.rm=TRUE),
      pov_rate = sum(wt * (poor==1), na.rm=TRUE) / w_pop,
      avg_p_hat = sum(wt * p_hat, na.rm=TRUE) / w_pop
    ) %>% arrange(desc(pov_rate))
}

weighted_rate(scored, hv206)     # electricity
weighted_rate(scored, hv226)     # cooking fuel
weighted_rate(scored, hv205)     # sanitation
weighted_rate(scored, hv201)     # water source
weighted_rate(scored, hv025)     # urban/rural




scored <- scored %>%
  mutate(risk_decile = ntile(p_hat, 10))

scored %>%
  group_by(risk_decile) %>%
  summarise(
    w_pop = sum(wt),
    share_of_poor = sum(wt * (poor==1)) / sum(scored$wt * (scored$poor==1))
  ) %>% arrange(desc(risk_decile))