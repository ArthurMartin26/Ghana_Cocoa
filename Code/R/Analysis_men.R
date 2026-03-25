## ============================================================
## Ghana DHS (Men): ADM2 panel + DiD analysis (2014 vs 2022/23)
## Mirrors women's analysis but EXCLUDES agency/norms parts
## ============================================================

## 0) Packages & helpers
source("Code/R/packages and functions.R")
library(dplyr)
library(fixest)
library(sf)

## 1) Build ADM2 outcomes for MEN and load mapping
source("Code/R/men_outcomes_14_DHS.R")   # -> adm2_outcomes_men_14
source("Code/R/men_outcomes_22_DHS.R")   # -> adm2_outcomes_men_22
source("Code/R/Ghana_cocoa_mapping.R")   # -> joined_all (sf), CSSVD & cocoa metrics

## 2) Treatment/intensity flags from mapping (same as women’s)
intensity_k <- 3
cocoa_k     <- 5000

# Ensure numeric
joined_all <- joined_all |>
  mutate(CSSVD_intensity_2014_num = as.numeric(as.character(CSSVD_intensity_2014)))

adm2_flags <- joined_all |>
  transmute(
    ADM2_NAME = shapeName,
    hectares,
    CSSVD_intensity_2014 = CSSVD_intensity_2014_num,
    cssvd_flag    = as.integer(CSSVD_intensity_2014_num >= intensity_k),
    hectares_flag = as.integer(hectares >= cocoa_k)
  ) |>
  st_drop_geometry() |>
  select(-hectares, -CSSVD_intensity_2014)  # keep only flags

## ============================================================
## A) Core 2‑period panel (MEN)
## ============================================================

# 1) Select outcomes for each wave and rename to common names
df14_men <- adm2_outcomes_men_14 %>%
  transmute(
    ADM2_CODE, ADM2_NAME,
    year = 2014,
    # ---- Core employment & composition ----
    emp_rate                  = emp_rate_men_14,
    agri_share_working        = agri_share_working_men_14,
    agri_emp_rate             = agri_emp_rate_men_14,
    nonagri_share_working     = nonagri_share_working_men_14,
    occ_missing_share_working = occ_missing_share_working_men_14,
    # ---- Wealth composition (baseline) ----
    poor40_share              = poor40_share_men_14,
    mean_wealth_score         = mean_wealth_score_men_14
  )

df22_men <- adm2_outcomes_men_22 %>%
  transmute(
    ADM2_CODE, ADM2_NAME,
    year = 2022,  # keep 2022 label for comparability
    emp_rate                  = emp_rate_men_22,
    agri_share_working        = agri_share_working_men_22,
    agri_emp_rate             = agri_emp_rate_men_22,
    nonagri_share_working     = nonagri_share_working_men_22,
    occ_missing_share_working = occ_missing_share_working_men_22,
    poor40_share              = poor40_share_men_22,
    mean_wealth_score         = mean_wealth_score_men_22
  )

# 2) Stack panel, join flags, and `post`
adm2_panel_men <- bind_rows(df14_men, df22_men) %>%
  left_join(adm2_flags, by = "ADM2_NAME") %>%
  mutate(post = as.integer(year == 2022))

# 3) Cocoa-only + balanced panel per ADM2
reg_df_men <- adm2_panel_men %>%
  filter(hectares_flag == 1) %>%
  group_by(ADM2_NAME) %>%
  filter(n_distinct(year) == 2) %>%    # balanced
  ungroup()

# Quick checks
if (interactive()) {
  print(table(reg_df_men$year))
  print(length(unique(reg_df_men$ADM2_NAME)))
}

## ============================================================
## B) Baseline poverty (2014) & heterogeneity setup
## ============================================================

# Baseline poverty from MEN 2014 (you can switch to women if preferred)
baseline_poverty_men <- reg_df_men %>%
  filter(year == 2014) %>%
  select(ADM2_NAME, poor40_share_2014 = poor40_share)

# Median split (poor vs less-poor)
poverty_cut_men <- median(baseline_poverty_men$poor40_share_2014, na.rm = TRUE)

baseline_poverty_men <- baseline_poverty_men %>%
  mutate(poor_region = as.integer(poor40_share_2014 >= poverty_cut_men))

# Join to both years
reg_df_men <- reg_df_men %>%
  left_join(baseline_poverty_men %>% select(ADM2_NAME, poor40_share_2014, poor_region),
            by = "ADM2_NAME")

# Center continuous poverty for interaction interpretation
reg_df_men <- reg_df_men %>%
  mutate(poor40_share_2014_c = poor40_share_2014 - mean(poor40_share_2014, na.rm = TRUE))

## ============================================================
## C) Difference‑in‑Differences (plain) + poverty heterogeneity
## ============================================================

outcomes <- c(
  "emp_rate",
  "agri_share_working",
  "agri_emp_rate",
  "nonagri_share_working",
  "occ_missing_share_working"
)

# --- Plain DiD (post × cssvd_flag), FE by ADM2, cluster by ADM2
did_models_men  <- list()
did_results_men <- data.frame()

for (y in outcomes) {
  
  if (!y %in% names(reg_df_men)) next
  
  df_y <- reg_df_men %>%
    filter(!is.na(.data[[y]])) %>%
    group_by(ADM2_NAME) %>%
    filter(n_distinct(year) == 2) %>%           # balanced
    filter(sd(.data[[y]]) > 0) %>%             # <-- NEW: outcome varies
    ungroup()
  
  fml <- as.formula(paste0(y, " ~ post + post:cssvd_flag | ADM2_NAME"))
  m   <- feols(fml, data = df_y, cluster = ~ ADM2_NAME)
  
  did_models_men[[y]] <- m
  co <- summary(m)$coeftable
  
  did_results_men <- bind_rows(did_results_men, data.frame(
    outcome   = y,
    term      = "post:cssvd_flag",
    estimate  = co["post:cssvd_flag", "Estimate"],
    std_error = co["post:cssvd_flag", "Std. Error"],
    t_value   = co["post:cssvd_flag", "t value"],
    p_value   = co["post:cssvd_flag", "Pr(>|t|)"],
    N         = nobs(m),
    districts = length(unique(df_y$ADM2_NAME))
  ))
}

# Pretty print (3 d.p.)
did_results_men_3dp <- did_results_men
did_results_men_3dp[, c("estimate","std_error","t_value","p_value")] <-
  round(did_results_men_3dp[, c("estimate","std_error","t_value","p_value")], 3)

if (interactive()) {
  print(did_results_men_3dp)
  print(summary(did_models_men[["emp_rate"]]))
}

# --- Heterogeneity by baseline poverty (median split)
did_models_pov_men  <- list()
did_results_pov_men <- data.frame()

for (y in outcomes) {
  
  if (!y %in% names(reg_df_men)) next
  
  df_y <- reg_df_men %>%
    filter(!is.na(.data[[y]])) %>%
    group_by(ADM2_NAME) %>%
    filter(n_distinct(year) == 2) %>%           # balanced
    filter(sd(.data[[y]]) > 0) %>%             # <-- NEW: outcome varies
    ungroup()
  
  fml <- as.formula(paste0(
    y, " ~ post + post:cssvd_flag + post:poor_region + post:cssvd_flag:poor_region | ADM2_NAME"
  ))
  m <- feols(fml, data = df_y, cluster = ~ ADM2_NAME)
  did_models_pov_men[[y]] <- m
  co <- summary(m)$coeftable
  
  get_term <- function(term) {
    if (term %in% rownames(co)) {
      c(co[term, "Estimate"], co[term, "Std. Error"], co[term, "t value"], co[term, "Pr(>|t|)"])
    } else c(NA, NA, NA, NA)
  }
  
  base <- get_term("post:cssvd_flag")
  het  <- get_term("post:cssvd_flag:poor_region")
  
  did_results_pov_men <- bind_rows(
    did_results_pov_men,
    data.frame(
      outcome = y,
      est_cssvd = base[1], se_cssvd = base[2], t_cssvd = base[3], p_cssvd = base[4],
      est_het   = het[1],  se_het   = het[2],  t_het   = het[3],  p_het   = het[4],
      N = nobs(m),
      districts = length(unique(df_y$ADM2_NAME))
    )
  )
}

# Total effect in poor regions = base + heterogeneity increment
did_results_pov_men <- did_results_pov_men %>%
  mutate(est_total_poor = est_cssvd + est_het) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

if (interactive()) print(did_results_pov_men)

# --- Continuous poverty gradient (example for emp_rate)
y <- "emp_rate"
m_pov_men <- feols(
  as.formula(paste0(
    y, " ~ post + post:cssvd_flag + ",
    "post:poor40_share_2014_c + post:cssvd_flag:poor40_share_2014_c | ADM2_NAME"
  )),
  data = reg_df_men,
  cluster = ~ ADM2_NAME
)
if (interactive()) print(summary(m_pov_men))

## ============================================================
## D) Occupation breakdown panel (MEN) + DiD on shares & rates
## ============================================================

# 1) Rebuild panel with occupation destination shares
df14_occ_men <- adm2_outcomes_men_14 %>%
  transmute(
    ADM2_CODE, ADM2_NAME,
    year = 2014,
    # core
    emp_rate                  = emp_rate_men_14,
    agri_emp_rate             = agri_emp_rate_men_14,
    agri_share_working        = agri_share_working_men_14,
    nonagri_share_working     = nonagri_share_working_men_14,
    occ_missing_share_working = occ_missing_share_working_men_14,
    # destination shares among employed MEN
    share_services_sales_working = share_services_sales_working_men_14,
    share_elementary_working     = share_elementary_working_men_14,
    share_skilled_ops_working    = share_skilled_ops_working_men_14,
    share_white_collar_working   = share_white_collar_working_men_14,
    share_other_working          = share_other_working_men_14,
    share_missing_occ_working    = share_missing_occ_working_men_14
  )

df22_occ_men <- adm2_outcomes_men_22 %>%
  transmute(
    ADM2_CODE, ADM2_NAME,
    year = 2022,
    emp_rate                  = emp_rate_men_22,
    agri_emp_rate             = agri_emp_rate_men_22,
    agri_share_working        = agri_share_working_men_22,
    nonagri_share_working     = nonagri_share_working_men_22,
    occ_missing_share_working = occ_missing_share_working_men_22,
    share_services_sales_working = share_services_sales_working_men_22,
    share_elementary_working     = share_elementary_working_men_22,
    share_skilled_ops_working    = share_skilled_ops_working_men_22,
    share_white_collar_working   = share_white_collar_working_men_22,
    share_other_working          = share_other_working_men_22,
    share_missing_occ_working    = share_missing_occ_working_men_22
  )

adm2_panel_occ_men <- bind_rows(df14_occ_men, df22_occ_men) %>%
  left_join(adm2_flags, by = "ADM2_NAME") %>%
  mutate(post = as.integer(year == 2022))

reg_df_occ_men <- adm2_panel_occ_men %>%
  filter(hectares_flag == 1) %>%
  group_by(ADM2_NAME) %>%
  filter(n_distinct(year) == 2) %>%
  ungroup()

# 2) DiD across all occupation outcomes
occ_outcomes <- c(
  # core
  "emp_rate",
  "agri_emp_rate",
  "agri_share_working",
  "nonagri_share_working",
  "occ_missing_share_working",
  # destination shares among employed
  "share_services_sales_working",
  "share_elementary_working",
  "share_skilled_ops_working",
  "share_white_collar_working",
  "share_other_working",
  "share_missing_occ_working"
)

did_models_occ_men  <- list()
did_results_occ_men <- data.frame()

for (y in occ_outcomes) {
  
  if (!y %in% names(reg_df_occ_men)) next
  
  df_y <- reg_df_occ_men %>%
    filter(!is.na(.data[[y]])) %>%
    group_by(ADM2_NAME) %>%
    filter(n_distinct(year) == 2) %>%
    ungroup()
  
  fml <- as.formula(paste0(y, " ~ post + post:cssvd_flag | ADM2_NAME"))
  m   <- feols(fml, data = df_y, cluster = ~ ADM2_NAME)
  
  did_models_occ_men[[y]] <- m
  co <- summary(m)$coeftable
  
  did_results_occ_men <- bind_rows(did_results_occ_men, data.frame(
    outcome   = y,
    estimate  = co["post:cssvd_flag", "Estimate"],
    std_error = co["post:cssvd_flag", "Std. Error"],
    t_value   = co["post:cssvd_flag", "t value"],
    p_value   = co["post:cssvd_flag", "Pr(>|t|)"],
    N         = nobs(m),
    districts = length(unique(df_y$ADM2_NAME))
  ))
}

did_results_occ_men[, c("estimate","std_error","t_value","p_value")] <-
  round(did_results_occ_men[, c("estimate","std_error","t_value","p_value")], 3)

if (interactive()) print(did_results_occ_men)

# 3) Convert destination shares -> employment rates among ALL MEN
reg_df_occ_men <- reg_df_occ_men %>%
  mutate(
    emp_rate_agri        = emp_rate * agri_share_working,
    emp_rate_services    = emp_rate * share_services_sales_working,
    emp_rate_elementary  = emp_rate * share_elementary_working,
    emp_rate_skilled_ops = emp_rate * share_skilled_ops_working,
    emp_rate_whitecollar = emp_rate * share_white_collar_working,
    emp_rate_other       = emp_rate * share_other_working,
    emp_rate_missingocc  = emp_rate * share_missing_occ_working
  )

# Check closure: sum of occupation-specific employment ≈ overall emp_rate
if (interactive()) {
  print(
    reg_df_occ_men %>%
      mutate(sum_occ_rates =
               emp_rate_agri +
               emp_rate_services +
               emp_rate_elementary +
               emp_rate_skilled_ops +
               emp_rate_whitecollar +
               emp_rate_other +
               emp_rate_missingocc) %>%
      summarise(
        min_gap  = min(emp_rate - sum_occ_rates, na.rm = TRUE),
        max_gap  = max(emp_rate - sum_occ_rates, na.rm = TRUE),
        mean_gap = mean(emp_rate - sum_occ_rates, na.rm = TRUE)
      )
  )
}

rate_outcomes <- c(
  "emp_rate_agri",
  "emp_rate_services",
  "emp_rate_elementary",
  "emp_rate_skilled_ops",
  "emp_rate_whitecollar",
  "emp_rate_other",
  "emp_rate_missingocc"
)

rate_models_men  <- list()
rate_results_men <- data.frame()

for (y in rate_outcomes) {
  
  df_y <- reg_df_occ_men %>%
    filter(!is.na(.data[[y]])) %>%
    group_by(ADM2_NAME) %>%
    filter(n_distinct(year) == 2) %>%
    ungroup()
  
  fml <- as.formula(paste0(y, " ~ post + post:cssvd_flag | ADM2_NAME"))
  m   <- feols(fml, data = df_y, cluster = ~ ADM2_NAME)
  
  rate_models_men[[y]] <- m
  co <- summary(m)$coeftable
  
  rate_results_men <- bind_rows(rate_results_men, data.frame(
    outcome   = y,
    estimate  = co["post:cssvd_flag", "Estimate"],
    std_error = co["post:cssvd_flag", "Std. Error"],
    t_value   = co["post:cssvd_flag", "t value"],
    p_value   = co["post:cssvd_flag", "Pr(>|t|)"],
    N         = nobs(m),
    districts = length(unique(df_y$ADM2_NAME))
  ))
}

rate_results_men[, c("estimate","std_error","t_value","p_value")] <-
  round(rate_results_men[, c("estimate","std_error","t_value","p_value")], 3)

if (interactive()) print(rate_results_men)

## ============================================================
## E) Tercile heterogeneity on baseline poverty (MEN)
## ============================================================

stopifnot("poor40_share_2014" %in% names(reg_df_men))

p33 <- quantile(reg_df_men$poor40_share_2014, probs = 1/3, na.rm = TRUE)
p67 <- quantile(reg_df_men$poor40_share_2014, probs = 2/3, na.rm = TRUE)

reg_df_men <- reg_df_men %>%
  mutate(
    poor_tercile = case_when(
      poor40_share_2014 <= p33 ~ 1L,      # least poor
      poor40_share_2014 <= p67 ~ 2L,      # middle
      poor40_share_2014 >  p67 ~ 3L,      # most poor
      TRUE ~ NA_integer_
    ),
    poor_tercile = factor(poor_tercile, levels = c(1,2,3))
  )

m_emp_terc_men <- feols(
  emp_rate ~ post + post:cssvd_flag + post:poor_tercile + post:cssvd_flag:poor_tercile | ADM2_NAME,
  data = reg_df_men,
  cluster = ~ ADM2_NAME
)

if (interactive()) summary(m_emp_terc_men)

## ============================================================
## End of men_analysis_DHS.R
## ============================================================