## run the packages
source("Code/R/packages and functions.R")

## run 2014, 2022 outcomes and the mapping script to make the panel df
source("Code/R/women_outcomes_14_DHS.R")
source("Code/R/women_outcomes_22_DHS.R")
source("Code/R/Ghana_cocoa_mapping.R")

## create a flag for region experience CSSVD start with intensity 3 and above
intensity_k <- 3
cocoa_k     <- 5000

joined_all <- joined_all |>
  mutate(
    CSSVD_intensity_2014_num = as.numeric(as.character(CSSVD_intensity_2014))
  )

adm2_flags <- joined_all |>
  transmute(
    ADM2_NAME = shapeName,
    hectares,
    CSSVD_intensity_2014 = CSSVD_intensity_2014_num,
    cssvd_flag    = as.integer(CSSVD_intensity_2014_num >= intensity_k),
    hectares_flag = as.integer(hectares >= cocoa_k)
  ) |>
  st_drop_geometry() |>  # we want to drop geometry here so good to do for memory
  select(-hectares, -CSSVD_intensity_2014)


## create a very basic first level panel df
# ============================================================
# Build 2-period panel with multiple outcomes (explicit rename)
# ============================================================
library(dplyr)
library(fixest)

# ============================================================
# 1) Select outcomes and rename to common names
# ============================================================

df14 <- adm2_outcomes_14 %>%
  transmute(
    ADM2_CODE, ADM2_NAME,
    year = 2014,
    emp_rate                  = emp_rate_14,
    agri_share_working        = agri_share_working_14,
    agri_emp_rate             = agri_emp_rate_14,
    nonagri_share_working     = nonagri_share_working_14,
    occ_missing_share_working = occ_missing_share_working_14,
    
    # --- wealth composition (ADM2) ---
    poor40_share              = poor40_share_14,
    mean_wealth_score         = mean_wealth_score_14,
    
    # --- NEW: norms proxy from v739 (ADM2) ---
    low_agency_v739_share     = low_agency_v739_share_14,
    n_v739_nonmiss            = n_v739_nonmiss_14
  )

df22 <- adm2_outcomes_22 %>%
  transmute(
    ADM2_CODE, ADM2_NAME,
    year = 2022,
    emp_rate                  = emp_rate_22,
    agri_share_working        = agri_share_working_22,
    agri_emp_rate             = agri_emp_rate_22,
    nonagri_share_working     = nonagri_share_working_22,
    occ_missing_share_working = occ_missing_share_working_22,
    
    # --- wealth composition (ADM2) ---
    poor40_share              = poor40_share_22,
    mean_wealth_score         = mean_wealth_score_22,
    
    # --- NEW: norms proxy from v739 (ADM2) ---
    low_agency_v739_share     = low_agency_v739_share_22,
    n_v739_nonmiss            = n_v739_nonmiss_22
  )

# ============================================================
# 2) Stack into a panel, join treatment flags, and create post
# ============================================================

adm2_panel <- bind_rows(df14, df22) %>%
  left_join(adm2_flags, by = "ADM2_NAME") %>%
  mutate(post = as.integer(year == 2022))

# ============================================================
# 3) Cocoa-only sample + drop singletons (balanced panel)
# ============================================================

reg_df <- adm2_panel %>%
  filter(hectares_flag == 1) %>%
  group_by(ADM2_NAME) %>%
  filter(n_distinct(year) == 2) %>%    # keep ADM2 observed in both 2014 & 2022
  ungroup()

# Quick checks (optional but useful)
table(reg_df$year)
length(unique(reg_df$ADM2_NAME))

# ============================================================
# 3a) Baseline household norms / agency (pre-period, 2014)
# ============================================================

# Pull baseline (2014) norms only
baseline_norms <- reg_df %>%
  filter(year == 2014) %>%
  select(
    ADM2_NAME,
    low_agency_2014 = low_agency_v739_share,
    n_norm_2014     = n_v739_nonmiss
  )

# Optional but recommended: drop districts with too little info
# (v739 has high missingness; this avoids very noisy ADM2 means)
min_norm_n <- 5

baseline_norms <- baseline_norms %>%
  mutate(
    low_agency_2014 = ifelse(n_norm_2014 >= min_norm_n, low_agency_2014, NA_real_)
  )

# Join baseline norms onto both years of the panel
reg_df <- reg_df %>%
  left_join(
    baseline_norms %>% select(ADM2_NAME, low_agency_2014),
    by = "ADM2_NAME"
  )

# Center for interaction interpretation
reg_df <- reg_df %>%
  mutate(
    low_agency_2014_c = low_agency_2014 - mean(low_agency_2014, na.rm = TRUE)
  )

# Quick diagnostic
summary(reg_df$low_agency_2014)
sum(is.na(reg_df$low_agency_2014))

# ============================================================
# 3b) Baseline poverty (pre-period) measure for heterogeneity
# ============================================================

baseline_poverty <- reg_df %>%
  filter(year == 2014) %>%
  select(ADM2_NAME, poor40_share_2014 = poor40_share)  # rename for clarity

# Define "poor region" as above-median baseline poor share (within cocoa sample)
poverty_cut <- median(baseline_poverty$poor40_share_2014, na.rm = TRUE)

baseline_poverty <- baseline_poverty %>%
  mutate(
    poor_region = as.integer(poor40_share_2014 >= poverty_cut)
  )

# Join baseline poverty group onto both years
reg_df <- reg_df %>%
  left_join(baseline_poverty %>% select(ADM2_NAME, poor40_share_2014, poor_region),
            by = "ADM2_NAME")

# ============================================================
# 4) DiD regressions + heterogeneity by baseline poverty
# ============================================================

outcomes <- c(
  "emp_rate",
  "agri_share_working",
  "agri_emp_rate",
  "nonagri_share_working",
  "occ_missing_share_working"
)

did_models_pov  <- list()
did_results_pov <- data.frame()

for (y in outcomes) {
  
  if (!y %in% names(reg_df)) next
  
  df_y <- reg_df %>%
    filter(!is.na(.data[[y]]), !is.na(poor_region)) %>%
    group_by(ADM2_NAME) %>%
    filter(n_distinct(year) == 2) %>%
    ungroup()
  
  # Heterogeneity DiD
  fml <- as.formula(paste0(
    y, " ~ post + post:cssvd_flag + post:poor_region + post:cssvd_flag:poor_region | ADM2_NAME"
  ))
  
  m <- feols(fml, data = df_y, cluster = ~ ADM2_NAME)
  did_models_pov[[y]] <- m
  
  co <- summary(m)$coeftable
  
  # Safely pull terms (in case a term is dropped)
  get_term <- function(term) {
    if (term %in% rownames(co)) {
      c(co[term, "Estimate"], co[term, "Std. Error"], co[term, "t value"], co[term, "Pr(>|t|)"])
    } else {
      c(NA, NA, NA, NA)
    }
  }
  
  base <- get_term("post:cssvd_flag")
  het  <- get_term("post:cssvd_flag:poor_region")
  
  did_results_pov <- bind_rows(
    did_results_pov,
    data.frame(
      outcome = y,
      
      # baseline DiD (non-poor regions)
      est_cssvd   = base[1], se_cssvd  = base[2], t_cssvd  = base[3], p_cssvd  = base[4],
      
      # extra effect in poor regions
      est_het     = het[1],  se_het    = het[2],  t_het    = het[3],  p_het    = het[4],
      
      N = nobs(m),
      districts = length(unique(df_y$ADM2_NAME))
    )
  )
}

# Pretty print
did_results_pov %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# ============================================================
# NEW: Plain DiD regressions (so did_results / did_models exist)
# (Your script later prints did_results and summaries did_models)
# ============================================================

did_models  <- list()
did_results <- data.frame()

for (y in outcomes) {
  
  if (!y %in% names(reg_df)) next
  
  df_y <- reg_df %>%
    filter(!is.na(.data[[y]])) %>%
    group_by(ADM2_NAME) %>%
    filter(n_distinct(year) == 2) %>%
    ungroup()
  
  fml <- as.formula(paste0(y, " ~ post + post:cssvd_flag | ADM2_NAME"))
  m <- feols(fml, data = df_y, cluster = ~ ADM2_NAME)
  
  did_models[[y]] <- m
  co <- summary(m)$coeftable
  
  did_results <- bind_rows(did_results, data.frame(
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

# ============================================================
# Continuous poverty gradient model (your existing block)
# ============================================================

y <- "emp_rate"

reg_df <- reg_df %>%
  mutate(poor40_share_2014_c = poor40_share_2014 - mean(poor40_share_2014, na.rm = TRUE))

fml <- as.formula(paste0(
  y, " ~ post + post:cssvd_flag + post:poor40_share_2014_c + post:cssvd_flag:poor40_share_2014_c | ADM2_NAME"
))

m_pov <- feols(fml, data = reg_df, cluster = ~ ADM2_NAME)

# See full output
summary(m_pov)

# ============================================================
# 5) Print results to 3 d.p. (without changing underlying numerics)
# ============================================================

did_results_3dp <- did_results
did_results_3dp[, c("estimate","std_error","t_value","p_value")] <-
  round(did_results_3dp[, c("estimate","std_error","t_value","p_value")], 3)

did_results_3dp

# If you want the *printed* version to always show trailing zeros (0.100),
# use this version for display/export:
did_results_print <- did_results %>%
  mutate(across(where(is.numeric), ~ format(round(.x, 3), nsmall = 3)))

did_results_print

# ============================================================
# 6) Example: inspect one model in full
# ============================================================

summary(did_models[["emp_rate"]])



##/////////////////////////////////////////////////////////
###     part 2 looking into occupation breakdowns
##/////////////////////////////////////////////////////////

library(dplyr)
library(fixest)

# ============================================================
# 1) Select outcomes and rename to common names (include OCC shares)
# ============================================================

# NEW: Define df14_occ/df22_occ (your script later uses these names)
df14_occ <- adm2_outcomes_14 %>%
  transmute(
    ADM2_CODE, ADM2_NAME,
    year = 2014,
    
    # core outcomes
    emp_rate                  = emp_rate_14,
    agri_emp_rate             = agri_emp_rate_14,
    agri_share_working        = agri_share_working_14,
    nonagri_share_working     = nonagri_share_working_14,
    occ_missing_share_working = occ_missing_share_working_14,
    
    # occupation destination shares among working women
    share_services_sales_working = share_services_sales_working_14,
    share_elementary_working     = share_elementary_working_14,
    share_skilled_ops_working    = share_skilled_ops_working_14,
    share_white_collar_working   = share_white_collar_working_14,
    share_other_working          = share_other_working_14,
    share_missing_occ_working    = share_missing_occ_working_14
  )

df22_occ <- adm2_outcomes_22 %>%
  transmute(
    ADM2_CODE, ADM2_NAME,
    year = 2022,
    
    # core outcomes
    emp_rate                  = emp_rate_22,
    agri_emp_rate             = agri_emp_rate_22,
    agri_share_working        = agri_share_working_22,
    nonagri_share_working     = nonagri_share_working_22,
    occ_missing_share_working = occ_missing_share_working_22,
    
    # occupation destination shares among working women
    share_services_sales_working = share_services_sales_working_22,
    share_elementary_working     = share_elementary_working_22,
    share_skilled_ops_working    = share_skilled_ops_working_22,
    share_white_collar_working   = share_white_collar_working_22,
    share_other_working          = share_other_working_22,
    share_missing_occ_working    = share_missing_occ_working_22
  )

# ============================================================
# 2) Stack panel, join flags, cocoa-only, drop singletons
# ============================================================

adm2_panel_occ <- bind_rows(df14_occ, df22_occ) %>%
  left_join(adm2_flags, by = "ADM2_NAME") %>%
  mutate(post = as.integer(year == 2022))

reg_df_occ <- adm2_panel_occ %>%
  filter(hectares_flag == 1) %>%
  group_by(ADM2_NAME) %>%
  filter(n_distinct(year) == 2) %>%
  ungroup()

# Quick checks
table(reg_df_occ$year)
length(unique(reg_df_occ$ADM2_NAME))

# ============================================================
# 3) FE DiD + clustered SEs for each outcome
# ============================================================

outcomes <- c(
  # core outcomes
  "emp_rate",
  "agri_emp_rate",
  "agri_share_working",
  "nonagri_share_working",
  "occ_missing_share_working",
  
  # occupation destination shares
  "share_services_sales_working",
  "share_elementary_working",
  "share_skilled_ops_working",
  "share_white_collar_working",
  "share_other_working",
  "share_missing_occ_working"
)

did_models  <- list()
did_results <- data.frame()

for (y in outcomes) {
  
  if (!y %in% names(reg_df_occ)) next
  
  # Balance per outcome (prevents fixest singleton drops due to NA patterns)
  df_y <- reg_df_occ %>%
    filter(!is.na(.data[[y]])) %>%
    group_by(ADM2_NAME) %>%
    filter(n_distinct(year) == 2) %>%
    ungroup()
  
  fml <- as.formula(paste0(y, " ~ post + post:cssvd_flag | ADM2_NAME"))
  m <- feols(fml, data = df_y, cluster = ~ ADM2_NAME)
  
  did_models[[y]] <- m
  
  co <- summary(m)$coeftable
  
  did_results <- bind_rows(
    did_results,
    data.frame(
      outcome   = y,
      term      = "post:cssvd_flag",
      estimate  = co["post:cssvd_flag", "Estimate"],
      std_error = co["post:cssvd_flag", "Std. Error"],
      t_value   = co["post:cssvd_flag", "t value"],
      p_value   = co["post:cssvd_flag", "Pr(>|t|)"],
      N         = nobs(m),
      districts = length(unique(df_y$ADM2_NAME))
    )
  )
}

# 4) Print to 3 d.p.
did_results_3dp <- did_results
did_results_3dp[, c("estimate","std_error","t_value","p_value")] <-
  round(did_results_3dp[, c("estimate","std_error","t_value","p_value")], 3)

did_results_3dp

# Create occupation "employment rates" among ALL women
# (share among employed * employment rate)

reg_df_occ <- reg_df_occ %>%
  mutate(
    emp_rate_agri        = emp_rate * agri_share_working,
    emp_rate_services    = emp_rate * share_services_sales_working,
    emp_rate_elementary  = emp_rate * share_elementary_working,
    emp_rate_skilled_ops = emp_rate * share_skilled_ops_working,
    emp_rate_whitecollar = emp_rate * share_white_collar_working,
    emp_rate_other       = emp_rate * share_other_working,
    emp_rate_missingocc  = emp_rate * share_missing_occ_working
  )

# Quick check: occupation rates should sum to (approx) total emp_rate
reg_df_occ %>%
  mutate(sum_occ_rates =
           emp_rate_services +
           emp_rate_elementary +
           emp_rate_skilled_ops +
           emp_rate_whitecollar +
           emp_rate_other +
           emp_rate_missingocc +
           if ("emp_rate_agri" %in% names(.)) emp_rate_agri else 0
  ) %>%
  summarise(
    min_gap = min(emp_rate - sum_occ_rates, na.rm = TRUE),
    max_gap = max(emp_rate - sum_occ_rates, na.rm = TRUE),
    mean_gap = mean(emp_rate - sum_occ_rates, na.rm = TRUE)
  )

rate_outcomes <- c(
  "emp_rate_services",
  "emp_rate_elementary",
  "emp_rate_skilled_ops",
  "emp_rate_whitecollar",
  "emp_rate_other",
  "emp_rate_missingocc"
)

# include agri if you created it
if ("emp_rate_agri" %in% names(reg_df_occ)) rate_outcomes <- c("emp_rate_agri", rate_outcomes)

rate_results <- data.frame()
rate_models  <- list()

for (y in rate_outcomes) {
  
  df_y <- reg_df_occ %>%
    filter(!is.na(.data[[y]])) %>%
    group_by(ADM2_NAME) %>%
    filter(n_distinct(year) == 2) %>%
    ungroup()
  
  fml <- as.formula(paste0(y, " ~ post + post:cssvd_flag | ADM2_NAME"))
  m <- feols(fml, data = df_y, cluster = ~ ADM2_NAME)
  
  rate_models[[y]] <- m
  co <- summary(m)$coeftable
  
  rate_results <- bind_rows(rate_results, data.frame(
    outcome   = y,
    estimate  = co["post:cssvd_flag", "Estimate"],
    std_error = co["post:cssvd_flag", "Std. Error"],
    t_value   = co["post:cssvd_flag", "t value"],
    p_value   = co["post:cssvd_flag", "Pr(>|t|)"],
    N         = nobs(m),
    districts = length(unique(df_y$ADM2_NAME))
  ))
}

# 3dp display
rate_results[, c("estimate","std_error","t_value","p_value")] <-
  round(rate_results[, c("estimate","std_error","t_value","p_value")], 3)

rate_results

##### this just summarises the results that poor regions do not see larger effects

did_results_pov <- did_results_pov %>%
  mutate(
    est_total_poor = est_cssvd + est_het
  )

did_results_pov %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

### running the same test but with terciles instead of median

# ============================================================
# A1) Build terciles using baseline poverty (2014)
# ============================================================

# Ensure baseline poverty exists for each ADM2 (copied across both years)
stopifnot("poor40_share_2014" %in% names(reg_df))

p33 <- quantile(reg_df$poor40_share_2014, probs = 1/3, na.rm = TRUE)
p67 <- quantile(reg_df$poor40_share_2014, probs = 2/3, na.rm = TRUE)

reg_df <- reg_df %>%
  mutate(
    poor_tercile = case_when(
      poor40_share_2014 <= p33 ~ 1L,      # least poor
      poor40_share_2014 <= p67 ~ 2L,      # middle
      poor40_share_2014 >  p67 ~ 3L,      # most poor
      TRUE ~ NA_integer_
    ),
    poor_tercile = factor(poor_tercile, levels = c(1,2,3))
  )

table(reg_df$poor_tercile, useNA = "ifany")

# ============================================================
# A2) FE DiD with tercile heterogeneity
# ============================================================

m_emp_terc <- feols(
  emp_rate ~ post + post:cssvd_flag + post:poor_tercile + post:cssvd_flag:poor_tercile | ADM2_NAME,
  data = reg_df,
  cluster = ~ ADM2_NAME
)
## this displays results for wealth terciles
summary(m_emp_terc)

# ============================================================
# NEW: Baseline norms heterogeneity regressions (add at bottom)
# ============================================================

# 1) Continuous norms heterogeneity for women’s employment
m_norm_emp <- feols(
  emp_rate ~ post +
    post:cssvd_flag +
    post:low_agency_2014_c +
    post:cssvd_flag:low_agency_2014_c
  | ADM2_NAME,
  data = reg_df,
  cluster = ~ ADM2_NAME
)

summary(m_norm_emp)

# 2) Optional: horse race with baseline poverty + baseline norms (both continuous)
m_pov_norm_emp <- feols(
  emp_rate ~ post +
    post:cssvd_flag +
    post:poor40_share_2014_c + post:cssvd_flag:poor40_share_2014_c +
    post:low_agency_2014_c   + post:cssvd_flag:low_agency_2014_c
  | ADM2_NAME,
  data = reg_df,
  cluster = ~ ADM2_NAME
)

summary(m_pov_norm_emp)