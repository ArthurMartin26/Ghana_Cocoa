## DHS 2014: Men MR (GHMR72FL) + GPS clusters (GHGE71FL) -> ADM2 -> ADM2 outcomes

source("Code/R/packages and functions.R")

# ----------------------------
# 1) Variables of interest (MR -> renamed to v* later)
# ----------------------------

# Employment-related variables (after renaming mv* -> v*)
employ_vars_14 <- c("v741", "v716", "v714", "v732", "v731")

# Wealth index (household-level; attached to man record)
wealth_vars_14 <- c("v190", "v191")

# ----------------------------
# 2) File paths (2014)
# ----------------------------

ge_path_14 <- "Data/Data_Raw/DHS/GH_2014_DHS_GEOG/GHGE71FL/GHGE71FL.shp"

# NOTE: update file name if yours differs (often GHMR72FL.DTA for 2014 standard MR)
mr_path_14 <- "Data/Data_Raw/DHS/GH_2014_DHS_Standard/GHMR71FL.DTA"

adm2_path  <- "Data/Data_Raw/Geographic/geoBoundaries-GHA-ADM2_simplified.geojson"

# ----------------------------
# 3) Read spatial data + build ADM2 lookup for clusters
# ----------------------------

ge_14   <- st_read(ge_path_14, quiet = TRUE)
adm2_14 <- st_read(adm2_path, quiet = TRUE) %>%
  st_transform(st_crs(ge_14))

clusters_adm2_14 <- st_join(
  ge_14,
  adm2_14 %>% select(ADM2_NAME = shapeName, ADM2_CODE = shapeID),
  join = st_within
)

lookup_14 <- clusters_adm2_14 %>%
  st_drop_geometry() %>%
  transmute(v001 = DHSCLUST, ADM2_CODE, ADM2_NAME)

# ----------------------------
# 4) Read MR, rename mv* -> v*, merge ADM2, keep needed vars + weights
# ----------------------------

mr_14_raw <- read_dta(mr_path_14)

# Adapter: rename mv* variables to v* so we can reuse the same downstream code
mr_14 <- mr_14_raw %>%
  rename_with(~ stringr::str_replace(.x, "^mv", "v"), dplyr::starts_with("mv"))

# Optional: if you want strict comparability to women 15–49, uncomment:
# mr_14 <- mr_14 %>% filter(v012 >= 15, v012 <= 49)

# Safety check: fail fast if key vars are missing (helps debugging if DHS recode differs)
required_vars <- c("v001","v024","v005", employ_vars_14, wealth_vars_14)
missing_vars <- setdiff(required_vars, names(mr_14))
if (length(missing_vars) > 0) {
  stop("Missing required variables in MR file after renaming: ",
       paste(missing_vars, collapse = ", "))
}

mr_14_adm2 <- mr_14 %>%
  left_join(lookup_14, by = "v001") %>%
  select(
    v001, v024, v005,
    all_of(employ_vars_14),
    all_of(wealth_vars_14),
    ADM2_CODE, ADM2_NAME
  ) %>%
  mutate(wt = v005 / 1000000)

# ----------------------------
# 5) Derived micro variables (work, occupation buckets, wealth)
# ----------------------------

mr_14_adm2 <- mr_14_adm2 %>%
  mutate(
    # ---- Employment ----
    is_working = as.integer(v714 == 1),
    
    # Agriculture occupation codes (same grouping you used)
    is_agri_occ = as.integer(v716 %in% c(61, 62, 63, 92)),
    
    # Missing occupation among working
    occ_missing_working = as.integer(is_working == 1 & is.na(v716)),
    
    # Agriculture among working only
    is_agri_working = as.integer(is_working == 1 & is_agri_occ == 1),
    
    # Non-agri among working only (occupation observed)
    is_nonagri_working = as.integer(is_working == 1 & !is.na(v716) & is_agri_occ == 0),
    
    # ---- Wealth ----
    wealth_q = as.integer(v190),         # 1..5 wealth quintile
    wealth_s = as.numeric(v191),         # continuous factor score
    wealth_q = ifelse(wealth_q %in% 1:5, wealth_q, NA_integer_),
    
    poor40 = as.integer(wealth_q %in% c(1, 2)),
    rich40 = as.integer(wealth_q %in% c(4, 5)),
    
    # ---- Occupation "destination" buckets (among working men) ----
    occ_bucket_14 = case_when(
      is_working == 0               ~ NA_character_,
      is_working == 1 & is.na(v716) ~ "missing_occ",
      
      v716 %in% c(61, 62, 63, 92)       ~ "agri",
      v716 %in% c(51, 52, 53, 54, 95)   ~ "services_sales",
      v716 %in% c(91, 93, 94, 96)       ~ "elementary",
      v716 %in% 71:75 | v716 %in% 81:82 ~ "skilled_ops",
      
      v716 %in% 11:14                   ~ "white_collar",
      v716 %in% 21:26 | v716 %in% 31:34 ~ "white_collar",
      v716 %in% 41:43                   ~ "white_collar",
      
      v716 == 1                         ~ "other",
      TRUE                              ~ "other"
    ),
    
    # bucket dummies (among working)
    occ_agri_14         = as.integer(occ_bucket_14 == "agri"),
    occ_services_14     = as.integer(occ_bucket_14 == "services_sales"),
    occ_elementary_14   = as.integer(occ_bucket_14 == "elementary"),
    occ_skilled_ops_14  = as.integer(occ_bucket_14 == "skilled_ops"),
    occ_white_collar_14 = as.integer(occ_bucket_14 == "white_collar"),
    occ_other_14        = as.integer(occ_bucket_14 == "other"),
    occ_missing_14      = as.integer(occ_bucket_14 == "missing_occ")
  )

# ----------------------------
# 6) Aggregate to ADM2 outcomes (2014) — MEN
# ----------------------------

adm2_outcomes_men_14 <- mr_14_adm2 %>%
  filter(!is.na(ADM2_NAME), !is.na(wt)) %>%
  group_by(ADM2_CODE, ADM2_NAME) %>%
  summarise(
    # ---- Wealth composition ----
    poor40_share_men_14        = weighted.mean(poor40, wt, na.rm = TRUE),
    rich40_share_men_14        = weighted.mean(rich40, wt, na.rm = TRUE),
    mean_wealth_score_men_14   = weighted.mean(wealth_s, wt, na.rm = TRUE),
    share_wq1_men_14           = weighted.mean(as.integer(wealth_q == 1), wt, na.rm = TRUE),
    share_wq2_men_14           = weighted.mean(as.integer(wealth_q == 2), wt, na.rm = TRUE),
    share_wq3_men_14           = weighted.mean(as.integer(wealth_q == 3), wt, na.rm = TRUE),
    share_wq4_men_14           = weighted.mean(as.integer(wealth_q == 4), wt, na.rm = TRUE),
    share_wq5_men_14           = weighted.mean(as.integer(wealth_q == 5), wt, na.rm = TRUE),
    
    # ---- Core employment ----
    emp_rate_men_14            = weighted.mean(is_working, wt, na.rm = TRUE),
    agri_emp_rate_men_14       = weighted.mean(is_agri_working, wt, na.rm = TRUE),
    
    # Share in agriculture among employed men
    agri_share_working_men_14 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0,
             sum(wt[is_agri_working == 1], na.rm = TRUE) / w_emp,
             NA_real_)
    },
    
    # Share in non-agri among employed men (occupation observed)
    nonagri_share_working_men_14 = {
      w_emp_obsocc <- sum(wt[is_working == 1 & !is.na(v716)], na.rm = TRUE)
      ifelse(w_emp_obsocc > 0,
             sum(wt[is_nonagri_working == 1], na.rm = TRUE) / w_emp_obsocc,
             NA_real_)
    },
    
    # % of working men missing occupation
    occ_missing_share_working_men_14 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0,
             sum(wt[occ_missing_working == 1], na.rm = TRUE) / w_emp,
             NA_real_)
    },
    
    # ---- Destination shares among employed men (denom = all employed) ----
    share_agri_working_men_14 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0, sum(wt[occ_agri_14 == 1], na.rm = TRUE) / w_emp, NA_real_)
    },
    share_services_sales_working_men_14 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0, sum(wt[occ_services_14 == 1], na.rm = TRUE) / w_emp, NA_real_)
    },
    share_elementary_working_men_14 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0, sum(wt[occ_elementary_14 == 1], na.rm = TRUE) / w_emp, NA_real_)
    },
    share_skilled_ops_working_men_14 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0, sum(wt[occ_skilled_ops_14 == 1], na.rm = TRUE) / w_emp, NA_real_)
    },
    share_white_collar_working_men_14 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0, sum(wt[occ_white_collar_14 == 1], na.rm = TRUE) / w_emp, NA_real_)
    },
    share_other_working_men_14 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0, sum(wt[occ_other_14 == 1], na.rm = TRUE) / w_emp, NA_real_)
    },
    share_missing_occ_working_men_14 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0, sum(wt[occ_missing_14 == 1], na.rm = TRUE) / w_emp, NA_real_)
    },
    
    # ---- Counts ----
    n_men_14 = sum(!is.na(is_working)),
    .groups = "drop"
  )

# ----------------------------
# 7) Quick checks
# ----------------------------

adm2_outcomes_men_14 %>%
  summarise(
    min_emp = min(emp_rate_men_14, na.rm = TRUE),
    max_emp = max(emp_rate_men_14, na.rm = TRUE),
    min_agri_share = min(agri_share_working_men_14, na.rm = TRUE),
    max_agri_share = max(agri_share_working_men_14, na.rm = TRUE),
    min_sum_dest = min(
      share_agri_working_men_14 +
        share_services_sales_working_men_14 +
        share_elementary_working_men_14 +
        share_skilled_ops_working_men_14 +
        share_white_collar_working_men_14 +
        share_other_working_men_14 +
        share_missing_occ_working_men_14,
      na.rm = TRUE
    ),
    max_sum_dest = max(
      share_agri_working_men_14 +
        share_services_sales_working_men_14 +
        share_elementary_working_men_14 +
        share_skilled_ops_working_men_14 +
        share_white_collar_working_men_14 +
        share_other_working_men_14 +
        share_missing_occ_working_men_14,
      na.rm = TRUE
    )
  )

summary(adm2_outcomes_men_14$occ_missing_share_working_men_14)

# ----------------------------
# 8) Join outcomes onto polygons for mapping (optional)
# ----------------------------

adm2_map_men_14 <- adm2_14 %>%
  mutate(ADM2_CODE = shapeID, ADM2_NAME = shapeName) %>%
  left_join(adm2_outcomes_men_14, by = c("ADM2_CODE", "ADM2_NAME"))