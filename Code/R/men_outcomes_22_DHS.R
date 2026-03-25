## DHS 2022/23: Men MR + GPS clusters -> ADM2 -> ADM2 outcomes

source("Code/R/packages and functions.R")

# ----------------------------
# 1) Variables of interest (MR -> renamed to v*)
# ----------------------------

# Employment-related variables
employ_vars_22 <- c("v741", "v716", "v714", "v732", "v731")

# Wealth index (household-level)
wealth_vars_22 <- c("v190", "v191")

# ----------------------------
# 2) File paths (2022/23)
# ----------------------------

ge_path_22   <- "Data/Data_Raw/DHS/GH_2022_DHS_GEOG/GHGE8AFL/GHGE8AFL.shp"

# adjust filename if needed (this is the standard)
mr_path_22 <- "Data/Data_Raw/DHS/GH_2022_DHS_Standard/GHMR8CFL.DTA"

adm2_path  <- "Data/Data_Raw/Geographic/geoBoundaries-GHA-ADM2_simplified.geojson"

# ----------------------------
# 3) Read spatial data + build ADM2 lookup for clusters
# ----------------------------

ge_22   <- st_read(ge_path_22, quiet = TRUE)
adm2_22 <- st_read(adm2_path, quiet = TRUE) %>%
  st_transform(st_crs(ge_22))

clusters_adm2_22 <- st_join(
  ge_22,
  adm2_22 %>% select(ADM2_NAME = shapeName, ADM2_CODE = shapeID),
  join = st_within
)

lookup_22 <- clusters_adm2_22 %>%
  st_drop_geometry() %>%
  transmute(v001 = DHSCLUST, ADM2_CODE, ADM2_NAME)

# ----------------------------
# 4) Read MR, rename mv* -> v*, merge ADM2, keep vars + weights
# ----------------------------

mr_22_raw <- read_dta(mr_path_22)

# Adapter: rename mv* to v*
mr_22 <- mr_22_raw %>%
  rename_with(~ stringr::str_replace(.x, "^mv", "v"),
              dplyr::starts_with("mv"))

# Optional: restrict to 15–49 for strict comparability
# mr_22 <- mr_22 %>% filter(v012 >= 15, v012 <= 49)

required_vars <- c("v001","v024","v005", employ_vars_22, wealth_vars_22)
missing_vars <- setdiff(required_vars, names(mr_22))
if (length(missing_vars) > 0) {
  stop("Missing required variables in MR 22 file: ",
       paste(missing_vars, collapse = ", "))
}

mr_22_adm2 <- mr_22 %>%
  left_join(lookup_22, by = "v001") %>%
  select(
    v001, v024, v005,
    all_of(employ_vars_22),
    all_of(wealth_vars_22),
    ADM2_CODE, ADM2_NAME
  ) %>%
  mutate(wt = v005 / 1000000)

# ----------------------------
# 5) Derived micro variables (identical to men 2014)
# ----------------------------


mr_22_adm2 <- mr_22_adm2 %>%
  mutate(
    # ---- Employment ----
    is_working = as.integer(v714 == 1),
    
    # Agriculture occupation codes
    is_agri_occ = as.integer(v716 %in% c(61, 62, 63, 92)),
    
    occ_missing_working = as.integer(is_working == 1 & is.na(v716)),
    is_agri_working     = as.integer(is_working == 1 & is_agri_occ == 1),
    is_nonagri_working  = as.integer(is_working == 1 & !is.na(v716) & is_agri_occ == 0),
    
    # ---- Wealth ----
    wealth_q = as.integer(v190),
    wealth_s = as.numeric(v191),
    wealth_q = ifelse(wealth_q %in% 1:5, wealth_q, NA_integer_),
    
    poor40 = as.integer(wealth_q %in% c(1, 2)),
    rich40 = as.integer(wealth_q %in% c(4, 5)),
    
    # ---- Occupation buckets ----
    occ_bucket_22 = case_when(
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
    
    occ_agri_22         = as.integer(occ_bucket_22 == "agri"),
    occ_services_22     = as.integer(occ_bucket_22 == "services_sales"),
    occ_elementary_22   = as.integer(occ_bucket_22 == "elementary"),
    occ_skilled_ops_22  = as.integer(occ_bucket_22 == "skilled_ops"),
    occ_white_collar_22 = as.integer(occ_bucket_22 == "white_collar"),
    occ_other_22        = as.integer(occ_bucket_22 == "other"),
    occ_missing_22      = as.integer(occ_bucket_22 == "missing_occ")
  )

# ----------------------------
# 6) Aggregate to ADM2 outcomes (2022/23) — MEN
# ----------------------------

adm2_outcomes_men_22 <- mr_22_adm2 %>%
  filter(!is.na(ADM2_NAME), !is.na(wt)) %>%
  group_by(ADM2_CODE, ADM2_NAME) %>%
  summarise(
    # ---- Wealth composition ----
    poor40_share_men_22        = weighted.mean(poor40, wt, na.rm = TRUE),
    rich40_share_men_22        = weighted.mean(rich40, wt, na.rm = TRUE),
    mean_wealth_score_men_22   = weighted.mean(wealth_s, wt, na.rm = TRUE),
    share_wq1_men_22           = weighted.mean(as.integer(wealth_q == 1), wt, na.rm = TRUE),
    share_wq2_men_22           = weighted.mean(as.integer(wealth_q == 2), wt, na.rm = TRUE),
    share_wq3_men_22           = weighted.mean(as.integer(wealth_q == 3), wt, na.rm = TRUE),
    share_wq4_men_22           = weighted.mean(as.integer(wealth_q == 4), wt, na.rm = TRUE),
    share_wq5_men_22           = weighted.mean(as.integer(wealth_q == 5), wt, na.rm = TRUE),
    
    # ---- Core employment ----
    emp_rate_men_22            = weighted.mean(is_working, wt, na.rm = TRUE),
    agri_emp_rate_men_22       = weighted.mean(is_agri_working, wt, na.rm = TRUE),
    
    agri_share_working_men_22 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0,
             sum(wt[is_agri_working == 1], na.rm = TRUE) / w_emp,
             NA_real_)
    },
    
    nonagri_share_working_men_22 = {
      w_emp_obsocc <- sum(wt[is_working == 1 & !is.na(v716)], na.rm = TRUE)
      ifelse(w_emp_obsocc > 0,
             sum(wt[is_nonagri_working == 1], na.rm = TRUE) / w_emp_obsocc,
             NA_real_)
    },
    
    occ_missing_share_working_men_22 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0,
             sum(wt[occ_missing_working == 1], na.rm = TRUE) / w_emp,
             NA_real_)
    },
    
    # ---- Destination shares ----
    share_agri_working_men_22 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0, sum(wt[occ_agri_22 == 1], na.rm = TRUE) / w_emp, NA_real_)
    },
    share_services_sales_working_men_22 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0, sum(wt[occ_services_22 == 1], na.rm = TRUE) / w_emp, NA_real_)
    },
    share_elementary_working_men_22 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0, sum(wt[occ_elementary_22 == 1], na.rm = TRUE) / w_emp, NA_real_)
    },
    share_skilled_ops_working_men_22 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0, sum(wt[occ_skilled_ops_22 == 1], na.rm = TRUE) / w_emp, NA_real_)
    },
    share_white_collar_working_men_22 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0, sum(wt[occ_white_collar_22 == 1], na.rm = TRUE) / w_emp, NA_real_)
    },
    share_other_working_men_22 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0, sum(wt[occ_other_22 == 1], na.rm = TRUE) / w_emp, NA_real_)
    },
    share_missing_occ_working_men_22 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0, sum(wt[occ_missing_22 == 1], na.rm = TRUE) / w_emp, NA_real_)
    },
    
    n_men_22 = sum(!is.na(is_working)),
    .groups = "drop"
  )

# ----------------------------
# 7) Quick checks
# ----------------------------

adm2_outcomes_men_22 %>%
  summarise(
    min_emp = min(emp_rate_men_22, na.rm = TRUE),
    max_emp = max(emp_rate_men_22, na.rm = TRUE),
    min_agri_share = min(agri_share_working_men_22, na.rm = TRUE),
    max_agri_share = max(agri_share_working_men_22, na.rm = TRUE),
    min_sum_dest = min(
      share_agri_working_men_22 +
        share_services_sales_working_men_22 +
        share_elementary_working_men_22 +
        share_skilled_ops_working_men_22 +
        share_white_collar_working_men_22 +
        share_other_working_men_22 +
        share_missing_occ_working_men_22,
      na.rm = TRUE
    ),
    max_sum_dest = max(
      share_agri_working_men_22 +
        share_services_sales_working_men_22 +
        share_elementary_working_men_22 +
        share_skilled_ops_working_men_22 +
        share_white_collar_working_men_22 +
        share_other_working_men_22 +
        share_missing_occ_working_men_22,
      na.rm = TRUE
    )
  )

summary(adm2_outcomes_men_22$occ_missing_share_working_men_22)

# ----------------------------
# 8) Join outcomes onto polygons (optional)
# ----------------------------
