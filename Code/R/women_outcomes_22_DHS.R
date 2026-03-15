## DHS 2022/23: Women IR (GHIR8CFL) + GPS clusters (GHGE8AFL) -> ADM2 -> agri share map
source("Code/R/packages and functions.R")

# ----------------------------
# Variables of interest (IR)
# ----------------------------

employ_vars_22 <- c("v741", "v716", "v714", "v732", "v731")
intra_hh_vars_22 <- c("v739", "v743a", "v743b", "v743d")
wealth_vars_22 <- c("v190", "v191")

# ----------------------------
# File paths (2022/23)
# ----------------------------

ge_path_22   <- "Data/Data_Raw/DHS/GH_2022_DHS_GEOG/GHGE8AFL/GHGE8AFL.shp"
ir_path_22   <- "Data/Data_Raw/DHS/GH_2022_DHS_Standard/GHIR8CFL.DTA"
adm2_path <- "Data/Data_Raw/Geographic/geoBoundaries-GHA-ADM2_simplified.geojson"

# ----------------------------
# Read spatial data
# ----------------------------

ge_22   <- st_read(ge_path_22)
adm2_22 <- st_read(adm2_path)

adm2_22 <- st_transform(adm2_22, st_crs(ge_22))

clusters_adm2_22 <- st_join(
  ge_22,
  adm2_22 %>% select(ADM2_NAME = shapeName, ADM2_CODE = shapeID),
  join = st_within
)

lookup_22 <- clusters_adm2_22 %>%
  st_drop_geometry() %>%
  transmute(v001 = DHSCLUST, ADM2_CODE, ADM2_NAME)

# ----------------------------
# Read IR and merge ADM2
# ----------------------------

ir_22 <- read_dta(ir_path_22)

ir_22_adm2 <- ir_22 %>%
  left_join(lookup_22, by = "v001") %>%
  select(
    v001, v024, v005,
    all_of(employ_vars_22),
    all_of(intra_hh_vars_22),
    all_of(wealth_vars_22),
    ADM2_CODE, ADM2_NAME
  ) %>%
  mutate(wt = v005 / 1000000)

# ----------------------------
# 5) Derived micro variables (work, occupation buckets, wealth, agency)
# ----------------------------

ir_22_adm2 <- ir_22_adm2 %>%
  mutate(
    # ---- Employment ----
    is_working = as.integer(v714 == 1),
    
    # Agriculture occupation codes (your ISCO-style grouping)
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
    
    # ---- Intra-household agency: v739 ----
    v739_num = as.integer(v739),
    
    has_agency_v739 = as.integer(v739_num %in% c(1, 2)),
    low_agency_v739 = as.integer(v739_num %in% c(4, 5)),
    
    v739_resp_alone   = as.integer(v739_num == 1),
    v739_joint        = as.integer(v739_num == 2),
    v739_husb_alone   = as.integer(v739_num == 4),
    v739_someone_else = as.integer(v739_num == 5),
    
    # ---- Occupation "destination" buckets (among working women) ----
    occ_bucket_22 = case_when(
      is_working == 0               ~ NA_character_,   # not in denominator for "among working" shares
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
    occ_agri_22         = as.integer(occ_bucket_22 == "agri"),
    occ_services_22     = as.integer(occ_bucket_22 == "services_sales"),
    occ_elementary_22   = as.integer(occ_bucket_22 == "elementary"),
    occ_skilled_ops_22  = as.integer(occ_bucket_22 == "skilled_ops"),
    occ_white_collar_22 = as.integer(occ_bucket_22 == "white_collar"),
    occ_other_22        = as.integer(occ_bucket_22 == "other"),
    occ_missing_22      = as.integer(occ_bucket_22 == "missing_occ")
  )

# ----------------------------
# 6) Aggregate to ADM2 outcomes (2022)
# ----------------------------

adm2_outcomes_22 <- ir_22_adm2 %>%
  filter(!is.na(ADM2_NAME), !is.na(wt)) %>%
  group_by(ADM2_CODE, ADM2_NAME) %>%
  summarise(
    # ---- Wealth composition ----
    poor40_share_22        = weighted.mean(poor40, wt, na.rm = TRUE),
    rich40_share_22        = weighted.mean(rich40, wt, na.rm = TRUE),
    mean_wealth_score_22   = weighted.mean(wealth_s, wt, na.rm = TRUE),
    share_wq1_22           = weighted.mean(as.integer(wealth_q == 1), wt, na.rm = TRUE),
    share_wq2_22           = weighted.mean(as.integer(wealth_q == 2), wt, na.rm = TRUE),
    share_wq3_22           = weighted.mean(as.integer(wealth_q == 3), wt, na.rm = TRUE),
    share_wq4_22           = weighted.mean(as.integer(wealth_q == 4), wt, na.rm = TRUE),
    share_wq5_22           = weighted.mean(as.integer(wealth_q == 5), wt, na.rm = TRUE),
    
    # ---- Core employment ----
    emp_rate_22            = weighted.mean(is_working, wt, na.rm = TRUE),
    agri_emp_rate_22       = weighted.mean(is_agri_working, wt, na.rm = TRUE),
    
    # Share in agriculture among employed women
    agri_share_working_22 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0,
             sum(wt[is_agri_working == 1], na.rm = TRUE) / w_emp,
             NA_real_)
    },
    
    # Share in non-agri among employed women (occupation observed)
    nonagri_share_working_22 = {
      w_emp_obsocc <- sum(wt[is_working == 1 & !is.na(v716)], na.rm = TRUE)
      ifelse(w_emp_obsocc > 0,
             sum(wt[is_nonagri_working == 1], na.rm = TRUE) / w_emp_obsocc,
             NA_real_)
    },
    
    # % of working women missing occupation
    occ_missing_share_working_22 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0,
             sum(wt[occ_missing_working == 1], na.rm = TRUE) / w_emp,
             NA_real_)
    },
    
    # ---- Destination shares among employed women (denom = all employed) ----
    share_agri_working_22 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0, sum(wt[occ_agri_22 == 1], na.rm = TRUE) / w_emp, NA_real_)
    },
    share_services_sales_working_22 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0, sum(wt[occ_services_22 == 1], na.rm = TRUE) / w_emp, NA_real_)
    },
    share_elementary_working_22 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0, sum(wt[occ_elementary_22 == 1], na.rm = TRUE) / w_emp, NA_real_)
    },
    share_skilled_ops_working_22 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0, sum(wt[occ_skilled_ops_22 == 1], na.rm = TRUE) / w_emp, NA_real_)
    },
    share_white_collar_working_22 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0, sum(wt[occ_white_collar_22 == 1], na.rm = TRUE) / w_emp, NA_real_)
    },
    share_other_working_22 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0, sum(wt[occ_other_22 == 1], na.rm = TRUE) / w_emp, NA_real_)
    },
    share_missing_occ_working_22 = {
      w_emp <- sum(wt[is_working == 1], na.rm = TRUE)
      ifelse(w_emp > 0, sum(wt[occ_missing_22 == 1], na.rm = TRUE) / w_emp, NA_real_)
    },
    
    # ---- Agency / norms proxies (v739) ----
    low_agency_v739_share_22      = weighted.mean(low_agency_v739, wt, na.rm = TRUE),
    v739_joint_share_22           = weighted.mean(v739_joint, wt, na.rm = TRUE),
    v739_husb_alone_share_22      = weighted.mean(v739_husb_alone, wt, na.rm = TRUE),
    v739_someone_else_share_22    = weighted.mean(v739_someone_else, wt, na.rm = TRUE),
    n_v739_nonmiss_22             = sum(!is.na(v739_num)),
    
    # ---- Counts ----
    n_women_22 = sum(!is.na(is_working)),
    .groups = "drop"
  )

# ----------------------------
# 7) Quick checks
# ----------------------------

adm2_outcomes_22 %>%
  summarise(
    min_emp = min(emp_rate_22, na.rm = TRUE),
    max_emp = max(emp_rate_22, na.rm = TRUE),
    min_agri_share = min(agri_share_working_22, na.rm = TRUE),
    max_agri_share = max(agri_share_working_22, na.rm = TRUE),
    min_sum_dest = min(
      share_agri_working_22 +
        share_services_sales_working_22 +
        share_elementary_working_22 +
        share_skilled_ops_working_22 +
        share_white_collar_working_22 +
        share_other_working_22 +
        share_missing_occ_working_22,
      na.rm = TRUE
    ),
    max_sum_dest = max(
      share_agri_working_22 +
        share_services_sales_working_22 +
        share_elementary_working_22 +
        share_skilled_ops_working_22 +
        share_white_collar_working_22 +
        share_other_working_22 +
        share_missing_occ_working_22,
      na.rm = TRUE
    )
  )

summary(adm2_outcomes_22$occ_missing_share_working_22)

# ----------------------------
# 8) Join outcomes onto polygons for mapping (optional)
# ----------------------------

adm2_map_22 <- adm2_22 %>%
  mutate(ADM2_CODE = shapeID, ADM2_NAME = shapeName) %>%
  left_join(adm2_outcomes_22, by = c("ADM2_CODE", "ADM2_NAME")) # ----------------------------
# Plot and save
# ----------------------------
# 
# p_agri_bins_22 <- ggplot(adm2_map_22) +
#   geom_sf(aes(fill = agri_share_22), color = "white", linewidth = 0.2) +
#   scale_fill_stepsn(
#     colours = c("#edf8e9", "#c7e9c0", "#74c476", "#238b45", "#005a32"),
#     breaks = c(0, .10, .25, .40, .60, 1),
#     labels = percent_format(accuracy = 1),
#     limits = c(0, 1),
#     name = "Share in\nagriculture"
#   ) +
#   theme_minimal(base_size = 12) +
#   theme(
#     legend.position = "right",
#     panel.grid = element_blank()
#   ) +
#   labs(
#     title = "Women in agriculture (ADM2, DHS 2022/23)",
#     subtitle = "Binned weighted shares; darker green = higher agriculture share",
#     caption = "Occupation from v716; weights from v005"
#   )
# 
# p_agri_bins_22
# ##ggsave("Outputs/figures/p_agri_bins_22.png", p_agri_bins_22, width = 9, height = 7, dpi = 300)