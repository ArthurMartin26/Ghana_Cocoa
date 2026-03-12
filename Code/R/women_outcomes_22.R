## DHS 2022/23: Women IR (GHIR8CFL) + GPS clusters (GHGE8AFL) -> ADM2 -> agri share map
source("Code/R/packages and functions.R")

# ----------------------------
# Variables of interest (IR)
# ----------------------------

employ_vars_22 <- c("v741", "v716", "v714", "v732", "v731")
intra_hh_vars_22 <- c("v739", "v743a", "v743b", "v743d")

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
    ADM2_CODE, ADM2_NAME
  ) %>%
  mutate(wt = v005 / 1000000)

# ----------------------------
# Occupation recodes (v716)
# ----------------------------

ir_22_adm2 <- ir_22_adm2 %>%
  mutate(
    occ_group_22 = case_when(
      v716 == 0 ~ "Not working",
      v716 == 1 ~ "Armed forces",
      v716 %in% 11:14 ~ "Managers",
      v716 %in% 21:26 | v716 %in% 31:34 ~ "Professionals / Associate prof.",
      v716 %in% 41:43 ~ "Clerical",
      v716 %in% c(51, 52, 53, 54, 95) ~ "Services & sales",
      v716 %in% c(61, 62, 63, 92) ~ "Agriculture",
      v716 %in% 71:75 | v716 %in% 81:82 ~ "Skilled trades / Operators",
      v716 %in% c(91, 93, 94, 96) ~ "Elementary",
      TRUE ~ NA_character_
    ),
    occ_3_22 = case_when(
      v716 == 0 ~ "Not working",
      v716 %in% c(61, 62, 63, 92) ~ "Agriculture",
      !is.na(v716) ~ "Non-agriculture",
      TRUE ~ NA_character_
    )
  )

# ----------------------------
# Weighted ADM2 share in agriculture
# ----------------------------

adm2_occ_shares_w_22 <- ir_22_adm2 %>%
  filter(!is.na(occ_group_22)) %>%
  group_by(ADM2_CODE, ADM2_NAME, occ_group_22) %>%
  summarise(w = sum(wt, na.rm = TRUE), .groups = "drop") %>%
  group_by(ADM2_CODE, ADM2_NAME) %>%
  mutate(share = w / sum(w))

adm2_agri_share_22 <- adm2_occ_shares_w_22 %>%
  filter(occ_group_22 == "Agriculture") %>%
  select(ADM2_CODE, ADM2_NAME, agri_share_22 = share)

adm2_map_22 <- adm2_22 %>%
  mutate(ADM2_CODE = shapeID, ADM2_NAME = shapeName) %>%
  left_join(adm2_agri_share_22, by = c("ADM2_CODE", "ADM2_NAME"))

# ----------------------------
# Plot and save
# ----------------------------

p_agri_bins_22 <- ggplot(adm2_map_22) +
  geom_sf(aes(fill = agri_share_22), color = "white", linewidth = 0.2) +
  scale_fill_stepsn(
    colours = c("#edf8e9", "#c7e9c0", "#74c476", "#238b45", "#005a32"),
    breaks = c(0, .10, .25, .40, .60, 1),
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    name = "Share in\nagriculture"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid = element_blank()
  ) +
  labs(
    title = "Women in agriculture (ADM2, DHS 2022/23)",
    subtitle = "Binned weighted shares; darker green = higher agriculture share",
    caption = "Occupation from v716; weights from v005"
  )

p_agri_bins_22
##ggsave("Outputs/figures/p_agri_bins_22.png", p_agri_bins_22, width = 9, height = 7, dpi = 300)