## DHS 2014: Women IR (GHIR72FL) + GPS clusters (GHGE71FL) -> ADM2 -> agri share map

source("Code/R/packages and functions.R")

# ----------------------------
# Variables of interest (IR)
# ----------------------------

# Employment-related variables
employ_vars_14 <- c("v741", "v716", "v714", "v732", "v731")

# Intra-household / agency variables
intra_hh_vars_14 <- c("v739", "v743a", "v743b", "v743d")

# ----------------------------
# File paths (2014)
# ----------------------------

ge_path_14   <- "Data/Data_Raw/DHS/GH_2014_DHS_GEOG/GHGE71FL/GHGE71FL.shp"
ir_path_14   <- "Data/Data_Raw/DHS/GH_2014_DHS_Standard/GHIR72FL.DTA"
adm2_path <- "Data/Data_Raw/Geographic/geoBoundaries-GHA-ADM2_simplified.geojson"

# ----------------------------
# Read spatial data
# ----------------------------

ge_14   <- st_read(ge_path_14)
adm2_14 <- st_read(adm2_path_14)

# Ensure same CRS
adm2_14 <- st_transform(adm2_14, st_crs(ge_14))

# Spatial join: assign each DHS cluster to an ADM2 polygon
clusters_adm2_14 <- st_join(
  ge_14,
  adm2_14 %>% select(ADM2_NAME = shapeName, ADM2_CODE = shapeID),
  join = st_within
)

## clusters
lookup_14 <- clusters_adm2_14 %>%
  st_drop_geometry() %>%
  transmute(v001 = DHSCLUST, ADM2_CODE, ADM2_NAME)

# ----------------------------
# Read IR and merge ADM2
# ----------------------------

ir_14 <- read_dta(ir_path_14)

ir_14_adm2 <- ir_14 %>%
  left_join(lookup_14, by = "v001") %>%
  select(
    v001, v024, v005,
    all_of(employ_vars_14),
    all_of(intra_hh_vars_14),
    ADM2_CODE, ADM2_NAME
  ) %>%
  mutate(wt = v005 / 1000000)

# ----------------------------
# Occupation recodes (v716)
# ----------------------------

ir_14_adm2 <- ir_14_adm2 %>%
  mutate(
    occ_group_14 = case_when(
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
    occ_3_14 = case_when(
      v716 == 0 ~ "Not working",
      v716 %in% c(61, 62, 63, 92) ~ "Agriculture",
      !is.na(v716) ~ "Non-agriculture",
      TRUE ~ NA_character_
    )
  )

# ----------------------------
# Weighted ADM2 share in agriculture
# ----------------------------

adm2_occ_shares_w_14 <- ir_14_adm2 %>%
  filter(!is.na(occ_group_14)) %>%
  group_by(ADM2_CODE, ADM2_NAME, occ_group_14) %>%
  summarise(w = sum(wt, na.rm = TRUE), .groups = "drop") %>%
  group_by(ADM2_CODE, ADM2_NAME) %>%
  mutate(share = w / sum(w))

adm2_agri_share_14 <- adm2_occ_shares_w_14 %>%
  filter(occ_group_14 == "Agriculture") %>%
  select(ADM2_CODE, ADM2_NAME, agri_share_14 = share)

# Join agri share onto polygons for mapping
adm2_map_14 <- adm2_14 %>%
  mutate(ADM2_CODE = shapeID, ADM2_NAME = shapeName) %>%
  left_join(adm2_agri_share_14, by = c("ADM2_CODE", "ADM2_NAME"))

# ----------------------------
# Plot and save
# ----------------------------

p_agri_bins_14 <- ggplot(adm2_map_14) +
  geom_sf(aes(fill = agri_share_14), color = "white", linewidth = 0.2) +
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
    title = "Women in agriculture (ADM2, DHS 2014)",
    subtitle = "Binned weighted shares; darker green = higher agriculture share",
    caption = "Occupation from v716; weights from v005"
  )

p_agri_bins_14
#ggsave("Outputs/figures/p_agri_bins_14.png", p_agri_bins_14, width = 9, height = 7, dpi = 300)