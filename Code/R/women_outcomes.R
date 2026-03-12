## start with 2014 

library(haven)
GHIR72FL <- read_dta("Data/Data_Raw/DHS/GH_2014_DHS_Standard/GHIR72FL.DTA")
View(GHIR72FL)

## vars of interest employment 
# v741 - type of earning 
# v716 - respondent's occupation 
# v714 - respondent  currently working 
# v732 - respondent employed all year / seasonal 
# v731 - respondent worked in last 12 months 
employ_vars <- c("v741" , "v716", "v714", "v732", "v731")
## vars of interest control over earning 
# v739 - person who usually decides how to spend earnings ( 62% miss)

## agency vars 
#v743a - pwud health care (42% miss)
#v743b - pwud large hh purchases (42% miss)
#v743c - pwud daily hh purchases 100% miss 
#v743d - pwud on visits to family and friends (42% miss)
intra_hh_vars <- c("v739","v743a","v743b","v743d")



# file paths
ge_path  <- "Data/Data_Raw/DHS/GH_2014_DHS_GEOG/GHGE71FL/GHGE71FL.shp"          # GPS cluster file
ir_path  <- "Data/Data_Raw/DHS/GH_2014_DHS_Standard/GHIR72FL.DTA"          # Women IR file (example name)
adm2_path <- "Data/Shapefiles/GHA_adm2.geojson"  # Your ADM2 polygons

ge <- st_read(ge_path)

# Inspect names to confirm
names(ge)
st_crs(ge)
# Create sf points (adjust column names if needed)
ge_sf <- st_as_sf(
  ge,
  coords = c("LONGNUM", "LATNUM"),
  crs = 4326,
  remove = FALSE
)

### Ghana ADM2 polygon file 
adm2 <- st_read("Data/Data_Raw/Geographic/geoBoundaries-GHA-ADM2_simplified.geojson")
st_crs(adm2)

## ensures that we are using the same coordinate system 
adm2_test <- st_transform(adm2, st_crs(ge))


# 1. Read the women’s IR file
ir <- read_dta(ir_path)

# 2. Make sure ADM2 polygons use the same CRS as DHS clusters
adm2_test <- st_transform(adm2_test, st_crs(ge))

# 3. Spatial join: assign each DHS cluster to an ADM2 // using sf_join we join each point with the polygon of the json file
# (replace ADM2_NAME / ADM2_CODE with the actual column names in your ADM2 file)
clusters_adm2 <- st_join(
  ge,
  adm2_test %>% select(ADM2_NAME = shapeName, ADM2_CODE = shapeID),
  join = st_within
)

# 4. Create cluster → ADM2 lookup
lookup <- clusters_adm2 %>%
  st_drop_geometry() %>%
  transmute(
    v001 = DHSCLUST,
    ADM2_CODE,
    ADM2_NAME
  )

# 5. Merge ADM2 onto women using cluster ID
ir_adm2 <- ir %>%
  left_join(lookup, by = "v001")


## create the 2014 df for women, note we are adding in sampling weights 

ir_14_adm2 <- ir_adm2 |>
  select(v001, v024, v005,
         all_of(employ_vars),
         all_of(intra_hh_vars),
         ADM2_CODE, ADM2_NAME) |>
  mutate(wt = v005 / 1000000)

## now to recode the vars then to aggregate 
##lets firstly look into where agri work is 

ir_14_adm2 <- ir_14_adm2 %>%
  mutate(
    occ_group = case_when(
      v716 == 0 ~ "Not working",
      v716 == 1 ~ "Armed forces",
      
      v716 %in% 11:14 ~ "Managers",
      
      v716 %in% 21:26 | v716 %in% 31:34 ~ "Professionals / Associate prof.",
      
      v716 %in% 41:43 ~ "Clerical",
      
      v716 %in% 51:54 | v716 == 52 | v716 == 95 ~ "Services & sales",
      
      v716 %in% 61:63 | v716 == 92 ~ "Agriculture",
      
      v716 %in% 71:75 | v716 %in% 81:82 ~ "Skilled trades / Operators",
      
      v716 %in% c(91, 93, 94, 96) ~ "Elementary",
      
      TRUE ~ NA_character_
    )
  )



ir_14_adm2 <- ir_14_adm2 %>%
  mutate(
    occ_3 = case_when(
      v716 == 0 ~ "Not working",
      v716 %in% c(61,62,63,92) ~ "Agriculture",
      !is.na(v716) ~ "Non-agriculture",
      TRUE ~ NA_character_
    )
  )

adm2_occ_shares_w <- ir_14_adm2 %>%
  filter(!is.na(occ_group)) %>%
  group_by(ADM2_CODE, ADM2_NAME, occ_group) %>%
  summarise(w = sum(wt, na.rm = TRUE), .groups = "drop") %>%
  group_by(ADM2_CODE, ADM2_NAME) %>%
  mutate(share = w / sum(w))

## visualise this


adm2_agri_share <- adm2_occ_shares_w %>%
  filter(occ_group == "Agriculture") %>%
  select(ADM2_CODE, ADM2_NAME, agri_share = share)

adm2_map <- adm2_test %>%
  mutate(ADM2_CODE = shapeID, ADM2_NAME = shapeName) %>%
  left_join(adm2_agri_share, by = c("ADM2_CODE", "ADM2_NAME"))

library(ggplot2)
library(scales)


p_agri_bins <- ggplot(adm2_map) +
  geom_sf(aes(fill = agri_share), color = "white", linewidth = 0.2) +
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

p_agri_bins
ggsave("Outputs/figures/p_agri_bins.png", p_agri_bins, width = 9, height = 7, dpi = 300)




