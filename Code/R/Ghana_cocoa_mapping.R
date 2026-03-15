source("Code/R/packages and functions.R")

here()
# notes Atiwa west needs coding for 4 and attin fosu needs coding for 2 in CSSVD intensity they got dropped in the merge 

#directory paths 
geog_data_dir  <- "Data/Data_Raw/Geographic"
output_figs_dir <- "Outputs/figures"
dir.create(output_figs_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# 1) File Paths
# ---------------------------
adm2_path <- file.path(geog_data_dir, "geoBoundaries-GHA-ADM2_simplified.geojson")

# NEW: Use the spatial metrics file (contains hectares + CSSVD_intensity_2014)
cssvd_intensity_path <- file.path(geog_data_dir, "spatial-metrics-ghana-cocoa-cocoa_area_district.csv")

# ---------------------------
# 2) Load data
# ---------------------------
adm2 <- st_read(adm2_path, quiet = TRUE)

cssvd_intensity_df <- read_csv(cssvd_intensity_path, show_col_types = FALSE)

# This is now your main district-level dataset
# Assume it includes:
# - region (district name)
# - numerical_value (cocoa hectares)
# - CSSVD_intensity_2014 (1..6 with NA)
cssvd_df <- cssvd_intensity_df |>
  mutate(
    region = toTitleCase(tolower(region)),
    hectares = cocoa_area_hectares,
    CSSVD_intensity_2014 = ifelse(is.na(CSSVD_intensity_2014), 0, CSSVD_intensity_2014),
    CSSVD_intensity_2014 = as.integer(CSSVD_intensity_2014)
  ) |>
  select(region, hectares, CSSVD_intensity_2014)

# ---------------------------
# 3) Cleaning function for joins (unchanged)
# ---------------------------
clean_admin_name <- function(x) {
  x %>%
    str_to_upper() %>%
    str_replace_all("\\bMUNIC?I?PAL\\b", "") %>%
    str_replace_all("\\bMUNICIPALITY\\b", "") %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_squish() %>%
    str_replace_all("\\bMETROPOLITAN\\b", "") %>%
    str_replace_all("\\bDISTRICT\\b", "") %>%
    str_replace_all("\\bREGION\\b", "") %>%
    str_replace_all("\\bASSEMBLY\\b", "") %>%
    str_replace_all("\\bAREA\\b", "") %>%
    str_replace_all("\\bNORTH\\s+MUNIC\\b", "NORTH") %>%
    str_squish()
}

# ---------------------------
# 4) Basic interactive map (blue) (unchanged)
# ---------------------------
adm2_leaf <- adm2 %>%
  select(shapeName, shapeID)

basic_interactive <- leaflet(adm2_leaf) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    color = "#444444",
    weight = 1,
    fillColor = "#2b8cbe",
    fillOpacity = 0.2,
    label = ~shapeName,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#FF8800",
      bringToFront = TRUE
    )
  )

# ---------------------------
# 5) Join cocoa hectares + intensity to ADM2 (single join)
# ---------------------------
adm2_leaf2 <- adm2_leaf %>%
  mutate(key = clean_admin_name(shapeName))

cssvd_df2 <- cssvd_df %>%
  mutate(key = clean_admin_name(region))

joined_all <- adm2_leaf2 %>%
  left_join(cssvd_df2, by = "key") %>%
  # keep only matched cocoa districts (as before); remove this filter if you want all ADM2 shown
  filter(!is.na(region)) %>%
  transmute(
    shapeName,
    shapeID,
    hectares = readr::parse_number(as.character(hectares)),
    CSSVD_intensity_2014 = as.integer(CSSVD_intensity_2014)
  )
