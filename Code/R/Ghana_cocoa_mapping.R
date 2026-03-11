source("Code/R/packages and functions.R")

here()

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

# ---------------------------
# 6) Cocoa interactive map (same bins + legend)
# ---------------------------
bins <- c(0, 1000, 5000, 10000, 25000, 40000, 70000, 100000, Inf)

pal_cocoa <- colorBin(
  palette = "YlOrRd",
  domain  = joined_all$hectares,
  bins    = bins,
  na.color = "#d9d9d9"
)

int_cocoa <- leaflet(joined_all) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    color = "white",
    weight = 0.6,
    opacity = 1,
    fillColor = ~pal_cocoa(hectares),
    fillOpacity = 0.75,
    label = ~ifelse(
      is.na(hectares),
      paste0(shapeName, ": no data"),
      paste0(shapeName, ": ", scales::comma(hectares), " ha")
    ),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#FF8800",
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal = pal_cocoa,
    values = ~hectares,
    title = "Cocoa area (hectares)",
    opacity = 0.75,
    labFormat = labelFormat(big.mark = ",", digits = 0)
  )

# ---------------------------
# 7) NEW: CSSVD intensity interactive map (0..6; NA treated as 0)
#     Colour mapping: 1 white, 2 green, 3 yellow, 4 pink, 5 light red, 6 dark red
#     plus 0 = grey (no data)
# ---------------------------
# Sequential blue palette for CSSVD intensity (0–6)
intensity_levels <- 0:6

intensity_cols <- c(
  "0" = "#eff3ff",  # very light (no data / zero)
  "1" = "#deebf7",
  "2" = "#c6dbef",
  "3" = "#9ecae1",
  "4" = "#6baed6",
  "5" = "#3182bd",
  "6" = "#08519c"   # darkest (highest intensity)
)

pal_intensity <- colorFactor(
  palette  = intensity_cols[as.character(intensity_levels)],
  domain   = intensity_levels,
  ordered  = TRUE,
  na.color = intensity_cols[["0"]]
)
int_cssvd <- leaflet(joined_all) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    color = "white",
    weight = 0.6,
    opacity = 1,
    fillColor = ~pal_intensity(CSSVD_intensity_2014),
    fillOpacity = 0.75,
    label = ~paste0(shapeName, ": CSSVD intensity = ", ifelse(is.na(CSSVD_intensity_2014), 0, CSSVD_intensity_2014)),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#FF8800",
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal = pal_intensity,
    values = intensity_levels,
    title = "CSSVD intensity (2014)",
    opacity = 0.75,
    labFormat = labelFormat(digits = 0)
  )

int_cssvd
int_cocoa
# ---------------------------
# 8) Save maps (HTML)
# ---------------------------
htmlwidgets::saveWidget(
  basic_interactive,
  file = file.path(output_figs_dir, "adm2_basic_interactive.html"),
  selfcontained = TRUE
)

htmlwidgets::saveWidget(
  int_cocoa,
  file = file.path(output_figs_dir, "cocoa_area_interactive.html"),
  selfcontained = TRUE
)

htmlwidgets::saveWidget(
  int_cssvd,
  file = file.path(output_figs_dir, "cssvd_intensity_interactive.html"),
  selfcontained = TRUE
)


#########################
## save as PNG images ###
#########################

# --- Cocoa bins (same as leaflet) ---
bins <- c(0, 1000, 5000, 10000, 25000, 40000, 70000, 100000, Inf)
bin_labels <- c("0–1k", "1–5k", "5–10k", "10–25k", "25–40k", "40–70k", "70–100k", "100k+")

joined_all$cocoa_bin <- cut(
  joined_all$hectares,
  breaks = bins,
  include.lowest = TRUE,
  right = FALSE,
  labels = bin_labels
)

p_cocoa <- ggplot(joined_all) +
  geom_sf(aes(fill = cocoa_bin), color = "white", linewidth = 0.2) +
  scale_fill_brewer(
    palette = "YlOrRd",
    na.value = "#d9d9d9",
    name = "Cocoa area (ha)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid = element_blank()
  ) +
  labs(
    title = "Ghana cocoa area by district",
    subtitle = "Bins in hectares",
    caption = "Source: Trase cocoa area (district aggregation)"
  )

ggsave(
  filename = file.path(output_figs_dir, "cocoa_area_static.png"),
  plot = p_cocoa,
  width = 10, height = 8, dpi = 300
)

############################
## now for intensity #######
############################

# Ensure NA treated as 0
joined_all$CSSVD_intensity_2014 <- ifelse(
  is.na(joined_all$CSSVD_intensity_2014), 0L, as.integer(joined_all$CSSVD_intensity_2014)
)

# Set as factor so legend stays in order
joined_all$CSSVD_intensity_2014 <- factor(joined_all$CSSVD_intensity_2014, levels = 0:6)

# Blue sequential palette (0..6)
intensity_cols <- c(
  "0" = "#eff3ff",
  "1" = "#deebf7",
  "2" = "#c6dbef",
  "3" = "#9ecae1",
  "4" = "#6baed6",
  "5" = "#3182bd",
  "6" = "#08519c"
)

p_cssvd <- ggplot(joined_all) +
  geom_sf(aes(fill = CSSVD_intensity_2014), color = "white", linewidth = 0.2) +
  scale_fill_manual(
    values = intensity_cols,
    drop = FALSE,
    name = "CSSVD intensity\n(2014)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid = element_blank()
  ) +
  labs(
    title = "CSSVD outbreak intensity (2014)",
    subtitle = "0 = no data / not coded; 6 = highest intensity",
    caption = "Intensity constructed from baseline outbreak map categories"
  )

ggsave(
  filename = file.path(output_figs_dir, "cssvd_intensity_static.png"),
  plot = p_cssvd,
  width = 10, height = 8, dpi = 300
)