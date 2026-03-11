# ================================================================
# Ghana ADM2 interactive maps
# - basic_interactive: blue ADM2 boundaries
# - int_cocoa: cocoa hectares

## original code but now redundant use cocoa mapping will delete soon 
# notes Atiwa west needs coding for 4 and attin fosu needs coding for 2 in CSSVD intensity they got dropped in the merge 
# ================================================================
source("Code/R/packages and functions.R")

here()

#directory paths 
geog_data_dir <- ("Data/Data_Raw/Geographic")
output_figs_dir <- ("Outputs/figures")
# ---------------------------
# 1) file Paths
# ---------------------------

adm2_path <- file.path(geog_data_dir, "geoBoundaries-GHA-ADM2_simplified.geojson")
cocoa_path <- file.path(geog_data_dir, "ghana-cocoa-area-2020.csv")
cssvd_intensity_path <- file.path(geog_data_dir, "spatial-metrics-ghana-cocoa-cocoa_area_district.csv")
# ---------------------------
# 2) Load data
# ---------------------------
adm2 <- st_read(adm2_path, quiet = TRUE)

ghana_cocoa_area_2020 <- read_csv(cocoa_path, show_col_types = FALSE)

cssvd_intensity_df <- read_csv(cssvd_intensity_path, show_col_types = FALSE)

cssvd_df <- cssvd_intensity_df |> 
  

cocoa_df <- ghana_cocoa_area_2020 |>
  mutate(region = toTitleCase(tolower(region))) |>
  rename(hectares = numerical_value) |>
  select(region, hectares)

# ---------------------------
# 3) Cleaning function for joins
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
# 4) Basic interactive map (blue)
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
# 5) Join cocoa hectares to ADM2
# ---------------------------
adm2_leaf2 <- adm2_leaf %>%
  mutate(key = clean_admin_name(shapeName))

cocoa_df2 <- cocoa_df %>%
  mutate(key = clean_admin_name(region))

joined <- adm2_leaf2 %>%
  left_join(cocoa_df2, by = "key") %>%
  filter(!is.na(region)) %>%
  transmute(
    shapeName,
    shapeID,
    hectares = readr::parse_number(as.character(hectares))
  )

# ---------------------------
# 6) Cocoa interactive map (YlOrRd bins + legend)
# ---------------------------
bins <- c(0, 1000, 5000, 10000, 25000, 40000, 70000, 100000, Inf)

pal <- colorBin(
  palette = "YlOrRd",
  domain  = joined$hectares,
  bins    = bins,
  na.color = "#d9d9d9"
)

int_cocoa <- leaflet(joined) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    color = "white",
    weight = 0.6,
    opacity = 1,
    fillColor = ~pal(hectares),
    fillOpacity = 0.75,
    label = ~ifelse(
      is.na(hectares),
      paste0(shapeName, ": no data"),
      paste0(shapeName, ": ", comma(hectares), " ha")
    ),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#FF8800",
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal = pal,
    values = ~hectares,
    title = "Cocoa area (hectares)",
    opacity = 0.75,
    labFormat = labelFormat(big.mark = ",", digits = 0)
  )

## save map 

saveWidget(
  int_cocoa,
  file = file.path( output_figs_dir, "cocoa_area_interactive.html"),
  selfcontained = TRUE
)

# ---------------------------
# 7) Print maps (optional)
# ---------------------------
basic_interactive
int_cocoa