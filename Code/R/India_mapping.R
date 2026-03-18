library(sf)
library(leaflet)
library(dplyr)

india_path <- file.path(geog_data_dir, "gadm41_IND_2.json")



india <- st_read(india_path, quiet = TRUE)


plot(st_geometry(india),
     col = "grey90",
     border = "grey40",
     main = "India – Sub-district Boundaries")

india_leaf <- india %>%
  select(
    subdistrict = NAME_2  )

india_interactive <- leaflet(india_leaf) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    color = "#444444",
    weight = 1,
    fillColor = "#2b8cbe",
    fillOpacity = 0.25,
    label = ~subdistrict,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#FF8800",
      bringToFront = TRUE
    )
  )

india_interactive