
library(haven)     
library(dplyr)
library(tidyr)
library(stringr)
library(labelled)
library(purrr)
library(survey) 
library(readr)
library(haven)
library(Hmisc)
library(skimr)
library(ggplot2)
library(fixest)
library(broom)
library(sf)
library(leaflet)
library(tools)
library(scales)
library(here)
library(htmlwidgets)


## cleanign df names :

clean_admin_name <- function(x) {
  x %>%
    str_to_upper() %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_squish() %>%
    str_replace_all("\\bMUNIC?I?PAL\\b", "") %>%
    str_replace_all("\\bMUNICIPALITY\\b", "") %>%
    str_replace_all("\\bMETROPOLITAN\\b", "") %>%
    str_replace_all("\\bDISTRICT\\b", "") %>%
    str_replace_all("\\bREGION\\b", "") %>%
    str_replace_all("\\bASSEMBLY\\b", "") %>%
    str_replace_all("\\bAREA\\b", "") %>%
    str_replace_all("\\bNORTH\\s+MUNIC\\b", "NORTH") %>%
    str_squish()
}
