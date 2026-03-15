
data_dir <- "Data/Data_Output"
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

library(haven)

wm <- read_sav("Data/Data_Raw/MICS/Ghana_MICS4_Datasets/Ghana MICS 2011 SPSS Datasets/wm.sav")

# Remove SPSS value labels *only* from the problematic variables
wm$MN22 <- zap_labels(wm$MN22)
wm$TN6A <- zap_labels(wm$TN6A)

# Write to .dta
out_path <- file.path(data_dir, "wm.dta")
write_dta(wm, out_path, version = 14)


