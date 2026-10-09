library(dplyr)
library(stringr)
library(lubridate)

# Set working directory
setwd("Q:/hydrolite_data/extracted stations")

# ---------------------------------------------------------
# List CSV files (case-insensitive)
# ---------------------------------------------------------
files <- list.files(pattern = "\\.csv$", ignore.case = TRUE)# when stations

# Read and combine all CSVs
df_list <- lapply(files, read.csv, stringsAsFactors = FALSE)
data_meta <- bind_rows(df_list)
meta_parameters<- c("X", "Cruise", "Station", "Type", "yyyy.mm.ddThh.mm.ss.sss",
                    "Longitude..degrees_east.", "Latitude..degrees_north.", "St.", "Depth..m.",
                    "Temperature...C.", "Salinity", "SigmT")
# Keep the data you considered as mets, for comparison and statistics
meta <- data_meta[, meta_parameters]
# ---------------------------------------------------------
# Extract year
# ---------------------------------------------------------
meta$years <- str_split(meta$yyyy.mm.ddThh.mm.ss.sss, "-", simplify = TRUE)[,1]
# Unique stations
stations <- unique(meta$Station)
# ---------------------------------------------------------
# Convert to datetime
# ---------------------------------------------------------
meta$Time <- as.POSIXct(
  meta$yyyy.mm.ddThh.mm.ss.sss,
  format = "%Y-%m-%dT%H:%M",
  tz = "UTC"
)
# Extract Hour (HH.MM format like Python)
meta$Hour <- format(meta$Time, "%H.%M")
# Day of year
meta$day <- yday(meta$Time)
# Final dataset
hydrometa_data <- meta

# ---------------------------------------------------------
# Access examples (equivalent to pandas .loc)
# ---------------------------------------------------------

