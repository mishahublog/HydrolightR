# Assuming 'final_df' is already loaded in your environment
# 1. Subset data for Eastern Greenland 2015
Eastern_Greenland_full_depth <- data.frame(subset(final_df, final_df$year == 2014 &
                                                    final_df$Longitude..degrees_east. > 0))

# FIX 1: Corrected apg.676...1.m. to cgp676 or agp650/agp715 matching your dataset's prefix pattern
# NOTE: Check if your function a_LH_676 expects 'agp' (CDOM/gelbstoff) or 'ap' (particulate) or 'cgp' (beam attenuation)
Eastern_Greenland_full_depth$a_LH_676 <- a_LH_676(a_715 = Eastern_Greenland_full_depth$agp715,
                                                  a_676 = Eastern_Greenland_full_depth$apg.676...1.m., # Ensure this matches your custom function's logic
                                                  a_650 = Eastern_Greenland_full_depth$agp650)

# Extract unique station names
station_names <- unique(Eastern_Greenland_full_depth$Station)

# Set working directory to write the files
setwd("Q:/SCM-UC/Arctic-SCMs/cdom_4_hydrolight_WSC_2014")

# 2. Loop through every unique station
for (station in station_names) {

  # Extract the specific data frame for the current loop element
  current_df <- subset(Eastern_Greenland_full_depth, Eastern_Greenland_full_depth$Station == station)

  # Select only the relevant fields: depth and a_LH_676
  # Limit to a maximum of 50 rows if available, safely avoiding out-of-bounds errors
  max_rows <- min(50, nrow(current_df))
  selected_data <- current_df[1:max_rows, c("Depth..m.", "ICH1..ex..em..310.450.nm...R.C.."), drop = FALSE]

  # Skip loop iteration if the station has no data rows
  if (nrow(selected_data) == 0) next

  # 3. Create a unique, descriptive output file name for each station
  output_filename <- paste0(station, "_cdomdata.txt")

  # 4. Open a file connection for writing
  file_conn <- file(output_filename, "w")

  # 5. Write the custom file header block
  writeLines("\\begin_header", file_conn)
  writeLines(paste0("orginal \"", station, "_cdomdata\" file showing the Hydrolight standard format."), file_conn)
  writeLines("Generated automatically from data using R.", file_conn)

  # Build left-aligned layout spacing matching the Hydrolight standard width
  header_row <- sprintf("%-12s%s", "depth (m)", "cdom(RU)")
  writeLines(header_row, file_conn)
  writeLines("\\end_header", file_conn)

  # 6. Loop through rows of this specific station and write aligned contents
  for (i in 1:nrow(selected_data)) {

    # FIX 2: Changed 'selected_data$depth[i]' to 'selected_data$Depth..m.[i]' to match original column name
    # Also handle potential NA values gracefully using standard formatting
    if (is.na(selected_data$Depth..m.[i]) || is.na(selected_data$ICH1..ex..em..310.450.nm...R.C..[i])) next

    depth_str <- format(as.numeric(selected_data$Depth..m.[i]), nsmall = 1)

    # Keep high float precision intact for your absorption/chlorophyll values
    cdom_str <- format(selected_data$ICH1..ex..em..310.450.nm...R.C..[i], scientific = FALSE)

    # Left-align depth inside an 11 character block
    data_row <- sprintf("%-11s%s", depth_str, cdom_str)
    writeLines(data_row, file_conn)
  }

  # 7. Close out data block and release the active file connection
  writeLines("\\end_data", file_conn)
  close(file_conn)

  cat("Successfully generated:", output_filename, "\n")
}
