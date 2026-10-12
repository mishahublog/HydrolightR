


#' Generate Hydrolight Standard Input Files by Station (with Conditional Calculation)
#'
#' Subsets oceanographic profile data for Eastern Greenland (2015), conditionally
#' computes line-height absorption if not already present, and exports formatted
#' text files for each unique station compatible with Hydrolight modeling software.
#'
#' @param data_file Data frame containing the raw oceanographic dataset. Must include columns
#'   \code{year}, \code{Longitude..degrees_east.}, \code{Depth..m.}, and \code{Station}.
#' @param output_dir Character string specifying the target directory path where output text files will be saved.
#' @param max_rows Integer. Maximum number of depth rows to output per station file. Default is \code{50}.
#' @return Invisible \code{NULL}. Side-effect: writes `.txt` files to the specified output directory.
#' @export
#' @examples
#' \dontrun{
#' generate_hydrolight_files(data_file = my_raw_data, output_dir = "Q:/hydrolight_runs/Abs_2015_bb_runs/chl_a")
#' }
generate_hydrolight_Chl_files <- function(data_file, output_dir, max_rows = 50) {

  # 1. Subset data for Eastern Greenland 2015
  Abs_full_depth <- data.frame(subset(
    data_file,
    data_file$year == 2015 &
      data_file$Longitude..degrees_east. > 0 &
      data_file$Depth..m. > 0
  ))

  # IF CONDITION: Check if 'a_LH_676' already exists; calculate it only if missing

  a_LH_676 <- function(a_715, a_650, a_676) {
    L_rf <- 676
    a_BL <- ((a_715 - a_650) / (715 - 650)) * (L_rf - 650) + a_650
    a_LH_676 <- a_676 - a_BL
    return(a_LH_676)
  }



  if (!"a_LH_676" %in% names(Abs_full_depth)) {
    message("Calculating line-height absorption (a_LH_676)...")
    Abs_full_depth$a_LH_676 <- a_LH_676(a_715 = Abs_full_depth$agp715,
                                      a_676 = Abs_full_depth$apg.676...1.m.,
                                      a_650 = Abs_full_depth$agp650)
  } else {
    message("Using existing 'a_LH_676' column found in dataset.")
  }

  # Extract unique station names
  station_names <- unique(WSC_full_depth$Station)

  # Set working directory to write the files
  setwd("Q:/hydrolight_runs/WSC_2015_bb_runs/chl_a")

  # 2. Loop through every unique station
  for (station in station_names) {

    # Extract the specific data frame for the current loop element
    current_df <- subset(WSC_full_depth, WSC_full_depth$Station == station)

    # Select only the relevant fields: depth and a_LH_676
    # Limit to a maximum of 50 rows if available, safely avoiding out-of-bounds errors
    max_rows <- min(50, nrow(current_df))
    selected_data <- current_df[1:max_rows, c("Depth..m.", "a_LH_676"), drop = FALSE]

    # Skip loop iteration if the station has no data rows
    if (nrow(selected_data) == 0) next

    # 3. Create a unique, descriptive output file name for each station
    output_filename <- paste0(station, "_chlzdata.txt")

    # 4. Open a file connection for writing
    file_conn <- file(output_filename, "w")

    # 5. Write the custom file header block
    writeLines("\\begin_header", file_conn)
    writeLines(paste0("orginal \"", station, "_chlzdata\" file showing the Hydrolight standard format."), file_conn)
    writeLines("Generated automatically from data using R.", file_conn)

    # Build left-aligned layout spacing matching the Hydrolight standard width
    header_row <- sprintf("%-12s%s", "depth (m)", "Chl (mg/m^3)")
    writeLines(header_row, file_conn)
    writeLines("\\end_header", file_conn)

    # 6. Loop through rows of this specific station and write aligned contents
    for (i in 1:nrow(selected_data)) {

      # FIX 2: Changed 'selected_data$depth[i]' to 'selected_data$Depth..m.[i]' to match original column name
      # Also handle potential NA values gracefully using standard formatting
      if (is.na(selected_data$Depth..m.[i]) || is.na(selected_data$a_LH_676[i])) next

      depth_str <- format(as.numeric(selected_data$Depth..m.[i]), nsmall = 1)

      # Keep high float precision intact for your absorption/chlorophyll values
      chl_str <- format(selected_data$a_LH_676[i], scientific = FALSE)

      # Left-align depth inside an 11 character block
      data_row <- sprintf("%-11s%s", depth_str, chl_str)
      writeLines(data_row, file_conn)
    }

    # 7. Close out data block and release the active file connection
    writeLines("\\end_data", file_conn)
    close(file_conn)

    cat("Successfully generated:", output_filename, "\n")
  }

}


