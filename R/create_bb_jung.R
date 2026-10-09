# create_bb_jung <- function(input_filepath,
#                       output_dir = ".",
#                       tilde_bb = 0.018, # Numeric backscatter fraction
#                       wave_lengths = c(440, 488, 532, 555, 650, 676),
#                       skip_lines = 14,
#                       num_rows = 50) {
#
#   # Ensure the output directory exists
#   if (!dir.exists(output_dir)) {
#     dir.create(output_dir, recursive = TRUE)
#   }
#
#   # Read ac-s raw data
#   ac_data <- read.csv(input_filepath, skip = skip_lines, sep = "", header = FALSE, flush = TRUE, nrows = num_rows)
#
#   # Define column names
#   a_names <- c("depth", "a412", "a440", "a488", "a510", "a532", "a555", "a650", "a676", "a715")
#   c_names <- c("c412", "c440", "c488", "c510", "c532", "c555", "c650", "c676", "c715")
#   colnames(ac_data) <- c(a_names, c_names)
#
#   # Convert columns safely to numeric
#   ac_data[] <- lapply(ac_data, function(x) as.numeric(as.character(x)))
#
#   # Remove rows with missing or corrupted optical/depth data
#   ac_data <- na.omit(ac_data)
#
#   if (nrow(ac_data) == 0) {
#     stop("No valid data rows remaining after na.omit(). Check input file or skip_lines setting.")
#   }
#
#   # Calculate b_bp = (c - a) * tilde_bb for each wavelength
#   bb_df <- data.frame(depth = ac_data$depth)
#
#   for (idx in seq_along(wave_lengths)) {
#     wl <- wave_lengths[idx]
#     a_col <- paste0("a", wl)
#     c_col <- paste0("c", wl)
#
#     if (!(a_col %in% colnames(ac_data)) || !(c_col %in% colnames(ac_data))) {
#       stop(paste("Wavelength", wl, "nm not found in column definitions."))
#     }
#
#     t_bb <- if (length(tilde_bb) == length(wave_lengths)) tilde_bb[idx] else tilde_bb[1]
#     bb_df[[paste0("bb", wl)]] <- (ac_data[[c_col]] - ac_data[[a_col]]) * t_bb
#   }
#
#   # Construct output file path
#   base_name <- tools::file_path_sans_ext(basename(input_filepath))
#   output_filename <- file.path(output_dir, paste0(base_name, "_Hydrolight_bb.txt"))
#
#   # Format tilde_bb description line for header
#   tilde_str <- if (length(tilde_bb) == 1) {
#     paste("tilde_bb value used:", tilde_bb)
#   } else {
#     paste("tilde_bb values used:", paste(tilde_bb, collapse = ", "))
#   }
#
#   # Open file connection safely
#   file_conn <- file(output_filename, "w")
#   on.exit(close(file_conn), add = TRUE)
#
#   # Write HydroLight header including tilde_bb
#   writeLines(c(
#     "\\begin_header",
#     "DATA GENERATED FROM AC-S MEASUREMENTS showing the Hydrolight Standard Format for backscatter data.",
#     "NOTE: These bb values are only the particle backscatter coef b_b, without water.",
#     tilde_str, # Added line header here
#     "Select the option \"bb data file does NOT include pure water bb\" on the IOP data form.",
#     "Pure sea water bb values will be then ADDED by Hydrolight to get the total bb.",
#     "Column headers (depth in m and bb in 1/m):",
#     paste(c("  depth", paste0("     bb", wave_lengths)), collapse = " "),
#     "The first data record gives the number of wavelengths and the wavelengths.",
#     "\\end_header"
#   ), file_conn)
#
#   # Write wavelength record header
#   wl_header <- sprintf("  %d      %s", length(wave_lengths), paste(sprintf("%.1f", wave_lengths), collapse = "    "))
#   writeLines(wl_header, file_conn)
#
#   # Write depth and backscattering records
#   for (i in 1:nrow(bb_df)) {
#     depth_str <- sprintf(" %6.3f", bb_df$depth[i])
#     bb_vals_str <- paste(sprintf("%10.3E", as.numeric(bb_df[i, -1])), collapse = " ")
#     writeLines(paste(depth_str, bb_vals_str), file_conn)
#   }
#
#   writeLines("\\end_data", file_conn)
#
#   cat("Successfully created:", output_filename, "\n")
#   return(output_filename)
# }




create_bb_jung<- function(input_filepath,
                      output_dir = ".",
                      tilde_bb = 0.018, # Default backscatter ratio (numeric)
                      wave_lengths = c(440, 488, 532, 555, 650, 676),
                      skip_lines = 14,
                      num_rows = 50) {

  # 1. Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # 2. Rectify & Validate tilde_bb inputs
  # Convert to numeric in case a string/character number was passed (e.g. "0.018")
  tilde_bb_num <- suppressWarnings(as.numeric(tilde_bb))

  # Check for NA, non-numeric strings, or invalid lengths
  if (any(is.na(tilde_bb_num))) {
    stop("Error: 'tilde_bb' contains NA or non-numeric values. Please provide valid numeric value(s).")
  }

  if (length(tilde_bb_num) != 1 && length(tilde_bb_num) != length(wave_lengths)) {
    stop(sprintf(
      "Error: 'tilde_bb' length (%d) must either be 1 or match 'wave_lengths' length (%d).",
      length(tilde_bb_num), length(wave_lengths)
    ))
  }

  # 3. Read ac-s raw data
  ac_data <- read.csv(input_filepath, skip = skip_lines, sep = "", header = FALSE, flush = TRUE, nrows = num_rows)

  # Define column names
  a_names <- c("depth", "a412", "a440", "a488", "a510", "a532", "a555", "a650", "a676", "a715")
  c_names <- c("c412", "c440", "c488", "c510", "c532", "c555", "c650", "c676", "c715")
  colnames(ac_data) <- c(a_names, c_names)

  # Convert columns safely to numeric
  ac_data[] <- lapply(ac_data, function(x) as.numeric(as.character(x)))

  # Remove rows with missing or corrupted optical/depth data
  ac_data <- na.omit(ac_data)

  if (nrow(ac_data) == 0) {
    stop("No valid data rows remaining after na.omit(). Check input file or skip_lines setting.")
  }

  # 4. Calculate b_bp = (c - a) * tilde_bb for each wavelength
  bb_df <- data.frame(depth = ac_data$depth)

  for (idx in seq_along(wave_lengths)) {
    wl <- wave_lengths[idx]
    a_col <- paste0("a", wl)
    c_col <- paste0("c", wl)

    if (!(a_col %in% colnames(ac_data)) || !(c_col %in% colnames(ac_data))) {
      stop(paste("Wavelength", wl, "nm not found in column definitions."))
    }

    # Assign single scalar or wavelength-specific tilde_bb value
    t_bb <- if (length(tilde_bb_num) == length(wave_lengths)) tilde_bb_num[idx] else tilde_bb_num[1]

    bb_df[[paste0("bb", wl)]] <- (ac_data[[c_col]] - ac_data[[a_col]]) * t_bb
  }

  # Double-check no NAs were created during computation
  if (any(is.na(bb_df))) {
    stop("Error: NA values generated in backscatter calculations. Check ac-s input data.")
  }

  # 5. Construct output file path and format header text
  base_name <- tools::file_path_sans_ext(basename(input_filepath))
  output_filename <- file.path(output_dir, paste0(base_name, "_Hydrolight_bb.txt"))

  tilde_str <- if (length(tilde_bb_num) == 1) {
    paste("tilde_bb value used:", tilde_bb_num)
  } else {
    paste("tilde_bb values used:", paste(tilde_bb_num, collapse = ", "))
  }

  # Open file connection safely
  file_conn <- file(output_filename, "w")
  on.exit(close(file_conn), add = TRUE)

  # Write HydroLight header
  writeLines(c(
    "\\begin_header",
    "DATA GENERATED FROM AC-S MEASUREMENTS showing the Hydrolight Standard Format for backscatter data.",
    "NOTE: These bb values are only the particle backscatter coef b_b, without water.",
    tilde_str,
    "Select the option \"bb data file does NOT include pure water bb\" on the IOP data form.",
    "Pure sea water bb values will be then ADDED by Hydrolight to get the total bb.",
    "Column headers (depth in m and bb in 1/m):",
    paste(c("  depth", paste0("     bb", wave_lengths)), collapse = " "),
    "The first data record gives the number of wavelengths and the wavelengths.",
    "\\end_header"
  ), file_conn)

  # Write wavelength record header
  wl_header <- sprintf("  %d      %s", length(wave_lengths), paste(sprintf("%.1f", wave_lengths), collapse = "    "))
  writeLines(wl_header, file_conn)

  # Write depth and backscattering records
  for (i in 1:nrow(bb_df)) {
    depth_str <- sprintf(" %6.3f", bb_df$depth[i])
    bb_vals_str <- paste(sprintf("%10.3E", as.numeric(bb_df[i, -1])), collapse = " ")
    writeLines(paste(depth_str, bb_vals_str), file_conn)
  }

  writeLines("\\end_data", file_conn)

  cat("Successfully created:", output_filename, "\n")
  return(output_filename)
}


























