#' Create Hydrolight Backscattering Input Files from ac-s Measurements
#'
#' Reads raw ac-s optical profiling data, computes particle backscattering coefficients
#' (beta_bp) across specified wavelengths using a scaling factor (\code{tilde_bb}),
#' and exports a formatted text file compatible with Hydrolight modeling software.
#'
#' @param input_filepath Character string specifying the path to the raw ac-s data file.
#' @param output_dir Character string specifying the target directory where the output
#'   Hydrolight text file will be saved. Default is current directory (\code{"."}).
#' @param tilde_bb Numeric scaling factor used to convert particulate beam attenuation
#'   minus absorption into particulate backscattering. Default is \code{0.014}.
#' @param wave_lengths Numeric vector of wavelengths (in nm) to process and include
#'   in the output file. Default is \code{c(440, 488, 532, 555, 650, 676)}.
#' @param skip_lines Integer. Number of header lines to skip when reading the raw ac-s file.
#'   Default is \code{14}.
#' @param num_rows Integer. Maximum number of depth rows to read from the input file.
#'   Default is \code{50}.
#' @return A character string representing the file path of the successfully generated Hydrolight text file.
#' @export
#' @examples
#' \dontrun{
#' create_bb(
#'   input_filepath = "path/to/raw_acs_data.txt",
#'   output_dir = "Q:/hydrolight_runs/WSC_2015_bb_runs",
#'   tilde_bb = 0.014
#' )
#' }
create_bb <- function(input_filepath,
                      output_dir = ".",
                      tilde_bb = 0.014,
                      wave_lengths = c(440, 488, 532, 555, 650, 676),
                      skip_lines = 14,
                      num_rows = 50) {

  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Read ac-s raw data
  ac_data <- read.csv(input_filepath, skip = skip_lines, sep = "", header = FALSE, flush = TRUE, nrows = num_rows)

  # Define column names
  a_names <- c("depth", "a412", "a440", "a488", "a510", "a532", "a555", "a650", "a676", "a715")
  c_names <- c("c412", "c440", "c488", "c510", "c532", "c555", "c650", "c676", "c715")
  colnames(ac_data) <- c(a_names, c_names)

  # Force all columns to numeric (converts non-numeric string values to NA)
  ac_data[] <- lapply(ac_data, function(x) as.numeric(as.character(x)))

  # Remove any rows where depth or optical data is missing/corrupted
  ac_data <- na.omit(ac_data)

  # Calculate b_bp = (c - a) * tilde_bb for each wavelength
  bb_df <- data.frame(depth = ac_data$depth)
  for (wl in wave_lengths) {
    a_col <- paste0("a", wl)
    c_col <- paste0("c", wl)

    if (!(a_col %in% colnames(ac_data)) || !(c_col %in% colnames(ac_data))) {
      stop(paste("Wavelength", wl, "nm not found in column definitions."))
    }

    bb_df[[paste0("bb", wl)]] <- (ac_data[[c_col]] - ac_data[[a_col]]) * tilde_bb
  }

  # Construct output file path
  base_name <- tools::file_path_sans_ext(basename(input_filepath))
  output_filename <- file.path(output_dir, paste0(base_name, "_Hydrolight_bb.txt"))

  # Open file connection and write header
  file_conn <- file(output_filename, "w")

  writeLines(c(
    "\\begin_header",
    "DATA GENERATED FROM AC-S MEASUREMENTS showing the Hydrolight Standard Format for backscatter data.",
    "NOTE: These bb values are only the particle backscatter coef b_b, without water.",
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

  # Close file connection
  writeLines("\\end_data", file_conn)
  close(file_conn)

  cat("Successfully created:", output_filename, "\n")
  return(output_filename)
}
