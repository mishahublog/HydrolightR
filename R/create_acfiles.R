#' Create Hydrolight AC Input Files from Absorption and Attenuation Spectra
#'
#' Reads absorption (`a`) and attenuation (`c`) spectral datasets, merges them,
#' and creates Hydrolight-formatted AC input files for one or more stations.
#' Each output file contains a standard Hydrolight header, wavelength
#' information, and depth-resolved absorption and attenuation coefficients.
#'
#' The function is designed for Hydrolight simulations requiring user-defined
#' inherent optical properties (IOPs) in the standard "ac-x" format.
#'
#' @param data_dir Character string. Directory containing the absorption and
#'   attenuation input files.
#'   Defaults to
#'   `"Q:/hydrolite_data/Explore_hydrolight/AC_corrected_datasets"`.
#' @param output_dir Character string. Directory where the Hydrolight AC text
#'   files will be written. Defaults to `data_dir`.
#' @param abs_file Character string. Name of the CSV file containing absorption
#'   spectra. Defaults to
#'   `"Atl_wat_all_depths_abs_spect_smoothed_2015.csv"`.
#' @param atts_file Character string. Name of the CSV file containing attenuation
#'   spectra. Defaults to
#'   `"Atl_wat_all_depths_atts_spect_smoothed_2015.csv"`.
#' @param stations Character vector of station identifiers to process.
#'   If `NULL` (default), all stations found in the merged dataset are processed.
#' @param max_depths Integer. Maximum number of depth records to include for
#'   each station. Defaults to `50`.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Reads absorption and attenuation spectral datasets from CSV files.
#'   \item Merges the datasets using common columns.
#'   \item Selects stations specified in `stations`, or all available stations.
#'   \item Extracts wavelengths from absorption column names (e.g., `a412`,
#'   `a440`, `a488`).
#'   \item Creates one Hydrolight AC file per station.
#'   \item Writes depth, absorption coefficients, and attenuation coefficients
#'   in Hydrolight standard format.
#' }
#'
#' Output filenames follow the convention:
#' \code{<station>_smooac_2015.txt}
#'
#' The working directory is temporarily changed to `output_dir` while files are
#' written and restored automatically when the function exits.
#'
#' @return
#' Invisibly returns `NULL`. The primary output is a set of Hydrolight AC text
#' files written to `output_dir`.
#'
#' @examples
#' \dontrun{
#' # Create Hydrolight files for all stations in the default directory
#' create_hydrolight_ac_files()
#'
#' # Save outputs to a different folder
#' create_hydrolight_ac_files(output_dir = "C:/Hydrolight/Outputs")
#'
#' # Create files for selected stations
#' create_hydrolight_ac_files(
#'   stations = c("Station01", "Station05")
#' )
#'
#' # Limit output to the first 30 depth records
#' create_hydrolight_ac_files(max_depths = 30)
#' }
#'
#' @seealso
#' \code{\link[utils]{read.csv}},
#' \code{\link[base]{merge}}
#'
#' @export
create_hydrolight_ac_files <- function(
    data_dir = "Q:/hydrolite_data/Explore_hydrolight/AC_corrected_datasets",
    output_dir = "Q:/hydrolight_runs/new_phase",
    abs_file = "Atl_wat_all_depths_abs_spect_smoothed_2015.csv",
    atts_file = "Atl_wat_all_depths_atts_spect_smoothed_2015.csv",
    data_suffix = "_smooac_2014.txt",
    stations = NULL,
    max_depths = 50
) {

  if (is.character(abs_file) == TRUE && is.character(atts_file) == TRUE) {
    # Read data
    abs_df  <- read.csv(file.path(data_dir, abs_file))
    atts_df <- read.csv(file.path(data_dir, atts_file))
  } else {
    abs_df  <- abs_file
    atts_df <- atts_file
  }

  # Merge datasets
  merged_df <- merge(abs_df, atts_df)

  # Determine stations to process
  if (is.null(stations)) {
    stations <- unique(merged_df$station)
  }

  # Ensure the output directory exists before moving into it
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(output_dir)

  # Standard Hydrolight header
  header_lines <- c(
    "\\begin_header",
    "! Example file of ac-9 data on the Hydrolight Standard Format for \"ac-x\" data.",
    "! This file contains ac-9 data binned in 25 cm depth bins, starting with",
    "! the first measured depth. The absorption coefficient at 412 nm is",
    "! plotted as the left-hand curve of Fig. 1 in the Hydrolight Users' Guide.",
    "! This file will be read by user \"IOP\" routine \"IOP_userdata\"",
    "! Any number of ascii header records (for identification of the user's data) can be used.",
    "! The last header record is flagged by \\end_header",
    "! The first data record contains the number of wavelengths and the nominal wavelengths (free format)",
    "! Subsequent data records contain the (free-format) ac-9 data ordered as:",
    "! depth, a(wavelength 1), ..., a(wavelength n), c(wavelength 1), ..., c(wavelength n)",
    "! Flag the end of file with a negative depth or an end of file.",
    "\\end_header"
  )

  for (station in stations) {

    # Filter station data
    station_df <- merged_df[merged_df$station == station, ]
    station_df <- station_df[, names(station_df) != "station"]
    station_df <- station_df[1:min(max_depths, nrow(station_df)), -1]

    # Identify wavelength columns
    a_cols <- grep("^a", names(station_df), value = TRUE)
    waves  <- substring(a_cols, 2)

    # Output file
    filename <- paste0(station, data_suffix)
    file_conn <- file(filename, open = "w", encoding = "UTF-8")

    # Write header
    writeLines(header_lines, file_conn)

    # Write wavelength line
    wave_line <- paste(sprintf("%.1f", as.numeric(waves)), collapse = "   ")
    cat(
      paste0("   ", length(waves), "   ", wave_line, "\n"),
      file = file_conn
    )

    # Write data
    for (i in seq_len(nrow(station_df))) {

      row_vals <- station_df[i, ]

      formatted_vals <- sapply(seq_along(row_vals), function(j) {
        val <- row_vals[[j]]

        if (j == 1) {
          as.character(round(val))         # depth
        } else {
          sprintf("%.3E", as.numeric(val)) # a and c values
        }
      })

      cat(
        paste0("   ", paste(formatted_vals, collapse = "   "), "\n"),
        file = file_conn
      )
    }

    # End marker
    cat("\\end_data\n", file = file_conn)
    close(file_conn)

    message("Created: ", file.path(output_dir, filename))
  }

  invisible(NULL)
}
