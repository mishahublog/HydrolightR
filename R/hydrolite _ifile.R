#' Create a HydroLight IFILE Configuration from Station Metadata
#'
#' Generates a HydroLight input file (IFILE) using station metadata
#' stored in a hydrographic dataset. The function automatically extracts
#' date, time, geographic coordinates, temperature, and salinity for a
#' specified station and populates a HydroLight template file.
#'
#' The resulting configuration file is written to the HydroLight batch
#' directory and named according to the station identifier:
#' \code{I<station_name>.txt}.
#'
#' @param station_name Character string specifying the station name as it
#'   appears in \code{hydrometa_data$Station}.
#' @param hydrometa_data Data frame containing station metadata. Required
#'   columns include:
#'   \itemize{
#'     \item \code{Station}
#'     \item \code{day}
#'     \item \code{Hour}
#'     \item \code{Longitude..degrees_east.}
#'     \item \code{Latitude..degrees_north.}
#'     \item \code{Salinity}
#'     \item \code{Temperature...C.}
#'   }
#' @param year Numeric year used to construct the path to the AC validation
#'   file. Default is \code{2015}.
#' @param template_path Character string giving the path to the HydroLight
#'   template configuration file.
#' @param working_dir Character string specifying the HydroLight batch
#'   directory where the generated IFILE will be written.
#' @param ac_txt_dir Character string specifying the base directory path
#'   where the station-specific AC validation `.txt` files are located.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Retrieves metadata for the specified station.
#'   \item Verifies and constructs the path to the station-specific AC file.
#'   \item Creates HydroLight environmental and geographic settings.
#'   \item Replaces placeholders in the template configuration file.
#'   \item Writes the completed HydroLight IFILE to disk.
#' }
#'
#' Geographic coordinates are rounded to two decimal places and converted
#' to the decimal-comma format expected by HydroLight.
#'
#' @return Invisibly returns the full path to the generated HydroLight
#'   configuration file.
#'
#' @examples
#' \dontrun{
#' hydrolight_ifile(
#'   station_name = "P1",
#'   hydrometa_data = hydrometa_data
#' )
#'
#' lapply(
#'   unique(hydrometa_data$Station),
#'   hydrolight_ifile,
#'   hydrometa_data = hydrometa_data
#' )
#' }
#'
#' @author Your Name
#' @export
# ---------------------------------------------------------
# Create HydroLight IFILE directly from station name
# # ---------------------------------------------------------
hydrolight_ifile <- function(
    station_name,
    hydrometa_data=hydrometa_data,
    template_path = "Q:/hydrolite_data/optical_proxyies_inverse/template_config.txt",
    working_dir = "C:/HE60/run/batch",
    ac_txt_dir = "Q:/hydrolight_runs/new_phase",
    chl_txt_dir = "Q:/SCM-UC/Arctic-SCMs/chl_hydrolight",
    chl_suffix = "_chlzdata.txt",
    cdom_txt_dir = "Q:/SCM-UC/Arctic-SCMs/cdom_hydrolight/",
    cdom_suffix = "_cdomdata.txt",
    data_suffix = "_smooac_2015.txt",
    bb_suffix = "_smooac_2015_Hydrolight_bb.txt",
    bb_txt_dir = "Q:/hydrolight_runs/WSC_2015_bb_runs/bb_files"

){
#station_name <- "FS015"
  # ---------------------------------------------------------
  # Find station metadata
  # ---------------------------------------------------------
  subset_data <- hydrometa_data[hydrometa_data$Station == station_name, ]

  if (nrow(subset_data) == 0) {
    stop(paste("Station not found:", station_name))
  }

  hydro_stat_data <- subset_data[1, ]

  # ---------------------------------------------------------
  # Build and verify AC validation file path
  # ---------------------------------------------------------
  validation_path <- paste0(ac_txt_dir,"/",station_name,data_suffix)

  if (!file.exists(validation_path)) {
    stop(paste("Target .txt file not found at path:", validation_path))
  }

  # ---------------------------------------------------------
  # Build and verify chl file path
  # ---------------------------------------------------------
  chl_path <- paste0(chl_txt_dir,"/",station_name,chl_suffix)

  if (!file.exists(chl_path)) {
    stop(paste("Target chl .txt file not found at path:", chl_path))
  }


  # ---------------------------------------------------------
  # Build and verify cdom file path
  # ---------------------------------------------------------
  cdom_path <- paste0(cdom_txt_dir,"/",station_name,cdom_suffix)

  if (!file.exists(cdom_path)) {
    stop(paste("Target cdom .txt file not found at path:", cdom_path))
  }

  # ---------------------------------------------------------
  # Build and verify bb file path
  # ---------------------------------------------------------
  bb_path <- paste0(bb_txt_dir,"/",station_name,bb_suffix)

  if (!file.exists(cdom_path)) {
    stop(paste("Target backscatter .txt file not found at path:", bb_path))
  }


  # ---------------------------------------------------------
  # Set working directory
  # ---------------------------------------------------------
  setwd(working_dir)

  # ---------------------------------------------------------
  # Format coordinates
  # ---------------------------------------------------------
  lon <- format(round(as.numeric(hydro_stat_data$Longitude..degrees_east.), 2),nsmall=1)
  lat <- format(round(as.numeric(hydro_stat_data$Latitude..degrees_north.), 2),nsmall=1)
# The decimels should be in comma, not in decimel dots
  long_deci <- lon
  lat_deci  <- lat
  #long_deci <- gsub("\\.", ",", as.character(lon)) #Add commas
  #lat_deci  <- gsub("\\.", ",", as.character(lat)) #Add commas

  # ---------------------------------------------------------
  # Dynamic values
  # ---------------------------------------------------------
  geo_conditions <- paste0(
    as.numeric(hydro_stat_data$day),
    ", ",
    lat_deci,
    ", ",
    long_deci,
    ", 29.92, 1, 80, 2.5, 15, 5, 300"
  )

  env_conditions <- paste0(
    "5, 1.34, ",
    as.numeric(hydro_stat_data$Temperature...C.),
    ", ",
    as.numeric(hydro_stat_data$Salinity),
    ", 3"
  )

  sky_conditions <- paste0(
    "3, 3, ",
    as.numeric(hydro_stat_data$Hour),
    ", 1, 0"
  )

  # ---------------------------------------------------------
  # Configuration values
  # ---------------------------------------------------------
  config_values <- list(
    general = "0, 400, 700, 0.02, 488, 0.00026, 1, 5.3",
    data_header = "Station",
    data_name = station_name,
    geometry_1 = "0, 0, 2, 1, 0",
    geometry_2 = "3, 1, 0, 4, 3, 1",
    depth_boundary = "2, 4",
    surface = "0, 0, 0, 0",
    layer_1 = "0, 0, 440, 1, 0.014",
    layer_2 = "2, -666, 440, 1, 0.014",
    layer_3 = "1, -1, 440, 1, 0.014",
    layer_4 = "1, 0, 440, 1, 0.014",
    H2OabsorpTS = "../data/H2OabsorpTS.txt",
    astar1 = "astarDummy.txt",
    astar2 = "astarDummy.txt",
    astarchl = "../data/examples/astarchl.txt",
    missing_IOPs = "0, -999, -999, -999, -999, -999",
    end_IOPs = "-666, -999, -999, -999, -999, -999",
    bstar1 = "bstarDummy.txt",
    bstar2 = "dummybstar.txt",
    irradiance_1 = "0,0,550,0.01,0",
    irradiance_2 = "-2,0,550,0.01,0", #change if you have a different phase functions
    phase_H2O = "dpf_pure_H2O.txt",
    phase_particles = "dpf_Petzold_avg_particle.txt",
    num_wavelengths = 80,
    wavelengths = paste(seq(400, 800, 5), collapse = ","),
    output_flags = "1,1,1,1,0",
    sky_conditions = sky_conditions,
    geo_conditions = geo_conditions,
    env_conditions = env_conditions,
    surface_conditions = "0, 0",
    output_depths = "0, 11, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50",
    H2Oabsorp_repeat = "../data/H2OabsorpTS.txt",
    num_runs = 1,
    validation_file = validation_path,
    ac9_file = "C:/HE60/data/examples/astar_CDOM_014_440.txt",
    scat_file = bb_path,
    chl_file = chl_path,
    cdom_file = cdom_path,
    bottom_reflect = "dummyR.bot",
    comp_file1 = "dummyComp.txt",
    comp_file2 = "dummyComp.txt",
    irrad_file = "DummyIrrad.txt",
    biolum_file = "../data/examples/So_biolum_user_data.txt",
    rad_file = "DummyRad.txt"
  )

  # ---------------------------------------------------------
  # Read template
  # ---------------------------------------------------------
  template_text <- readLines(template_path, warn = FALSE)
  template_text <- paste(template_text, collapse = "\n")

  # ---------------------------------------------------------
  # Replace placeholders
  # ---------------------------------------------------------
  for (name in names(config_values)) {
    template_text <- gsub(
      paste0("\\$", name, "\\b"),  # \\b ensures an exact word match
      as.character(config_values[[name]]),
      template_text,
      perl = TRUE                 # Enables advanced regex matching
    )
  }

  # ---------------------------------------------------------
  # Save IFILE
  # ---------------------------------------------------------
  output_file <- paste0("I", station_name, ".txt")

  # Open file connection in binary mode ("wb") to force LF
  con <- file(output_file, open = "wb")
  writeLines(template_text, con, sep = "\n", useBytes = TRUE)
  close(con)

  message("HydroLight configuration created with unixLF: ",
          normalizePath(output_file))

  invisible(output_file)
}
