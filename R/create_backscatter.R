# Create backscatter from ac files

# read ac files
test<- read.csv("FS015_smooac_2015.txt",skip = 14,sep = "",header = FALSE,flush = TRUE,nrows = 50)

a_names<- c( "depth","a412","a440","a488","a510","a532","a555","a650","a676","a715" )

c_names<- c( "c412","c440","c488" ,"c510" ,"c532" ,"c555" , "c650"  ,"c676"   ,"c715" )

names(test)<- c(a_names,c_names)

data.frame(
  bb440 = (test$c440 -test$a440)*0.014,
  bb488 = (test$c488 - test$a488)*0.014,
  bb532 = (test$c532 - test$a532)*0.014,
  bb555 = (test$c555 - test$a555)*0.014,
  bb650 = (test$c650 - test$a650)*0.014,
  bb676 = (test$c676 - test$a676)*0.014

)





# Load data from the ac-s file
ac_data <- read.csv("Q:/hydrolight_runs/new_phase/FS015_smooac_2015.txt", skip = 14, sep = "", header = FALSE, flush = TRUE, nrows = 50)

# Define column names
a_names <- c("depth", "a412", "a440", "a488", "a510", "a532", "a555", "a650", "a676", "a715")
c_names <- c("c412", "c440", "c488", "c510", "c532", "c555", "c650", "c676", "c715")
colnames(ac_data) <- c(a_names, c_names)

# Define target wavelengths and backscattering ratio
wave_lengths <- c(440, 488, 532, 555, 650, 676)
tilde_bb <- 0.014  # Particulate backscattering ratio (b_bp / b_p)

# Calculate b_bp = (c - a) * tilde_bb for each wavelength
bb_df <- data.frame(depth = ac_data$depth)
for (wl in wave_lengths) {
  a_col <- paste0("a", wl)
  c_col <- paste0("c", wl)
  bb_df[[paste0("bb", wl)]] <- (ac_data[[c_col]] - ac_data[[a_col]]) * tilde_bb
}

# Define file output name
output_filename <- "Hydroscat_from_ac.txt"

# Open file connection and write Hydrolight header
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

# Write wavelength record (Number of wavelengths followed by the wavelength values)
wl_header <- sprintf("  %d      %s", length(wave_lengths), paste(sprintf("%.1f", wave_lengths), collapse = "    "))
writeLines(wl_header, file_conn)

# Write data records in Scientific Notation matching Hydrolight format
for (i in 1:nrow(bb_df)) {
  depth_str <- sprintf(" %6.3f", bb_df$depth[i])
  bb_vals_str <- paste(sprintf("%10.3E", as.numeric(bb_df[i, -1])), collapse = " ")
  writeLines(paste(depth_str, bb_vals_str), file_conn)
}

# Write end marker and close file
writeLines("\\end_data", file_conn)
close(file_conn)

cat("Successfully exported Hydrolight backscattering file:", output_filename, "\n")













