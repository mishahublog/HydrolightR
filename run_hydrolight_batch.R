library(progressr)

# 1. Update the function to accept multiple stations
run_hydrolight_batch <- function(stations) {
  setwd("C:/HE60/backend")

  # Create a progress reporter signal
  p <- progressor(steps = length(stations))

  for (station in stations) {
    input_file <- paste0("..\\run\\batch\\I", station, ".txt")

    if (!file.exists(input_file)) {
      warning(paste("Input file not found:", input_file))
      p(message = paste("Skipped:", station))
      next
    }

    cmd <- paste("HydroLight6.exe <", shQuote(input_file))
    shell(cmd) # Runs synchronously one by one

    # Update progress bar after each station completes
    p(message = paste("Finished station", station))
  }
}

# 2. To run it with a visual progress bar in your console:
handlers(global = TRUE)
handlers("txtprogressbar") # Or "beepr", "rstudio", etc.

# Example execution:
# run_hydrolight_batch(c("001", "002", "003"))
