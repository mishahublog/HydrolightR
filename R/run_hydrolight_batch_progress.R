library(progress)
library(stringr)
library(RPushbullet)

run_hydrolight_batch <- function(directory = "C:/HE60/run/batch", pb_api_key = "o.fMNaBrPojmldEtyTVULaNN1OpBY6DfV7") {

  # Set up API key
  pbSetup(apikey = pb_api_key)

  # Set directory and get stations
  original_wd <- getwd()
  setwd(directory)
  on.exit(setwd(original_wd)) # Ensure we return to original path on exit

  files <- list.files()
  stations <- str_replace_all(files, c("^I" = "", "\\.txt$" = ""))

  # Progress bar
  pb <- progress_bar$new(
    format = "Running [:bar] :current/:total (:percent) ETA: :eta",
    total = length(stations),
    clear = FALSE,
    width = 60
  )

  failed <- data.frame(
    station = character(),
    error = character(),
    stringsAsFactors = FALSE
  )

  # Loop
  for (i in stations) {
    tryCatch(
      {
        run_hydrolight(station = i)
      },
      error = function(e) {
        failed <<- rbind(
          failed,
          data.frame(
            station = i,
            error = conditionMessage(e),
            stringsAsFactors = FALSE
          )
        )
      }
    )
    pb$tick()
  }

  # Summary
  successful <- length(stations) - nrow(failed)
  cat("\n\nBatch processing finished.\n")
  cat("Successful:", successful, "\n")
  cat("Failed:", nrow(failed), "\n")

  # Reporting and Notification
  if (nrow(failed) > 0) {
    report_file <- file.path(getwd(), "HydroLight_failed_report.csv")
    write.csv(failed, report_file, row.names = FALSE)

    cat("\nReport saved as:", report_file, "\n")

    pbPost(
      type = "note",
      title = "HydroLight batch completed",
      body = paste(
        "Stations completed:", successful,
        "\nFailed:", nrow(failed),
        "\nReport:", report_file
      )
    )
  } else {
    pbPost(
      type = "note",
      title = "HydroLight batch completed",
      body = paste(successful, "stations completed successfully.")
    )
  }

  return(failed) # Return the dataframe of failures for further inspection if needed
}
