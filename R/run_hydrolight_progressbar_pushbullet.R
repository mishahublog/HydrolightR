library(progress)
library(stringr)
library(RPushbullet)

#----------------------------------------
# Pushbullet setup
#----------------------------------------
pbSetup(apikey ="o.fMNaBrPojmldEtyTVULaNN1OpBY6DfV7")

setwd("C:/HE60/run/batch")
files <- list.files()

stations <- str_replace_all(files, c("^I" = "", "\\.txt$" = ""))

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

#----------------------------------------
# Summary
#----------------------------------------
successful <- length(stations) - nrow(failed)

cat("\n\nBatch processing finished.\n")
cat("Successful:", successful, "\n")
cat("Failed:", nrow(failed), "\n")

if (nrow(failed) > 0) {

  cat("\nFailed stations:\n")
  print(failed)

  report_file <- file.path(getwd(), "HydroLight_failed_report.csv")

  write.csv(
    failed,
    report_file,
    row.names = FALSE
  )

  cat("\nReport saved as 'HydroLight_failed_report.csv'\n")

  # Pushbullet notification
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

  # Pushbullet notification
  pbPost(
    type = "note",
    title = "HydroLight batch completed",
    body = paste(
      successful,
      "stations completed successfully."
    )
  )

}

