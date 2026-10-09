library(progress)

setwd("C:/HE60/run/batch")
files <- list.files()
library(stringr)

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

# -------- Summary --------
cat("\n\nBatch processing finished.\n")
cat("Successful:", length(stations) - nrow(failed), "\n")
cat("Failed:", nrow(failed), "\n")

if (nrow(failed) > 0) {
  cat("\nFailed stations:\n")
  print(failed)

  # Save report
  write.csv(failed,
            "HydroLight_failed_report.csv",
            row.names = FALSE)

  cat("\nReport saved as 'HydroLight_failed_report.csv'\n")
}
