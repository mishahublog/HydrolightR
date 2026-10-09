run_hydrolight <- function(station,model) {


 setwd("C:/HE60/backend")

  input_file <- paste0("..\\run\\batch\\I", station, ".txt")

  if (!file.exists(input_file)) {
    stop(paste("Input file not found:", input_file))
  }

  cmd <- paste0(model,"Light6.exe <", shQuote(input_file))

  shell(cmd)
}
