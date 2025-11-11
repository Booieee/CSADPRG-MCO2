# ********************
#  Last names: Lim, Sayat (Lead), Sia, Vanguardia
#  Language: R
#  Paradigm(s): Functional, Procedural
#  ********************
library(data.table)

process_dataset <- function() {
  data <- fread("dpwh_flood_control_projects.csv")
  num_rows <- nrow(data)

  filter_year_count <- data[FundingYear >= 2021 & FundingYear <= 2023, .N]

  cat(sprintf("Processing dataset... (%d rows loaded, %d filtered for 2021-2023)\n", num_rows, filter_year_count))
}

main <- function() {

  repeat {
    cat("\nSelect Language Implementation: \n",
        "[1] Load the file \n",
        "[2] Generate Reports \n\n")

    response <- as.integer(readline(prompt = "Enter Choice: "))

    if (response == 1) {
      process_dataset()
    } else if (response == 2) {
      cat("Report generation is not yet implemented.\n")
    } else {
      cat("Exiting program.\n")
      break
    }

  }

}

main()