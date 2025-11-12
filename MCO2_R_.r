# ********************
#  Last names: Lim, Sayat (Lead), Sia, Vanguardia
#  Language: R
#  Paradigm(s): Functional, Procedural
#  ********************
library(data.table)

cost_saving <- function(approved_budget, contract_cost) {
  (approved_budget - contract_cost)
}

completion_delay_days <- function(start_date, actual_completion_date) {
  # Expect start_date and actual_completion_date to be Date or character; coerce safely
  s <- as.Date(start_date)
  a <- as.Date(actual_completion_date)
  as.integer(a - s)
}

process_dataset <- function() {
  # Read CSV
  dt <- fread("dpwh_flood_control_projects.csv", na.strings = c("", "NA", "N/A"))
  setDT(dt)
  num_rows <- nrow(dt)

  cat(sprintf("Loaded %d rows from dpwh_flood_control_projects.csv\n", num_rows))

  # Helper: clean currency-like column to numeric (conservative)
  clean_currency_column <- function(x) {
    s <- as.character(x)
    s[s == ""] <- NA_character_
    # NBSP -> space
    s <- gsub("\u00A0", " ", s)
    s <- trimws(s)

    # Parentheses -> negative
    s <- ifelse(grepl("^\\s*\\(.*\\)\\s*$", s), sub("^\\s*\\((.*)\\)\\s*$", "-\\1", s), s)

    # Leading numeric token
    leading <- sub("^\\s*([+-]?\\d[\\d,]*\\.?\\d*).*$", "\\1", s)
    leading[leading == s] <- NA_character_  # if substitution didn't match, mark NA

    # Any numeric token (fallback)
    anynum <- sub("^.*?([+-]?\\d[\\d,]*\\.?\\d*).*$", "\\1", s)
    anynum[anynum == s] <- NA_character_

    # Currency marker detection
    has_currency <- grepl("₱|PHP|php|\\$", s)

    chosen <- ifelse(!is.na(leading), leading,
                     ifelse(has_currency & !is.na(anynum), anynum, NA_character_))

    # Remove commas and coerce
    chosen_num <- as.numeric(gsub(",", "", chosen))
    chosen_num
  }

  # Apply cleaning to financial columns if they exist
  fin_cols <- c("ApprovedBudgetForContract", "ContractCost")
  for (col in fin_cols) {
    if (col %in% names(dt)) {
      clean_col <- paste0(col, "_clean")
      dt[[clean_col]] <- clean_currency_column(dt[[col]])
      fail_count <- sum(!is.na(dt[[col]]) & is.na(dt[[clean_col]]))
      cat(sprintf("Column %s: parsed %d -> numeric, failed to parse %d rows\n", col,
                  sum(!is.na(dt[[clean_col]])), fail_count))
    }
  }

  # Parse date columns simply (try common formats)
  date_cols <- intersect(c("StartDate", "ActualCompletionDate"), names(dt))
  for (cname in date_cols) {
    # Try YYYY-MM-DD then other common formats
    parsed <- as.IDate(dt[[cname]], tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%Y/%m/%d"))
    dt[[paste0(cname, "_parsed")]] <- parsed
    bad <- sum(!is.na(dt[[cname]]) & is.na(parsed))
    cat(sprintf("Date column %s: %d unparsable rows\n", cname, bad))
  }

  # Compute derived fields if cleaned numeric and parsed dates exist
  if (all(paste0(fin_cols, "_clean") %in% names(dt))) {
    dt[, CostSavings := get("ApprovedBudgetForContract_clean") - get("ContractCost_clean")]
    cat(sprintf("Computed CostSavings for %d rows (NA where inputs missing)\n", nrow(dt)))
  }

  if (all(c("StartDate_parsed", "ActualCompletionDate_parsed") %in% names(dt))) {
    dt[, CompletionDelayDays := as.integer(ActualCompletionDate_parsed - StartDate_parsed)]
    cat(sprintf("Computed CompletionDelayDays for %d rows (NA where dates missing)\n", nrow(dt)))
  }

  # Basic validation: detect rows with missing critical fields
  missing_latlong <- 0
  if (all(c("Latitude", "Longitude") %in% names(dt))) {
    missing_latlong <- sum(is.na(dt$Latitude) | is.na(dt$Longitude))
  }
  cat(sprintf("Rows with missing lat/long: %d\n", missing_latlong))

  # Filter to 2021-2023 if FundingYear exists
  if ("FundingYear" %in% names(dt)) {
    prefilter_count <- nrow(dt)
    dt_filtered <- dt[FundingYear >= 2021 & FundingYear <= 2023]
    cat(sprintf("Filtered projects 2021-2023: %d rows (from %d)\n", nrow(dt_filtered), prefilter_count))
  } else {
    dt_filtered <- dt
    cat("Warning: FundingYear column not found; no year filtering applied.\n")
  }

  # Return the cleaned/filtered data.table for downstream reporting
  return(dt_filtered)
}

#  Report 1: Regional Flood Mitigation Efficiency Summary. This table will
# have the following columns:
# ● aggregate total ApprovedBudgetForContract,
# ● median CostSavings,
# ● average CompletionDelayDays, and
# ● percentage of projects with delays >30 days by Region and MainIsland.
# Include "Efficiency Score", which is computed as
# (median savings / average delay) * 100, normalized to 0-100.
# Output as sorted CSV (descending by EfficiencyScore).
report_1 <- function() {

  cat("\nReport 1: Regional Flood Mitigation Efficiency Summary\n",
      "(Filtered: 2021-2023 Projects)\n\n")

}

# Provision to generate Report 2: Top Contractors Performance Ranking.
# Rank top 15 Contractors by total ContractCost (descending, filter >=5 projects), with
# columns for the following:
# ● number of projects,
# ● average CompletionDelayDays,
# ● total CostSavings,
# ● "Reliability Index", which is computed as (1 - (avg delay / 90)) * (total savings / total
# cost) * 100 (capped at 100). Flag <50 as "High Risk".
# Output as sorted CSV.
report_2 <- function() {
  cat("\nReport 2: Top Contractors Performance Ranking\n")
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