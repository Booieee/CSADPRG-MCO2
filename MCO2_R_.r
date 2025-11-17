# ********************
#  Last names: Lim, Sayat (Lead), Sia, Vanguardia
#  Language: R
#  Paradigm(s): Functional, Procedural
#  ********************
require(data.table)
require(knitr)

cost_saving <- function(approved_budget, contract_cost) {
  (approved_budget - contract_cost)
}

completion_delay_days <- function(start_date, actual_completion_date) {
  # Expect start_date and actual_completion_date to be Date or character; coerce safely
  s <- as.Date(start_date)
  a <- as.Date(actual_completion_date)
  as.integer(a - s)
}

process_dataset <- function(write_clean = FALSE, write_problems = FALSE,
                            clean_path = "dpwh_flood_control_projects_cleaned.csv",
                            problems_path = "dpwh_flood_control_parsing_problems.csv") {
  # Read CSV
  dt <- fread("dpwh_flood_control_projects.csv", na.strings = c("", "NA", "N/A"))
  setDT(dt)
  # ensure we have a writable, standalone data.table to avoid shallow-copy warnings when using :=
  dt <- copy(dt)
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

    # Leading numeric token (match at start)
    leading_pat <- "^\\s*([+-]?\\d[\\d,]*\\.?\\d*)"
    leading <- ifelse(grepl(leading_pat, s), sub(leading_pat, "\\1", s), NA_character_)

    # Any numeric token (fallback)
    anynum_pat <- "([+-]?\\d[\\d,]*\\.?\\d*)"
    anynum <- ifelse(grepl(anynum_pat, s), sub(paste0("^.*", anynum_pat, ".*$"), "\\1", s), NA_character_)

    # Currency marker detection
    has_currency <- grepl("₱|PHP|php|\\$", s)

  chosen <- ifelse(!is.na(leading) & leading != "NA", leading,
           ifelse(has_currency & !is.na(anynum) & anynum != "NA", anynum, NA_character_))

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
      # Show a few examples where parsing failed to help manual review
      if (fail_count > 0) {
        cat(sprintf("Examples of unparsed values in %s:\n", col))
        print(head(unique(dt[!is.na(get(col)) & is.na(get(clean_col)), get(col)]), 5))
      }
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
  # Normalize FundingYear to integer if possible
  if ("FundingYear" %in% names(dt)) dt[, FundingYear := as.integer(FundingYear)]

  # Fill missing Municipality with 'Unknown' (light-touch imputation)
  if ("Municipality" %in% names(dt)) dt[is.na(Municipality) | Municipality == "", Municipality := "Unknown"]

  # Impute ProjectLatitude/ProjectLongitude using province averages, fallback to ProvincialCapital coords
  if (all(c("ProjectLatitude", "ProjectLongitude", "Province") %in% names(dt))) {
    # compute province means where available
    prov_means <- dt[!is.na(ProjectLatitude) & !is.na(ProjectLongitude), .(
      prov_lat = mean(ProjectLatitude, na.rm = TRUE), prov_lon = mean(ProjectLongitude, na.rm = TRUE)
    ), by = Province]
    dt <- prov_means[dt, on = "Province"]
    # For rows with missing project coords, first try province mean, else provincial capital coords
    dt[is.na(ProjectLatitude), ProjectLatitude := ifelse(!is.na(prov_lat), prov_lat, ProvincialCapitalLatitude)]
    dt[is.na(ProjectLongitude), ProjectLongitude := ifelse(!is.na(prov_lon), prov_lon, ProvincialCapitalLongitude)]
    # drop helper columns
    dt[, c("prov_lat", "prov_lon") := NULL]
    missing_latlong <- sum(is.na(dt$ProjectLatitude) | is.na(dt$ProjectLongitude))
  } else {
    missing_latlong <- NA_integer_
  }
  cat(sprintf("Rows with missing project lat/long after imputation: %s\n", ifelse(is.na(missing_latlong), "N/A", as.character(missing_latlong))))

  # Filter to 2021-2023 if FundingYear exists
  if ("FundingYear" %in% names(dt)) {
    prefilter_count <- nrow(dt)
    dt_filtered <- dt[FundingYear >= 2021 & FundingYear <= 2023]
    cat(sprintf("Filtered projects 2021-2023: %d rows (from %d)\n", nrow(dt_filtered), prefilter_count))
  } else {
    dt_filtered <- dt
    cat("Warning: FundingYear column not found; no year filtering applied.\n")
  }

  # Final basic checks summary
  cat("\nBasic validation summary:\n")
  cat(sprintf(" Total rows after filter: %d\n", nrow(dt_filtered)))
  cat(sprintf(" Columns: %d\n", ncol(dt_filtered)))
  if ("ApprovedBudgetForContract_clean" %in% names(dt_filtered)) {
    cat(sprintf(" ApprovedBudgetForContract: %d numeric, %d unparsable\n",
                sum(!is.na(dt_filtered$ApprovedBudgetForContract_clean)),
                sum(!is.na(dt_filtered$ApprovedBudgetForContract) & is.na(dt_filtered$ApprovedBudgetForContract_clean))))
  }
  if ("ContractCost_clean" %in% names(dt_filtered)) {
    cat(sprintf(" ContractCost: %d numeric, %d unparsable\n",
                sum(!is.na(dt_filtered$ContractCost_clean)),
                sum(!is.na(dt_filtered$ContractCost) & is.na(dt_filtered$ContractCost_clean))))
  }
  if (all(c("StartDate_parsed", "ActualCompletionDate_parsed") %in% names(dt_filtered))) {
    cat(sprintf(" Unparsable StartDate: %d, Unparsable ActualCompletionDate: %d\n",
                sum(!is.na(dt_filtered$StartDate) & is.na(dt_filtered$StartDate_parsed)),
                sum(!is.na(dt_filtered$ActualCompletionDate) & is.na(dt_filtered$ActualCompletionDate_parsed))))
  }

  # Optionally write cleaned dataset and parsing-problems CSVs
  if (write_clean) {
    cat(sprintf("Writing cleaned dataset to %s ...\n", clean_path))
    fwrite(dt_filtered, file = clean_path)
    cat("Cleaned dataset written.\n")
  }

  # Build parsing-problems table: rows where any parsing failed but original had data
  problems_dt <- NULL
  problem_conditions <- rep(FALSE, nrow(dt_filtered))
  if ("ApprovedBudgetForContract" %in% names(dt_filtered) && "ApprovedBudgetForContract_clean" %in% names(dt_filtered)) {
    problem_conditions <- problem_conditions | (!is.na(dt_filtered$ApprovedBudgetForContract) & is.na(dt_filtered$ApprovedBudgetForContract_clean))
  }
  if ("ContractCost" %in% names(dt_filtered) && "ContractCost_clean" %in% names(dt_filtered)) {
    problem_conditions <- problem_conditions | (!is.na(dt_filtered$ContractCost) & is.na(dt_filtered$ContractCost_clean))
  }
  if ("StartDate" %in% names(dt_filtered) && "StartDate_parsed" %in% names(dt_filtered)) {
    problem_conditions <- problem_conditions | (!is.na(dt_filtered$StartDate) & is.na(dt_filtered$StartDate_parsed))
  }
  if ("ActualCompletionDate" %in% names(dt_filtered) && "ActualCompletionDate_parsed" %in% names(dt_filtered)) {
    problem_conditions <- problem_conditions | (!is.na(dt_filtered$ActualCompletionDate) & is.na(dt_filtered$ActualCompletionDate_parsed))
  }

  if (any(problem_conditions)) {
    problems_dt <- dt_filtered[problem_conditions]
    cat(sprintf("Found %d rows with parsing issues.\n", nrow(problems_dt)))
    if (write_problems) {
      cat(sprintf("Writing parsing-problems to %s ...\n", problems_path))
      fwrite(problems_dt, file = problems_path)
      cat("Parsing-problems CSV written.\n")
    }
  } else {
    cat("No parsing-problem rows found.\n")
  }

  # Return the cleaned/filtered data.table for downstream reporting
  # return list with cleaned table and problems (if any)
  return(list(clean = dt_filtered, problems = problems_dt))
}


# Implementation of Report 1: Regional Flood Mitigation Efficiency Summary
report_1 <- function(dt = NULL, out_path = "report1_regional_summary.csv", sample_n = 2) {
  cat("\nReport 1: Regional Flood Mitigation Efficiency Summary\n",
      "(Filtered: 2021-2023 Projects)\n")
  # dt: optional data.table; if NULL, use dt_cleaned in global env
  if (is.null(dt)) {
    if (exists("dt_cleaned", envir = .GlobalEnv)) {
      dt <- get("dt_cleaned", envir = .GlobalEnv)
    } else {
      stop("No data available. Run process_dataset() first or pass a data.table to report_1().")
    }
  }

  # required columns check
  req_cols <- c("Region", "MainIsland", "ApprovedBudgetForContract_clean", "CostSavings", "CompletionDelayDays")
  missing_cols <- setdiff(req_cols, names(dt))
  if (length(missing_cols) > 0) stop(sprintf("Missing required columns for report_1: %s", paste(missing_cols, collapse = ", ")))

  # copy to avoid modifying original
  DT <- copy(dt)

  # Aggregate by Region and MainIsland
  summary_dt <- DT[, .(
    totalBudget = sum(ApprovedBudgetForContract_clean, na.rm = TRUE),
    MedianSavings = median(CostSavings, na.rm = TRUE),
    AvgDelay = mean(CompletionDelayDays, na.rm = TRUE),
    HighDelayPct = if (.N > 0) sum(CompletionDelayDays > 30, na.rm = TRUE) / .N * 100 else NA_real_,
    Projects = .N
  ), by = .(Region, MainIsland)]

  # Compute raw score and normalize to 0-100
  # raw_score = (median savings / average delay) * 100 (NA when AvgDelay is 0 or NA)
  summary_dt[, raw_score := ifelse(is.na(AvgDelay) | AvgDelay == 0, NA_real_, (MedianSavings / AvgDelay) * 100)]

  if (all(is.na(summary_dt$raw_score))) {
    summary_dt[, EfficiencyScore := 0]
  } else {
    rs_min <- min(summary_dt$raw_score, na.rm = TRUE)
    rs_max <- max(summary_dt$raw_score, na.rm = TRUE)
    if (rs_max == rs_min) {
      summary_dt[, EfficiencyScore := 100]
    } else {
      summary_dt[, EfficiencyScore := (raw_score - rs_min) / (rs_max - rs_min) * 100]
    }
  }

  # Order descending by EfficiencyScore
  summary_dt <- summary_dt[order(-EfficiencyScore)]

  # Select final columns
  out_dt <- summary_dt[, .(Region, MainIsland, totalBudget, MedianSavings, AvgDelay, HighDelayPct, EfficiencyScore)]

  # Prepare writable table: format numeric fields to two-decimal strings so CSV shows two decimals
  out_dt_write <- copy(out_dt)
  numeric_cols <- c("totalBudget", "MedianSavings", "AvgDelay", "EfficiencyScore")
  for (cn in numeric_cols) {
    if (cn %in% names(out_dt_write)) out_dt_write[[cn]] <- sprintf("%.2f", as.numeric(out_dt_write[[cn]]))
  }
  # HighDelay percentage column with '%' appended
  if ("HighDelayPct" %in% names(out_dt_write)) {
    out_dt_write[, HighDelayPct := paste0(sprintf("%.2f", as.numeric(HighDelayPct)), "%")]
  }

  # Write CSV (numeric columns are formatted as strings with two decimals)
  fwrite(out_dt_write, file = out_path)

  # Print a small sample table (first sample_n rows) to console using knitr::kable
  require(knitr)
  if (nrow(out_dt_write) == 0) {
    cat("No groups to display in Report 1.\n")
  } else {
    sample_rows <- head(out_dt_write, sample_n)
    # For nicer console display, ensure numeric columns show two decimals as strings
    sample_print <- copy(sample_rows)
    for (cn in numeric_cols) {
      if (cn %in% names(sample_print)) sample_print[[cn]] <- sprintf("%.2f", as.numeric(sample_print[[cn]]))
    }
    print(kable(sample_print))
    cat(sprintf("\nFull report written to: %s\n", out_path))
  }

  invisible(out_dt_write)
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
report_2 <- function(dt = NULL, out_path = "report2_contractor_ranking.csv", sample_n = 2) {
  cat("\nReport 2: Top Contractors Performance Ranking\n")
  # dt: optional data.table; if NULL, use dt_cleaned in global env
  if (is.null(dt)) {
    if (exists("dt_cleaned", envir = .GlobalEnv)) {
      dt <- get("dt_cleaned", envir = .GlobalEnv)
    } else {
      stop("No data available. Run process_dataset() first or pass a data.table to report_2().")
    }
  }

  # required columns
  req_cols <- c("Contractor", "ContractCost_clean", "CompletionDelayDays", "CostSavings")
  missing_cols <- setdiff(req_cols, names(dt))
  if (length(missing_cols) > 0) stop(sprintf("Missing required columns for report_2: %s", paste(missing_cols, collapse = ", ")))

  DT <- copy(dt)

  # Aggregate by Contractor
  agg <- DT[, .(
    TotalCost = sum(ContractCost_clean, na.rm = TRUE),
    NumProjects = .N,
    AvgDelay = mean(CompletionDelayDays, na.rm = TRUE),
    TotalSaving = sum(CostSavings, na.rm = TRUE)
  ), by = Contractor]

  # Filter contractors with at least 5 projects
  agg <- agg[NumProjects >= 5]

  # Rank top 15 by TotalCost descending
  agg <- agg[order(-TotalCost)]
  if (nrow(agg) > 15) agg <- agg[1:15]

  # Compute ReliabilityIndex = (1 - (avg delay / 90)) * (total savings / total cost) * 100
  # Handle divide-by-zero and cap to 0-100
  agg[, ReliabilityIndex := ifelse(is.na(AvgDelay) | is.na(TotalCost) | TotalCost == 0,
                                   NA_real_, (1 - (AvgDelay / 90)) * (TotalSaving / TotalCost) * 100)]
  agg[, ReliabilityIndex := pmax(pmin(ReliabilityIndex, 100), 0, na.rm = TRUE)]

  # Risk flag: <50 -> High Risk, else Low Risk
  agg[, RiskFlag := ifelse(is.na(ReliabilityIndex), "Unknown", ifelse(ReliabilityIndex < 50, "High Risk", "Low Risk"))]

  # Add Rank
  agg[, Rank := seq_len(.N)]
  setcolorder(agg, c("Rank", "Contractor", "TotalCost", "NumProjects", "AvgDelay", "TotalSaving", "ReliabilityIndex", "RiskFlag"))

  # Prepare output: format numeric columns to 2 decimals and write CSV
  out <- copy(agg)
  num_cols <- c("TotalCost", "AvgDelay", "TotalSaving", "ReliabilityIndex")
  for (cn in num_cols) if (cn %in% names(out)) out[[cn]] <- sprintf("%.2f", as.numeric(out[[cn]]))

  fwrite(out, file = out_path)

  # Print sample_n rows (cap to available)
  require(knitr)
  if (nrow(out) == 0) {
    cat("No contractors meet the criteria for Report 2.\n")
  } else {
    sample_rows <- head(out, sample_n)
    print(kable(sample_rows))
    cat(sprintf("\nFull report written to: %s\n", out_path))
  }

  invisible(out)
}

main <- function() {
  # If running non-interactively (e.g., via Rscript), run processing once and exit.
  if (!interactive()) {
    cat("Non-interactive session detected. Running data processing once and writing outputs...\n")
    res <- process_dataset(write_clean = TRUE, write_problems = TRUE)
    if (!is.null(res$clean)) {
      assign("dt_cleaned", res$clean, envir = .GlobalEnv)
      cat("Cleaned data assigned to variable 'dt_cleaned' in global environment.\n")
    }
    if (!is.null(res$problems)) {
      assign("dt_parsing_problems", res$problems, envir = .GlobalEnv)
      cat("Parsing problems assigned to variable 'dt_parsing_problems' in global environment.\n")
    }
    return(invisible(NULL))
  }

  repeat {
    cat("\nSelect Language Implementation: \n",
        "[1] Load the file \n",
        "[2] Generate Reports \n",
        "[0] Exit\n\n")

    ans <- readline(prompt = "Enter Choice: ")
    ans <- trimws(ans)
    if (ans == "") next
    response <- suppressWarnings(as.integer(ans))
    if (is.na(response)) {
      cat("Please enter a valid number (0, 1, or 2).\n")
      next
    }

    if (response == 1) {
      cat("Loading and cleaning dataset now...\n")
      res <- process_dataset()
      if (!is.null(res$clean)) {
        assign("dt_cleaned", res$clean, envir = .GlobalEnv)
        cat("Cleaned data saved to variable 'dt_cleaned' for reporting.\n")
      }
      if (!is.null(res$problems)) {
        assign("dt_parsing_problems", res$problems, envir = .GlobalEnv)
        cat("Parsing-problem rows saved to variable 'dt_parsing_problems'.\n")
      }
    } else if (response == 2) {
      cat("Generating Report 1...\n")
      report_1()
      report_2()
    } else if (response == 0) {
      cat("Exiting program.\n")
      break
    } else {
      cat("Unknown option. Please select 0, 1, or 2.\n")
    }
  }
}

main()