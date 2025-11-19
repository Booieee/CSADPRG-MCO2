# ********************
#  Last names: Lim, Sayat (Lead), Sia, Vanguardia
#  Language: R
#  Paradigm(s): Functional, Procedural
#  ********************


# libraries
library(dplyr)
library(readr)
library(stringr)
library(jsonlite)

#=============== Usable Functions ==============

#the cost saving formula
cost_saving <- function(approved_budget, contract_cost) {
  return(approved_budget - contract_cost)
}

#delay days formula: days between StartDate and ActualCompletionDate
delay_days <- function(start_date, completion_date) {
  as.numeric(completion_date - start_date)
}


#============== Summary ========================
summary_report <- function(df) {

  summary <- list(
    total_projs = nrow(df),
    total_contractors = n_distinct(df$Contractor),
    total_provinces_with_projects = n_distinct(df$Province),
    global_average_delay = round(mean(df$CompletionDelayDays, na.rm = TRUE), 2),
    total_savings = round(sum(df$ApprovedBudget_num - df$ContractCost_num, na.rm = TRUE), 2)
  )

  #PREVIEW
  cat(toJSON(summary, pretty = TRUE, auto_unbox = TRUE))

  #EXPORT
  write_json(summary, "summary.json", pretty = TRUE, auto_unbox = TRUE)
}


#============== Generate Report 3 ==============
report_3 <- function(df) {
  cat("\nAnnual Project Type Cost Overrun Trends\n",
      "(Grouped by FundingYear and TypeOfWork)\n")

  # Summarize per year and type
  annual <- df %>%
    group_by(FundingYear, TypeOfWork) %>%
    summarise(
      TotalProjects = n(),
      AvgSavings = mean(cost_saving(ApprovedBudget_num, ContractCost_num), na.rm = TRUE),
      TotalApproved = sum(ApprovedBudget_num, na.rm = TRUE),
      TotalContract = sum(ContractCost_num, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      OverrunRate = ((TotalContract - TotalApproved) / TotalApproved) * 100
    )

  # Extract 2021 baseline per TypeOfWork
  baseline_2021 <- annual %>%
    filter(FundingYear == 2021) %>%
    select(TypeOfWork, AvgSavings) %>%
    rename(Baseline2021 = AvgSavings)

  # Join baseline to all years
  annual <- annual %>%
    left_join(baseline_2021, by = "TypeOfWork") %>%
    mutate(
      YoYChange = ifelse(
        !is.na(Baseline2021) & Baseline2021 != 0,
        ((AvgSavings - Baseline2021) / Baseline2021) * 100,
        0.0
      ),
      AvgSavings = format(round(AvgSavings, 2), nsmall = 2, big.mark = ","),
      OverrunRate = round(OverrunRate, 2),
      YoYChange = round(YoYChange, 2)
    ) %>%
    arrange(FundingYear, desc(AvgSavings)) %>%
    select(FundingYear, TypeOfWork, TotalProjects, AvgSavings, OverrunRate, YoYChange)

  #PREVIEW
  print(head(annual, 2))

  #EXPORT
  write_csv(annual, "report3_annual_trends.csv")
  cat("\nFull table exported to report2_contractor_ranking.csv\n")
}



#============== Generate Report 2 ==============
report_2 <- function(df) {
  cat("\nTop Contractors Performance Ranking\n",
      "(Top 15 by Total Cost, >=5 projects)\n")

  contractors <- df %>%
    group_by(Contractor) %>%
    summarise(
      NumProjects = n(),
      TotalCost = sum(ContractCost_num, na.rm = TRUE),
      AvgDelay = mean(CompletionDelayDays, na.rm = TRUE),
      TotalSavings = sum(cost_saving(ApprovedBudget_num, ContractCost_num), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(NumProjects >= 5) %>%
    arrange(desc(TotalCost)) %>%
    mutate(
      ReliabilityIndex = ((1 - (AvgDelay / 90)) * (TotalSavings / TotalCost)) * 100,
      ReliabilityIndex = pmin(ReliabilityIndex, 100),
      RiskFlag = ifelse(ReliabilityIndex < 50, "High Risk", "Low Risk"),

      Rank = row_number(),
      TotalCost = format(round(TotalCost, 2), nsmall = 2, big.mark = ","),
      AvgDelay = format(round(AvgDelay, 1), nsmall = 1, big.mark = ","),
      TotalSavings = format(round(TotalSavings, 2), nsmall = 2, big.mark = ","),
      ReliabilityIndex = round(ReliabilityIndex, 2)
    ) %>%
    select(Rank, Contractor, TotalCost, NumProjects, AvgDelay, TotalSavings, ReliabilityIndex, RiskFlag)

  #preview
  print(head(contractors, 2))

  #Export
  write_csv(head(contractors, 15), "report2_contractor_ranking.csv")
  cat("\nFull table exported to report2_contractor_ranking.csv\n")
}

#============== Generate Report 1 ==============
report_1 <- function(df) {

  cat("\nRegional Flood Mitigation Efficiency Summary\n",
      "(Filtered: 2021-2023 Projects)")

  regions <- df %>%
    group_by(Region, MainIsland) %>%
    summarise(
      TotalBudget = sum(ApprovedBudget_num, na.rm = TRUE),
      MedianSavings = median(cost_saving(ApprovedBudget_num, ContractCost_num), na.rm = TRUE),
      AvgDelay = mean(CompletionDelayDays, na.rm = TRUE),
      HighDelayPct = round(mean(CompletionDelayDays > 30, na.rm = TRUE) * 100, 2),
      .groups = "drop"
    ) %>%
    mutate(
      RawEfficiency = (MedianSavings / AvgDelay) * 100,
      #normalize to 0-100 = x - min(x) / max(x) - min(x) * 100
      EfficiencyScore = (RawEfficiency - min(RawEfficiency)) /
        (max(RawEfficiency) - min(RawEfficiency)) * 100,
      TotalBudget = format(round(TotalBudget, 2), nsmall = 2, big.mark = ","),
      MedianSavings = format(round(MedianSavings, 2), nsmall = 2, big.mark = ","),
      AvgDelay = format(round(AvgDelay, 1), nsmall = 1, big.mark = ","),
      HighDelayPct = paste0(HighDelayPct, "%"),
      EfficiencyScore = round(EfficiencyScore, 2)
    ) %>%
    arrange(desc(EfficiencyScore)) %>%
    select(Region, MainIsland, TotalBudget, MedianSavings, AvgDelay, HighDelayPct, EfficiencyScore)

  print(head(regions, 2))

  #write report1 in csv
  write_csv(regions, "report1_regional_summary.csv")
  cat("\nFull table exported to report1_regional_summary.csv\n")
}


#============== Data Ingestion =================
#function to detect non-numeric values
check_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

#function to detect errors in dates
check_date <- function(x, format = "%d-%m-%Y") {
  parsed <- suppressWarnings(as.Date(x, format = format))
  return(parsed)
}

process_dataset <- function(path = "dpwh_flood_control_projects.csv") {
  df_flood <- read_csv(path)

  count_rows <- nrow(df_flood)
  cat(paste("\nProcessing dataset...(", count_rows, "rows loaded, "))

  df_flood <- df_flood %>%
    filter(FundingYear >= 2021 & FundingYear <= 2023)

  rows_filtered <- nrow(df_flood)
  cat(paste(rows_filtered, "filtered for 2021-2023)\n"))

  #check for non-numeric in financial fields and dates
  df_flood <- df_flood %>%
    mutate(
      ApprovedBudget_num = ifelse(
        str_detect(ApprovedBudgetForContract, "Clustered|MYCA"),
        NA,
        check_numeric(ApprovedBudgetForContract)
      ),
      ContractCost_num = ifelse(
        str_detect(ContractCost, "Clustered|MYCA"),
        NA,
        check_numeric(ContractCost)
      ),
      StartDate_parsed = check_date(StartDate),
      ActualCompletionDate_parsed = check_date(ActualCompletionDate),
      CompletionDelayDays = delay_days(StartDate_parsed, ActualCompletionDate_parsed)
    )

  #identify bad rows
  bad_rows <- df_flood %>%
    filter(
      (is.na(ApprovedBudget_num) & !str_detect(ApprovedBudgetForContract, "Clustered|MYCA")) |
        (is.na(ContractCost_num) & !str_detect(ContractCost, "Clustered|MYCA"))
    )

  #for dates
  bad_date_rows <- df_flood %>%
    filter(is.na(StartDate_parsed) | is.na(ActualCompletionDate_parsed))


  if (nrow(bad_rows) > 0 | nrow(bad_date_rows) > 0) {
    cat("\nDetected invalid values:\n")
    print(bad_rows %>% select(ApprovedBudgetForContract, ContractCost))
    print(bad_date_rows %>% select(StartDate, ActualCompletionDate))
  } else {
    cat("\nNo numeric errors detected.\n")
  }

  return(df_flood)
}

#============== Main =================
main <- function() {
  data <- NULL

  repeat {
    cat("\nSelect Language Implementation: \n",
        "[1] Load the file \n",
        "[2] Generate Reports \n",
        "[0] Exit\n\n")
    ans <- readline(prompt = "Enter Choice: ")
    choice <- as.integer(ans)

    if (choice == 1) {
      if (is.null(data)) {
        data <- process_dataset()
      } else {
        cat("\nYou already loaded this file.\n")
        next
      }
    } else if (choice == 2) {
      if (is.null(data)) {
        cat("\nYou have to load the file first.\n")
        next
      }
      cat("\nGenerating Reports...\n",
          "Outputs saved to individual files...\n")

      cat("\nReport 1: Regional Flood Mitigation Efficiency Summary\n")
      report_1(data)
      cat("\nReport 2: Top Contractors Performance Ranking\n")
      report_2(data)
      cat("\nReport 3: Annual Project Type Cost Overrun Trends\n")
      report_3(data)
      cat("\nSummary Stats (summary.json):\n")
      summary_report(data)

      ans <- readline(prompt = "\n\nBack to Report Selection (Y/N): ")

      if (ans == "Y" || ans == "y") {
        next
      } else {
        cat("closing program...")
        break
      }
    }else if (choice == 0) {
      cat("closing program...")
      break
    } else {
      cat("Please enter a valid number (0, 1, or 2).\n")
    }
  }
}

main()