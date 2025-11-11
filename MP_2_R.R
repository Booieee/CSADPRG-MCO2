# ********************
#  Last names: Lim, Sayat, Sia, Vanguardia
#  Language: R
#  Paradigm(s): Functional, Procedural
#  ********************

# Global variables
exchange_rates <- list(PHP = 1.0, USD = 0.0, JPY = 0.0,
                       GBP = 0.0, EUR = 0.0, CNY = 0.0)

currencies <- c("[1] Philippine Peso (PHP)", "[2] US Dollar (USD)",
                "[3] Japanese Yen (JPY)", "[4] British Pound (GBP)",
                "[5] Euro (EUR)", "[6] Chinese Yuan (CNY)")

currency_codes <- c("PHP", "USD", "JPY", "GBP", "EUR", "CNY")

# ============= CHOOSE ACCOUNT =============
choose_account <- function(accounts) {
  if (length(accounts) == 0) {
    cat("\nMake an account first!\n")
    return(NULL)
  }

  cat("\nAccounts:\n")
  for (i in seq_along(accounts)) {
    cat(sprintf("[%d] %s\n", i, accounts[[i]]$name))
  }

  choice <- as.integer(readline(prompt = "Select Account: "))

  if (is.na(choice) || choice < 1 || choice > length(accounts)) {
    cat("Invalid selection!\n")
    return(NULL)
  }

  choice
}

# ============= MAIN MENU =============
main_menu <- function(accounts = list(), rate = exchange_rates) {
  repeat {
    cat("\nSelect Transaction\n",
        "[1] Register Account Name\n",
        "[2] Deposit Amount\n",
        "[3] Withdraw Amount\n",
        "[4] Currency Exchange\n",
        "[5] Record Exchange Rates\n",
        "[6] Show Interest Computation\n",
        "[0] Exit\n")

    response <- as.integer(readline(prompt = "Enter Choice: "))

    if (response == 0) {
      break
    } else if (response == 1) {
      accounts <- register_acc_name(accounts)
    } else if (response == 2) {
      if (length(accounts) == 0) {
        cat("Please register an account first!\n")
      } else {
        accounts <- deposit_amount(accounts)
      }
    } else if (response == 3) {
      if (length(accounts) == 0) {
        cat("Please register an account first!\n")
      } else {
        accounts <- withdraw_amount(accounts)
      }
    } else if (response == 4) {
      rate <- currency_exchange(rate)
    } else if (response == 5) {
      rate <- record_exchange_rate(rate)
    } else if (response == 6) {
      if (length(accounts) == 0) {
        cat("Please register an account first!\n")
      } else {
        accounts <- show_interest_amount(accounts)
      }
    } else {
      cat("Invalid Option!\n")
    }
  }
}

# ============= REGISTER =============
register_acc_name <- function(accounts) {
  cat("\nRegister Account Name\n")
  name <- readline("Account Name: ")
  acc <- list(name = name, balance = 0, currency = "PHP")

  accounts[[length(accounts) + 1]] <- acc
  cat(sprintf("Welcome, %s! You have been registered.\n", name))

  response <- readline(prompt = "Back to Main Menu (Y/N): ")

  if (response == "Y" || response == "y") {
    accounts
  } else if (response == "N" || response == "n") {
    register_acc_name(accounts)
  } else {
    accounts
  }
}

# ============= DEPOSIT =============
deposit_amount <- function(acct) {
  idx <- choose_account(acct)
  if (is.null(idx)) return(acct)

  repeat {
    cat("\nDeposit Amount\n",
        paste("Account Name:", acct[[idx]]$name), "\n",
        paste("Current Balance:", sprintf("%.2f", acct[[idx]]$balance)), "\n",
        paste("Currency:", acct[[idx]]$currency), "\n")

    amount <- as.numeric(readline(prompt = "Deposit Amount: "))

    if (!is.na(amount) && amount > 0) {
      acct[[idx]]$balance <- acct[[idx]]$balance + amount
      cat(paste("Updated Balance:", sprintf("%.2f", acct[[idx]]$balance)), "\n")
    } else {
      cat("Invalid amount!\n")
    }

    response <- readline(prompt = "Back to Main Menu (Y/N): ")

    if (response == "Y" || response == "y") {
      break
    }
  }

  acct
}

# ============= WITHDRAW =============
withdraw_amount <- function(acct) {
  idx <- choose_account(acct)
  if (is.null(idx)) return(acct)

  repeat {
    cat("\nWithdraw Amount\n",
        paste("Account Name:", acct[[idx]]$name), "\n",
        paste("Current Balance:", sprintf("%.2f", acct[[idx]]$balance)), "\n",
        paste("Currency:", acct[[idx]]$currency), "\n")

    amount <- as.numeric(readline(prompt = "Withdraw Amount: "))

    if (!is.na(amount) && amount > 0 && amount <= acct[[idx]]$balance) {
      acct[[idx]]$balance <- acct[[idx]]$balance - amount
      cat(paste("Updated Balance:", sprintf("%.2f", acct[[idx]]$balance)), "\n")
    } else if (!is.na(amount) && amount > acct[[idx]]$balance) {
      cat("Insufficient funds!\n")
    } else {
      cat("Invalid amount!\n")
    }

    response <- readline(prompt = "Back to Main Menu (Y/N): ")
    if (response == "Y" || response == "y") {
      break
    }
  }

  acct
}

# ============= RECORD EXCHANGE RATE =============
record_exchange_rate <- function(rate) {
  cat("Record Exchange Rate\n")
  for (i in seq_along(currencies)) {
    cat(currencies[i], "\n")
  }

  get_num <- as.integer(readline(prompt = "Select Foreign Currency: "))
  rate_value <- as.numeric(readline(prompt = "Exchange Rate: "))

  if (get_num == 1) {
    rate$PHP <- rate_value
  } else if (get_num == 2) {
    rate$USD <- rate_value
  } else if (get_num == 3) {
    rate$JPY <- rate_value
  } else if (get_num == 4) {
    rate$GBP <- rate_value
  } else if (get_num == 5) {
    rate$EUR <- rate_value
  } else if (get_num == 6) {
    rate$CNY <- rate_value
  }

  response <- readline(prompt = "Back to Main Menu (Y/N): ")
  if (response == "Y" || response == "y") {
    rate
  } else if (response == "N" || response == "n") {
    return(record_exchange_rate(rate))
  }
  rate
}


show_interest_amount <- function(acct) {
  idx <- choose_account(acct)
  if (is.null(idx)) return(acct)

  repeat {
    cat("\nShow Interest Amount\n")
    cat(paste("Account Name:", acct[[idx]]$name), "\n")
    cat(paste("Current Balance:", sprintf("%.2f", acct[[idx]]$balance)), "\n")
    cat(paste("Currency:", acct[[idx]]$currency), "\n")

    interest_rate <- 0.05
    cat(paste("Interest Rate:", paste0(interest_rate * 100, "%")), "\n\n")

    total_days <- as.numeric(readline(prompt = "Total Number of Days: "))

    if (!is.na(total_days) && total_days > 0) {
      cat(paste("Total Number of Days:", paste0(total_days, " days")), "\n\n")

      cat(sprintf("%-5s | %-10s | %-10s\n", "Day", "Interest", "Balance"))
      cat(strrep("-", 35), "\n")

      current_balance <- acct[[idx]]$balance

      for (day in 1:total_days) {
        # Daily Interest = (End-of-Day Balance) x (Annual Interest Rate / 365)
        daily_interest <- current_balance * (interest_rate / 365)
        current_balance <- current_balance + daily_interest

        # Print the day, interest, and balance
        cat(sprintf("%-5d | %-10s | %-10s\n",
                    day,
                    sprintf("%.2f", daily_interest),
                    sprintf("%.2f", current_balance)))
      }

    } else {
      cat("Invalid number of days!\n")
    }

    response <- readline(prompt = "Back to Main Menu (Y/N): ")

    if (response == "Y" || response == "y") {
      break
    }
  }

  acct
}


currency_exchange <- function(rate) {
  repeat {
    cat("\nForeign Currency Exchange\n",
        "Source Currency Option:\n")
    for (i in seq_along(currencies)) {
      cat(currencies[i], "\n")
    }

    source <- as.numeric(readline(prompt = "Source Currency: "))
    amount <- as.numeric(readline(prompt = "Source Amount: "))

    cat("Exchange Currency Options: \n")
    for (i in seq_along(currencies)) {
      cat(currencies[i], "\n")
    }

    destination <- as.numeric(readline(prompt = "Exchange Currency: "))

    if (is.na(source) || is.na(destination) || source < 1 || destination < 1 ||
          source > length(currencies) || destination > length(currencies)) {
      cat("Invalid currency selection!\n")
      next
    }

    # Map menu selection to currency codes for rate lookup
    src_code <- currency_codes[source]
    dst_code <- currency_codes[destination]

    src_rate <- rate[[src_code]]
    dst_rate <- rate[[dst_code]]

    # Validate rates and amount
    if (is.null(src_rate) || is.null(dst_rate) || is.na(src_rate) || is.na(dst_rate) ||
          is.na(amount) || amount <= 0 || dst_rate <= 0) {
      cat("Please record valid exchange rates first!\n")
      next
    }

    dst_amt <- amount * src_rate / dst_rate
    cat(paste("Exchanged Amount:", sprintf("%.2f", dst_amt)), "\n\n")

    convert_response <- readline(prompt = "Convert another amount (Y/N): ")
    if (convert_response == "Y" || convert_response == "y") {
      next
    } else {
      break
    }
  }

  rate
}

main_menu()
