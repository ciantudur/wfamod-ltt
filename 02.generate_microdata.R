# ---------------------------------------------------------------------------- #
# WFAMOD-LTT ----------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# 02.generate_microdata.R

# DATE CREATED:  21 September 2022
# LAST MODIFIED: 12 October 2022
# AUTHOR: Cian Sion


# DESCRIPTION:
# This script uses WRA price bin data to simulate transactions and generate the
# model microdata.

# CAUTIONS:
# ! Remember to set working directory using setwd().
# ! Remember to define base_year object before running script.



# 2.01 INITIALIZE SCRIPT -------------------------------------------------------

## Load required packages
packages_required <- c("stats", "dplyr")
for (pkg in packages_required) {
  require(pkg, character.only = TRUE)
}

## Set a base year (FYE) for the model (e.g. use 2022 for 2021-22 fiscal year)
base_year <- 2022



# 2.02 LOAD INPUT DATA ---------------------------------------------------------

## Load cleaned input data from sub-directory
wra_data_cleaned <- read.csv("data/temp/wra_data_cleaned.csv")

## Subset transactions from base year
wra_data_cleaned <- wra_data_cleaned[wra_data_cleaned$baseYear == base_year, ]



# 2.03 GENERATE RESIDENTIAL (MAIN) MICRODATA -----------------------------------

## Set seed to ensures randomly-generated values are reproducible
set.seed(100001)

## Subset residential transactions
wra_data_rm <- wra_data_cleaned[
  wra_data_cleaned$transactionTypeCode == "RM",
]

## Generate vector of unique residential price bins for use in loop
value_bin_codes <- unique(wra_data_rm$valueBinCode)

## Generate empty vector to store loop output
transactions_rm <- NULL

## For each price bin, generate transactions from probability distribution
for (i in seq_along(value_bin_codes)) {
  price_bin_no <- value_bin_codes[i]

  ## Define variables for use in loop
  low_bin <- as.numeric(wra_data_rm$lowBin[
    wra_data_rm$valueBinCode == price_bin_no &
      wra_data_rm$measureCode == "C"
  ])
  high_bin <- as.numeric(wra_data_rm$highBin[
    wra_data_rm$valueBinCode == price_bin_no &
      wra_data_rm$measureCode == "C"
  ])
  transaction_n <- as.numeric(wra_data_rm$dataValue[
    wra_data_rm$valueBinCode == price_bin_no &
      wra_data_rm$measureCode == "C"
  ])
  bin_value <- as.numeric(wra_data_rm$dataValue[
    wra_data_rm$valueBinCode == price_bin_no &
      wra_data_rm$measureCode == "V"
  ])

  ## If price bin is unbounded, assume high_bin is twice the mean value
  if (is.na(high_bin) == TRUE) {
    transaction_temp <- NULL
    mean <- (as.numeric(bin_value) * 1000000 / as.numeric(transaction_n))
    high_bin <- mean * 2
    standard_mean <- (mean - low_bin) / (high_bin - low_bin)
    alpha <- 1
    beta <- (alpha * (1 - standard_mean)) / standard_mean
    transaction_temp <- rbeta(transaction_n, alpha, beta)
    transaction_temp <- (transaction_temp * (high_bin - low_bin)) + low_bin
    transactions_rm <- append(transactions_rm, transaction_temp)

    ## If high_bin is 0, generate vector of 0s with length transaction_n
  } else if (high_bin == 0) {
    transaction_temp <- rep(0, transaction_n)
    transactions_rm <- append(transactions_rm, transaction_temp)
  } else {
    if (is.na(bin_value) == TRUE) {
      mean <- ((high_bin - low_bin) / 2) + low_bin
    } else {
      mean <- (as.numeric(bin_value) * 1000000 / as.numeric(transaction_n))
    }

## If mean is outside price bin, replace with mean from inside the domain
if (mean >= high_bin) {
  mean <- high_bin - 1000
} else if (mean <= low_bin) {
  mean <- low_bin + 1000
} else {
}

    if (is.na(high_bin) == FALSE) {
      # Standardize mean bin transaction to [0,1] domain
      standard_mean <- (mean - low_bin) / (high_bin - low_bin)

      if (standard_mean > 0.5) {
        alpha <- (-standard_mean) / (standard_mean - 1)
        beta <- 1 # Beta(a > 1, b = 1) is negatively skewed
        transaction_temp <- rbeta(transaction_n, alpha, beta)
        transaction_temp <- (transaction_temp * (high_bin - low_bin)) + low_bin
        transactions_rm <- append(
          transactions_rm,
          transaction_temp
        )
      } else if (standard_mean < 0.5) {
        alpha <- 1 # Beta(a = 1, b > 1) is positively skewed
        beta <- (1 / standard_mean) - 1
        transaction_temp <- rbeta(transaction_n, alpha, beta)
        transaction_temp <- (transaction_temp * (high_bin - low_bin)) + low_bin
        transactions_rm <- append(
          transactions_rm,
          transaction_temp
        )
      } else {
        alpha <- 1 # Beta(a = 1, b = 1) is equivalent to a uniform distribution
        beta <- 1
        transaction_temp <- rbeta(transaction_n, alpha, beta)
        transaction_temp <- (transaction_temp * (high_bin - low_bin)) + low_bin
        transactions_rm <- append(
          transactions_rm,
          transaction_temp
        )
      }
    }
  }
}

## Smooth upper tail of distribution (£300,000 - £750,000) using LOESS
transaction_id <- seq(from = 1, to = length(transactions_rm), by = 1)
transactions_rm <- sort(transactions_rm)
rm_smoothing <- data.frame(transaction_id, transactions_rm)
rm_smoothing <- rm_smoothing[
  rm_smoothing$transactions_rm > 300001 &
    rm_smoothing$transactions_rm < 750000,
]
transaction_res_length <- length(
  rm_smoothing$transactions_rm
)
rm_smoothing$transaction_id <- seq(
  from = 1,
  to = transaction_res_length,
  by = 1
)
loess_res <- loess(rm_smoothing$transactions_rm ~
rm_smoothing$transaction_id, span = 0.5)
smoothed_temp1 <- predict(loess_res)
smoothed_temp <- (smoothed_temp1 - min(smoothed_temp1)) /
  (max(smoothed_temp1) - min(smoothed_temp1))
smoothed_values <- ((750000 - 300001) * smoothed_temp) + 300001
transactions_rm[transactions_rm > 300001 &
  transactions_rm < 750000] <- smoothed_values

## Draw histogram of residential transactions
hist(transactions_rm, breaks = 200)



# 2.04 GENERATE RESIDENTIAL (HIGHER) MICRODATA ---------------------------------

## Set seed to ensures randomly-generated values are reproducible
set.seed(200001)

## Subset residential transactions
wra_data_rh <- wra_data_cleaned[
  wra_data_cleaned$transactionTypeCode == "RH",
]

## Generate vector of unique residential price bins for use in loop
value_bin_codes <- unique(wra_data_rh$valueBinCode)

## Generate empty vector to store loop output
transactions_rh <- NULL

## For each price bin, generate transactions from probability distribution
for (i in seq_along(value_bin_codes)) {
  price_bin_no <- value_bin_codes[i]

  ## Define variables for use in loop
  low_bin <- as.numeric(wra_data_rh$lowBin[
    wra_data_rh$valueBinCode == price_bin_no &
      wra_data_rh$measureCode == "C"
  ])
  high_bin <- as.numeric(wra_data_rh$highBin[
    wra_data_rh$valueBinCode == price_bin_no &
      wra_data_rh$measureCode == "C"
  ])
  transaction_n <- as.numeric(wra_data_rh$dataValue[
    wra_data_rh$valueBinCode == price_bin_no &
      wra_data_rh$measureCode == "C"
  ])
  bin_value <- as.numeric(wra_data_rh$dataValue[
    wra_data_rh$valueBinCode == price_bin_no &
      wra_data_rh$measureCode == "V"
  ])

  ## If price bin is unbounded, assume high_bin is 3x the mean value
  if (is.na(high_bin) == TRUE) {
    transaction_temp <- NULL
    mean <- (as.numeric(bin_value) * 1000000 / as.numeric(transaction_n))
    high_bin <- mean * 3
    standard_mean <- (mean - low_bin) / (high_bin - low_bin)
    alpha <- 1
    beta <- (alpha * (1 - standard_mean)) / standard_mean
    transaction_temp <- rbeta(transaction_n, alpha, beta)
    transaction_temp <- (transaction_temp * (high_bin - low_bin)) + low_bin
    transactions_rh <- append(transactions_rh, transaction_temp)

    ## If high_bin is 0, generate vector of 0s with length transaction_n
  } else if (high_bin == 0) {
    transaction_temp <- rep(0, transaction_n)
    transactions_rh <- append(transactions_rh, transaction_temp)
  } else {
    if (is.na(bin_value) == TRUE) {
      mean <- ((high_bin - low_bin) / 2) + low_bin
    } else {
      mean <- (as.numeric(bin_value) * 1000000 / as.numeric(transaction_n))
    }

    ## If mean is outside price bin, replace with mean from inside the domain
    if (mean >= high_bin) {
      mean <- high_bin - 1000
    } else if (mean <= low_bin) {
      mean <- low_bin + 1000
    } else {
    }

    if (is.na(high_bin) == FALSE) {
      # Standardize mean bin transaction to [0,1] domain
      standard_mean <- (mean - low_bin) / (high_bin - low_bin)

      if (standard_mean > 0.5) {
        alpha <- (-standard_mean) / (standard_mean - 1)
        beta <- 1 # Beta(a > 1, b = 1) is negatively skewed
        transaction_temp <- rbeta(transaction_n, alpha, beta)
        transaction_temp <- (transaction_temp * (high_bin - low_bin)) + low_bin
        transactions_rh <- append(
          transactions_rh,
          transaction_temp
        )
      } else if (standard_mean < 0.5) {
        alpha <- 1 # Beta(a = 1, b > 1) is positively skewed
        beta <- (1 / standard_mean) - 1
        transaction_temp <- rbeta(transaction_n, alpha, beta)
        transaction_temp <- (transaction_temp * (high_bin - low_bin)) + low_bin
        transactions_rh <- append(
          transactions_rh,
          transaction_temp
        )
      } else {
        alpha <- 1 # Beta(a = 1, b = 1) is equivalent to uniform distribution
        beta <- 1
        transaction_temp <- rbeta(transaction_n, alpha, beta)
        transaction_temp <- (transaction_temp * (high_bin - low_bin)) + low_bin
        transactions_rh <- append(
          transactions_rh,
          transaction_temp
        )
      }
    }
  }
}

## Smooth upper tail of distribution (£450,000 - £750,000) using LOESS
transaction_id <- seq(from = 1, to = length(transactions_rh), by = 1)
transactions_rh <- sort(transactions_rh)
rh_smoothing <- data.frame(transaction_id, transactions_rh)
rh_smoothing <- rh_smoothing[
  rh_smoothing$transactions_rh > 450001 &
    rh_smoothing$transactions_rh < 750000,
]
transaction_res_length <- length(
  rh_smoothing$transactions_rh
)
rh_smoothing$transaction_id <- seq(
  from = 1,
  to = transaction_res_length,
  by = 1
)
loess_res <- loess(rh_smoothing$transactions_rh ~
rh_smoothing$transaction_id, span = 0.5)
smoothed_temp1 <- predict(loess_res)
smoothed_temp <- (smoothed_temp1 - min(smoothed_temp1)) /
  (max(smoothed_temp1) - min(smoothed_temp1))
smoothed_values <- ((750000 - 450001) * smoothed_temp) + 450001
transactions_rh[transactions_rh > 450001 &
  transactions_rh < 750000] <- smoothed_values

## Draw histogram of residential transactions
hist(transactions_rh, breaks = 200)



# 2.05 GENERATE NON-RESIDENTIAL PREMIUM MICRODATA ------------------------------

## Set seed to ensures randomly-generated values are reproducible
set.seed(300001)

## Subset non-residential premium transactions
wra_data_nrp <- wra_data_cleaned[
  wra_data_cleaned$transactionTypeCode == "NRP",
]

## Generate vector of unique residential price bins for use in loop
value_bin_codes <- unique(wra_data_nrp$valueBinCode)

## Generate empty vector to store loop output
transactions_nrp <- NULL

## For each price bin, generate transactions from probability distribution
for (i in seq_along(value_bin_codes)) {
  price_bin_no <- value_bin_codes[i]

  ## Define variables for use in loop
  low_bin <- as.numeric(wra_data_nrp$lowBin[
    wra_data_nrp$valueBinCode == price_bin_no &
      wra_data_nrp$measureCode == "C"
  ])
  high_bin <- as.numeric(wra_data_nrp$highBin[
    wra_data_nrp$valueBinCode == price_bin_no &
      wra_data_nrp$measureCode == "C"
  ])
  transaction_n <- as.numeric(wra_data_nrp$dataValue[
    wra_data_nrp$valueBinCode == price_bin_no &
      wra_data_nrp$measureCode == "C"
  ])
  bin_value <- as.numeric(wra_data_nrp$dataValue[
    wra_data_nrp$valueBinCode == price_bin_no &
      wra_data_nrp$measureCode == "V"
  ])

  ## If price bin is unbounded, assume high_bin is twice the mean value
  if (is.na(high_bin) == TRUE) {
    transaction_temp <- NULL
    mean <- (as.numeric(bin_value) * 1000000 / as.numeric(transaction_n))
    high_bin <- mean * 2
    standard_mean <- (mean - low_bin) / (high_bin - low_bin)
    alpha <- 1
    beta <- (alpha * (1 - standard_mean)) / standard_mean
    transaction_temp <- rbeta(transaction_n, alpha, beta)
    transaction_temp <- (transaction_temp * (high_bin - low_bin)) + low_bin
    transactions_nrp <- append(transactions_nrp, transaction_temp)

    ## If high_bin is 0, generate vector of 0s with length transaction_n
  } else if (high_bin == 0) {
    transaction_temp <- rep(0, transaction_n)
    transactions_nrp <- append(transactions_nrp, transaction_temp)
  } else {
    if (is.na(bin_value) == TRUE) {
      mean <- ((high_bin - low_bin) / 2) + low_bin
    } else {
      mean <- (as.numeric(bin_value) * 1000000 / as.numeric(transaction_n))
    }

    ## If mean is outside price bin, replace with mean from inside the domain
    if (mean >= high_bin) {
      mean <- high_bin - 500
    } else if (mean <= low_bin) {
      mean <- low_bin + 500
    } else {
    }

    if (is.na(high_bin) == FALSE) {
      # Standardize mean bin transaction to [0,1] domain
      standard_mean <- (mean - low_bin) / (high_bin - low_bin)

      if (standard_mean > 0.5) {
        alpha <- (-standard_mean) / (standard_mean - 1)
        beta <- 1 # Beta(a > 1, b = 1) is negatively skewed
        transaction_temp <- rbeta(transaction_n, alpha, beta)
        transaction_temp <- (transaction_temp * (high_bin - low_bin)) + low_bin
        transactions_nrp <- append(
          transactions_nrp,
          transaction_temp
        )
      } else if (standard_mean < 0.5) {
        alpha <- 1 # Beta(a = 1, b > 1) is positively skewed
        beta <- (1 / standard_mean) - 1
        transaction_temp <- rbeta(transaction_n, alpha, beta)
        transaction_temp <- (transaction_temp * (high_bin - low_bin)) + low_bin
        transactions_nrp <- append(
          transactions_nrp,
          transaction_temp
        )
      } else {
        alpha <- 1 # Beta(a = 1, b = 1) is equivalent to uniform distribution
        beta <- 1
        transaction_temp <- rbeta(transaction_n, alpha, beta)
        transaction_temp <- (transaction_temp * (high_bin - low_bin)) + low_bin
        transactions_nrp <- append(
          transactions_nrp,
          transaction_temp
        )
      }
    }
  }
}

## Smooth upper tail of distribution (£400,000 - £1,000,000) using LOESS
transaction_id <- seq(from = 1, to = length(transactions_nrp), by = 1)
transactions_nrp <- sort(transactions_nrp)
nrp_smoothing <- data.frame(transaction_id, transactions_nrp)
nrp_smoothing <- nrp_smoothing[
  nrp_smoothing$transactions_nrp > 400001 &
    nrp_smoothing$transactions_nrp < 1000000,
]
transaction_res_length <- length(
  nrp_smoothing$transactions_nrp
)
nrp_smoothing$transaction_id <- seq(
  from = 1,
  to = transaction_res_length,
  by = 1
)
loess_res <- loess(nrp_smoothing$transactions_nrp ~
nrp_smoothing$transaction_id, span = 0.5)
smoothed_values_temp1 <- predict(loess_res)
smoothed_values_temp2 <- (smoothed_values_temp1 - min(smoothed_values_temp1)) /
  (max(smoothed_values_temp1) - min(smoothed_values_temp1))
smoothed_values <- ((1000000 - 400001) * smoothed_values_temp2) + 400001
transactions_nrp[transactions_nrp > 400001 &
  transactions_nrp < 1000000] <- smoothed_values

## Draw histogram of residential transactions
hist(transactions_nrp, breaks = 200)



# 2.06 GENERATE NON-RESIDENTIAL RENTAL MICRODATA -------------------------------

## Set seed to ensures randomly-generated values are reproducible
set.seed(400001)

## Subset non-residential premium transactions
wra_data_nrn <- wra_data_cleaned[
  wra_data_cleaned$transactionTypeCode == "NRN",
]

## Generate vector of unique residential price bins for use in loop
value_bin_codes <- unique(wra_data_nrn$valueBinCode)

## Generate empty vector to store loop output
transactions_nrn <- NULL

## For each price bin, generate transactions from probability distribution
for (i in seq_along(value_bin_codes)) {
  price_bin_no <- value_bin_codes[i]

  ## Define variables for use in loop
  low_bin <- as.numeric(wra_data_nrn$lowBin[
    wra_data_nrn$valueBinCode == price_bin_no &
      wra_data_nrn$measureCode == "C"
  ])
  high_bin <- as.numeric(wra_data_nrn$highBin[
    wra_data_nrn$valueBinCode == price_bin_no &
      wra_data_nrn$measureCode == "C"
  ])
  transaction_n <- as.numeric(wra_data_nrn$dataValue[
    wra_data_nrn$valueBinCode == price_bin_no &
      wra_data_nrn$measureCode == "C"
  ])
  bin_value <- as.numeric(wra_data_nrn$dataValue[
    wra_data_nrn$valueBinCode == price_bin_no &
      wra_data_nrn$measureCode == "N"
  ])

  if (length(bin_value) == 0) {
    bin_value <- 0
  }


  ## If price bin is unbounded, assume high_bin is twice the mean value
  if (is.na(high_bin) == TRUE) {
    transaction_temp <- NULL
    mean <- (as.numeric(bin_value) * 1000000 / as.numeric(transaction_n))
    high_bin <- mean * 2
    standard_mean <- (mean - low_bin) / (high_bin - low_bin)
    alpha <- 1
    beta <- (alpha * (1 - standard_mean)) / standard_mean
    transaction_temp <- rbeta(transaction_n, alpha, beta)
    transaction_temp <- (transaction_temp * (high_bin - low_bin)) + low_bin
    transactions_nrn <- append(transactions_nrn, transaction_temp)

    ## If high_bin is 0, generate vector of 0s with length transaction_n
  } else if (high_bin == 0) {
    transaction_temp <- rep(0, transaction_n)
    transactions_nrn <- append(transactions_nrn, transaction_temp)
  } else {
    if (is.na(bin_value) == TRUE) {
      mean <- ((high_bin - low_bin) / 2) + low_bin
    } else {
      mean <- (as.numeric(bin_value) * 1000000 / as.numeric(transaction_n))
    }

    ## If mean is outside price bin, replace with mean from inside the domain
    if (mean >= high_bin) {
      mean <- high_bin - 500
    } else if (mean <= low_bin) {
      mean <- low_bin + 500
    } else {
    }

    if (is.na(high_bin) == FALSE) {
      # Standardize mean bin transaction to [0,1] domain
      standard_mean <- (mean - low_bin) / (high_bin - low_bin)

      if (standard_mean > 0.5) {
        alpha <- (-standard_mean) / (standard_mean - 1)
        beta <- 1 # Beta(a > 1, b = 1) is negatively skewed
        transaction_temp <- rbeta(transaction_n, alpha, beta)
        transaction_temp <- (transaction_temp * (high_bin - low_bin)) + low_bin
        transactions_nrn <- append(
          transactions_nrn,
          transaction_temp
        )
      } else if (standard_mean < 0.5) {
        alpha <- 1 # Beta(a = 1, b > 1) is positively skewed
        beta <- (1 / standard_mean) - 1
        transaction_temp <- rbeta(transaction_n, alpha, beta)
        transaction_temp <- (transaction_temp * (high_bin - low_bin)) + low_bin
        transactions_nrn <- append(
          transactions_nrn,
          transaction_temp
        )
      } else {
        alpha <- 1 # Beta(a = 1, b = 1) is equivalent to uniform distribution
        beta <- 1
        transaction_temp <- rbeta(transaction_n, alpha, beta)
        transaction_temp <- (transaction_temp * (high_bin - low_bin)) + low_bin
        transactions_nrn <- append(
          transactions_nrn,
          transaction_temp
        )
      }
    }
  }
}

## Smooth upper tail of distribution (£200,000 - £2,000,000) using LOESS
transaction_id <- seq(from = 1, to = length(transactions_nrn), by = 1)
transactions_nrn <- sort(transactions_nrn)
nrn_smoothing <- data.frame(transaction_id, transactions_nrn)
nrn_smoothing <- nrn_smoothing[
  nrn_smoothing$transactions_nrn > 200001 &
    nrn_smoothing$transactions_nrn < 2000000,
]
transaction_res_length <- length(
  nrn_smoothing$transactions_nrn
)
nrn_smoothing$transaction_id <- seq(
  from = 1,
  to = transaction_res_length,
  by = 1
)
loess_res <- loess(nrn_smoothing$transactions_nrn ~
nrn_smoothing$transaction_id, span = 0.5)
smoothed_values_temp1 <- predict(loess_res)
smoothed_values_temp2 <- (smoothed_values_temp1 - min(smoothed_values_temp1)) /
  (max(smoothed_values_temp1) - min(smoothed_values_temp1))
smoothed_values <- ((2000000 - 200001) * smoothed_values_temp2) + 200001
transactions_nrn[transactions_nrn > 200001 &
  transactions_nrn < 200001] <- smoothed_values

## Draw histogram of residential transactions
hist(transactions_nrn, breaks = 200)



# 2.07 CLEAN AND SAVE RESULTS --------------------------------------------------

## Combine and merge data for each transaction type
microdata <- as.data.frame(transactions_rm)
colnames(microdata) <- "transactionValue"
microdata$transactionTypeCode <- "RM"

rh <- as.data.frame(transactions_rh)
colnames(rh) <- "transactionValue"
rh$transactionTypeCode <- "RH"

nrp <- as.data.frame(transactions_nrp)
colnames(nrp) <- "transactionValue"
nrp$transactionTypeCode <- "NRP"

nrn <- as.data.frame(transactions_nrn)
colnames(nrn) <- "transactionValue"
nrn$transactionTypeCode <- "NRN"

microdata <- rbind(microdata, rh)
microdata <- rbind(microdata, nrp)
microdata <- rbind(microdata, nrn)

microdata$yearCode <- base_year

## Save microdata as csv file
write.csv(
  microdata,
  paste0("data/temp/microdata_", base_year, ".csv")
)
