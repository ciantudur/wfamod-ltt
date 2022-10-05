# ---------------------------------------------------------------------------- #
# WFAMOD-LTT ----------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# 03.run_model.R

# DATE CREATED:  30 September 2022
# LAST MODIFIED: 05 October 2022
# AUTHOR: Cian Sion (post@ciantudur.com)


# DESCRIPTION:
# This script uprates the microdata using forecast parameters and calculates the
# tax due using the tax schedule provided.

# CAUTIONS:
# ! Remember to define base_year object before running script.
# ! Check that microdata for the specified base year exists in /data/temp.
# ! Make sure results_viewer.xlsx is not running before starting script.
# ! Remember to set working directory using setwd().
# ! Check csv separators in Section 03.2 if data doesn't load properly.



# 3.01 INITIALIZE SCRIPT -------------------------------------------------------

## Load required packages
packages_required <- c("stats", "dplyr", "tidyr", "openxlsx", "statswalesr")
for (pkg in packages_required) {
  require(pkg, character.only = TRUE)
}

## Set a base year (FYE) for the model (e.g. use 2022 for 2021-22 fiscal year)
base_year <- 2022

## Define custom function for calculating tax due
ltt_tax <- function(income, brackets, rates) {
  sum(diff(c(0, pmin(income, brackets))) * rates)
}

## Set seed to ensures randomly-generated values are reproducible
set.seed(500001)



# 3.02 LOAD MICRODATA & FORECAST PARAMETERS ------------------------------------

wra_data_cleaned <- read.csv("data/temp/wra_data_cleaned.csv")

microdata <- read.csv(paste0(
  "data/temp/microdata_", base_year, ".csv"
), sep = ",")
tax_schedule <- read.csv("data/input/tax_schedule.csv", sep = ",")
parameters <- read.csv("data/input/forecast_parameters.csv", sep = ",")
wra_wide_bins_re <- statswales_get_dataset("WRAx0003")
wra_wide_bins_nonres <- statswales_get_dataset("WRAX0004 ")


## Subset data frame to exclude empty rows
tax_schedule <- subset(tax_schedule, nchar(tax_schedule$fiscalYearEnding) == 4)



# 3.03 UPRATE MICRODATA USING GIVEN PARAMETERS ---------------------------------

years <- unique(tax_schedule$fiscalYearEnding)
years_to_forecast <- years[years >= base_year]
microdata_uprated <- NULL
microdata_temp <- microdata

## For each year to forecast, duplicate transactions in new data frame
for (i in seq_along(years_to_forecast)) {
  year <- years_to_forecast[i]
  df_name <- paste("microdata_", year, sep = "")
  microdata_temp$yearCode <- year

  if (year != base_year) {
    ## Uprate property values using forecast parameters
    microdata_temp$transactionValue[
      microdata_temp$transactionTypeCode %in% c("RM", "RH")
    ] <-
      microdata_temp$transactionValue[
        microdata_temp$transactionTypeCode %in% c("RM", "RH")
      ] * (1 + parameters$housePriceResidential[
        parameters$fiscalYearEnding == year
      ])
    microdata_temp$transactionValue[
      microdata_temp$transactionTypeCode %in% c("NRP", "NRN")
    ] <-
      microdata_temp$transactionValue[
        microdata_temp$transactionTypeCode %in% c("NRP", "NRN")
      ] * (1 + parameters$housePriceCommercial[
        parameters$fiscalYearEnding == year
      ])

    ## If residential transaction volume is growing, add more transactions
    if (parameters$transactionVolumeResidential[
      parameters$fiscalYearEnding == year
    ] > 0) {
      res_temp <- subset(microdata_temp, microdata_temp$transactionTypeCode
        %in% c("RM", "RH"))
      nrow_temp <- nrow(res_temp)
      add_row <- abs(nrow_temp * (parameters$transactionVolumeResidential[
        parameters$fiscalYearEnding == year
      ]))

      new_transactions <- res_temp %>% sample_n(add_row)
      random_values <- runif(add_row, min = 0, max = 1)
      new_property_values <- quantile(res_temp$transactionValue,
        probs = random_values
      )
      new_property_values <- as.vector(new_property_values)
      new_transactions$transactionValue <- new_property_values
      microdata_temp <- rbind(microdata_temp, new_transactions)

      ## If residential transaction volume is falling, delete transactions
    } else if (parameters$transactionVolumeResidential[
      parameters$fiscalYearEnding == year
    ] < 0) {
      res_temp <- subset(microdata_temp, microdata_temp$transactionTypeCode
        %in% c("RM", "RH"))
      nrow_temp <- nrow(res_temp)
      remove_row <- abs(nrow_temp * (parameters$transactionVolumeResidential[
        parameters$fiscalYearEnding == year
      ]))

      drop_transactions <- res_temp %>% sample_n(remove_row)
      microdata_temp <- microdata_temp[!microdata_temp$transactionValue
        %in% drop_transactions$transactionValue, ]
    } else {

    }

    ## If commercial transaction volume is growing, add more transactions
    if (parameters$transactionVolumeCommercial[
      parameters$fiscalYearEnding == year
    ] > 0) {
      non_res_temp <- subset(microdata_temp, microdata_temp$transactionTypeCode
        %in% c("NRP", "NRN"))
      nrow_temp <- nrow(non_res_temp)
      add_row <- abs(nrow_temp * (parameters$transactionVolumeCommercial[
        parameters$fiscalYearEnding == year
      ]))

      new_transactions <- non_res_temp %>% sample_n(add_row)
      random_values <- runif(add_row, min = 0, max = 1)
      new_property_values <- quantile(non_res_temp$transactionValue,
        probs = random_values
      )
      new_property_values <- as.vector(new_property_values)
      new_transactions$transactionValue <- new_property_values
      microdata_temp <- rbind(microdata_temp, new_transactions)

      ## If commercial transaction volume is falling, delete transactions
    } else if (parameters$transactionVolumeCommercial[
      parameters$fiscalYearEnding == year
    ] < 0) {
      non_res_temp <- subset(microdata_temp, microdata_temp$transactionTypeCode
        %in% c("NRP", "NRN"))
      nrow_temp <- nrow(non_res_temp)
      remove_row <- abs(nrow_temp * (parameters$transactionVolumeCommercial[
        parameters$fiscalYearEnding == year
      ]))

      drop_transactions <- non_res_temp %>% sample_n(remove_row)
      microdata_temp <- microdata_temp[!microdata_temp$transactionValue %in%
        drop_transactions$transactionValue, ]
    } else {

    }
  } else {

  }
  microdata_uprated <- rbind(microdata_uprated, assign(df_name, microdata_temp))
}

## Drop redundant variables
microdata_uprated <- subset(microdata_uprated, select = c(
  transactionValue,
  transactionTypeCode,
  yearCode
))



# 3.04 DEFINE TAX SCHEDULE -----------------------------------------------------

## Subset data frame to exclude empty rows
tax_schedule <- subset(tax_schedule, nchar(tax_schedule$fiscalYearEnding) == 4)

## Define transaction types
transaction_type <- c("RM", "RH", "NRP", "NRN")

## Define years with one weight (i.e. no in-year changes to the tax schedeule)
years_one_weight <- unique(tax_schedule$fiscalYearEnding[
  tax_schedule$weight == 1 &
    tax_schedule$fiscalYearEnding %in% years_to_forecast
])

## Define years with in-year tax changes
years_multi_weight <- years_to_forecast[!years_to_forecast %in%
  years_one_weight]


## Define brackets and tax schedule for years with one weight
for (i in seq_along(transaction_type)) {
  for (j in seq_along(years_one_weight)) {
    name_bracket <- paste("brackets_", transaction_type[i],
      years_one_weight[j],
      sep = ""
    )
    assign(name_bracket, as.vector(tax_schedule$taxBracketHigh[
      tax_schedule$transactionTypeCode == transaction_type[i] &
        tax_schedule$fiscalYearEnding == years_one_weight[j]
    ]))
    name_schedule <- paste("schedule_", transaction_type[i],
      years_one_weight[j],
      sep = ""
    )
    assign(name_schedule, as.vector(tax_schedule$taxRate[
      tax_schedule$transactionTypeCode == transaction_type[i] &
        tax_schedule$fiscalYearEnding == years_one_weight[j]
    ]))
  }
}

## Define brackets and tax schedule for years with multiple weights
for (i in seq_along(transaction_type)) {
  for (j in seq_along(years_multi_weight)) {
    schedule_id <- unique(tax_schedule$inYearCount[
      tax_schedule$transactionTypeCode == transaction_type[i] &
        tax_schedule$fiscalYearEnding == years_multi_weight[j]
    ])
    for (k in seq_along(schedule_id)) {
      name_bracket <- paste("brackets_", transaction_type[i],
        years_multi_weight[j],
        schedule_id[k],
        sep = ""
      )
      assign(name_bracket, as.vector(tax_schedule$taxBracketHigh[
        tax_schedule$transactionTypeCode == transaction_type[i] &
          tax_schedule$fiscalYearEnding == years_multi_weight[j] &
          tax_schedule$inYearCount == schedule_id[k]
      ]))
      name_schedule <- paste("schedule_", transaction_type[i],
        years_multi_weight[j],
        schedule_id[k],
        sep = ""
      )
      assign(name_schedule, as.vector(tax_schedule$taxRate[
        tax_schedule$transactionTypeCode == transaction_type[i] &
          tax_schedule$fiscalYearEnding == years_multi_weight[j] &
          tax_schedule$inYearCount == schedule_id[k]
      ]))
    }
  }
}



# 3.05 CALCULATE TAX DUE (NO IN-YEAR TAX CHANGES) ------------------------------

## Generate empty data frames for loop
microdata_taxdue <- data.frame(NULL)
microdata_temp <- data.frame(NULL)
tax_due <- data.frame(NULL)

## For each year (1 weight) and transaction type, calculate tax due
for (i in seq_along(years_one_weight)) {
  for (j in seq_along(transaction_type)) {
    year <- years_one_weight[i]
    transaction <- transaction_type[j]

    message(paste(
      "Calculating tax due for",
      transaction, "transactions", "FYE", year, "..."
    ))

    ## Subset transactions by type and year
    microdata_temp <- subset(
      microdata_uprated,
      microdata_uprated$transactionTypeCode == as.character(transaction) &
        microdata_uprated$yearCode == as.numeric(year)
    )
    brackets <- get(paste0("brackets_", transaction, year))
    schedule <- get(paste0("schedule_", transaction, year))

    ## For each transaction in subset, calculate tax due
    for (k in microdata_temp$transactionValue) {
      temp_tax <- ltt_tax(
        k,
        brackets,
        schedule
      )
      tax_due <- rbind(tax_due, temp_tax)
    }

    ## Append tax due to microdata
    microdata_temp$taxDue <- tax_due[, 1]
    tax_due <- NULL
    microdata_taxdue <- rbind(microdata_taxdue, microdata_temp)

    message(paste("  --> Complete"))
  }
}



# 3.06 CALCULATE TAX DUE (IN-YEAR TAX CHANGES) ---------------------------------

## Generate empty data frames for loop
microdata_taxdue2 <- data.frame(NULL)
microdata_temp2 <- data.frame(NULL)
microdata_temp3 <- data.frame(NULL)
tax_due2 <- data.frame(NULL)

## If there are years with in-year tax changes, calculate tax due here
if (length(years_multi_weight) != 0) {
  for (i in seq_along(years_multi_weight)) {
    year <- years_multi_weight[i]
    in_year_count <- unique(tax_schedule$inYearCount[
      tax_schedule$fiscalYearEnding == year
    ])
    weights <- tax_schedule$weight[
      tax_schedule$fiscalYearEnding == year
    ]

    microdata_temp2 <- subset(
      microdata_uprated,
      microdata_uprated$yearCode == year
    )

    microdata_temp2$taxScheduleID <- 0
    message(paste("Calculating tax due for FYE", year, "..."))
    j <- 1

    ## Allocate transactions to different tax schedules based on weights
    while (j < length((inYearCount))) {
      microdata_temp2$taxScheduleID[microdata_temp2$taxScheduleID != 0] <-
        microdata_temp2$taxScheduleID[microdata_temp2$taxScheduleID != 0] + 1
      microdata_temp2$taxScheduleID[
        microdata_temp2$taxScheduleID == 0
      ] <- rbinom(length(microdata_temp2$taxScheduleID[
        microdata_temp2$taxScheduleID == 0
      ]), size = 1, prob = weights[j])
      j <- j + 1
    }
    microdata_temp2$taxScheduleID[microdata_temp2$taxScheduleID == 0] <-
      max(in_year_count)

    ## Calculate tax due for each transaction type and tax schedule
    for (j in seq_along(transaction_type)) {
      transaction <- transaction_type[j]
      for (k in seq_along(in_year_count)) {
        tax_id <- in_year_count[k]

        ## Subset transactions by type and year
        microdata_temp3 <- subset(
          microdata_temp2,
          microdata_temp2$transactionTypeCode == as.character(transaction) &
            microdata_temp2$taxScheduleID == tax_id
        )

        # Get tax brackets and schedules
        brackets <- get(paste0("brackets_", transaction, year, tax_id))
        schedule <- get(paste0("schedule_", transaction, year, tax_id))

        ## For each transaction in subset, calculate tax due
        for (l in microdata_temp3$transactionValue) {
          temp_tax <- ltt_tax(
            l,
            brackets,
            schedule
          )
          tax_due2 <- rbind(tax_due2, temp_tax)
        }

        ## Append tax due to microdata
        microdata_temp3$taxDue <- tax_due2[, 1]
        tax_due2 <- NULL
        microdata_taxdue2 <- rbind(microdata_taxdue2, microdata_temp3)
      }
    }
    message(paste("  --> Complete"))
  }
} else {

}

## Drop redundant variable
microdata_taxdue2$taxScheduleID <- NULL

## Merge results from 3.05 and 03.06
if (nrow(microdata_taxdue2) > 0) {
  microdata_taxdue <- rbind(microdata_taxdue, microdata_taxdue2)
}

## Save microdata (before reliefs) as csv file
write.csv(microdata_taxdue, paste0(
  "output/microdata_output_gross_reliefs_",
  base_year, ".csv"
))



# 3.07 APPLY RELIEFS -----------------------------------------------------------

## Get data on residential tax due by wide value bands (StatsWales)
wra_wide_bins_re <- wra_wide_bins_re[
  wra_wide_bins_re$Period_Code == paste0(
    substr(base_year, 1, 2),
    as.numeric(substr(base_year, 3, 4)) - 1,
    substr(base_year, 3, 4)
  ),
]
wra_wide_bins_re <- wra_wide_bins_re[
  order(wra_wide_bins_re$Transactionvalue_SortOrder),
]

## Calculate difference between tax due and modelled amount for RM transactions
low_bin_re <- c(0, 180000, 250000, 400000, 750000, 1500000)
high_bin_re <- c(180000, 250000, 400000, 750000, 1500000, Inf)
transaction_value_code <- c(101, 102, 103, 104, 105, 106)

sum_temp <- NULL
sum <- NULL
for (j in seq_along(low_bin_re)) {
  low_bin <- low_bin_re[j]
  high_bin <- high_bin_re[j]
  sum_temp <- sum(microdata_taxdue$taxDue[
    microdata_taxdue$yearCode == base_year &
      microdata_taxdue$transactionTypeCode == "RM" &
      microdata_taxdue$transactionValue > low_bin &
      microdata_taxdue$transactionValue <= high_bin
  ])
  sum <- append(sum, sum_temp)
}

re_count_temp <- NULL
re_count <- NULL
for (j in seq_along(low_bin_re)) {
  low_bin <- low_bin_re[j]
  high_bin <- high_bin_re[j]
  re_count_temp <- length(microdata_taxdue$taxDue[
    microdata_taxdue$yearCode == base_year &
      microdata_taxdue$transactionTypeCode == "RM" &
      microdata_taxdue$transactionValue > low_bin &
      microdata_taxdue$transactionValue <= high_bin
  ])
  re_count <- append(re_count, re_count_temp)
}

tax_transaction_model <- sum / re_count

tax_transaction_outturn_d <- wra_wide_bins_re$Data[
  wra_wide_bins_re$Measure_Code %in% c("D") &
    wra_wide_bins_re$Transactiontype_Code == "RS" &
    wra_wide_bins_re$Transactionvalue_SortOrder != 100
]
tax_transaction_outturn_c <- wra_wide_bins_re$Data[
  wra_wide_bins_re$Measure_Code %in% c("C") &
    wra_wide_bins_re$Transactiontype_Code == "RS" &
    wra_wide_bins_re$Transactionvalue_SortOrder != 100
]
tax_transaction_outturn <- tax_transaction_outturn_d / tax_transaction_outturn_c

diff <- tax_transaction_model / (tax_transaction_outturn * 1000000)
diff[is.na(diff)] <- 1

for (j in seq_along(low_bin_re)) {
  microdata_taxdue$taxDue[microdata_taxdue$transactionTypeCode == "RM" &
    microdata_taxdue$transactionValue > low_bin_re[j] &
    microdata_taxdue$transactionValue <= high_bin_re[j]] <-
    microdata_taxdue$taxDue[microdata_taxdue$transactionTypeCode == "RM" &
      microdata_taxdue$transactionValue > low_bin_re[j] &
      microdata_taxdue$transactionValue <= high_bin_re[j]] / diff[j]
}

## Calculate difference between tax due and modelled amount for RH transactions
wra_wide_bins_re <- wra_wide_bins_re[
  order(wra_wide_bins_re$Transactionvalue_SortOrder),
]

sum_temp <- NULL
sum <- NULL
for (j in seq_along(low_bin_re)) {
  low_bin <- low_bin_re[j]
  high_bin <- high_bin_re[j]
  sum_temp <- sum(microdata_taxdue$taxDue[
    microdata_taxdue$yearCode == base_year &
      microdata_taxdue$transactionTypeCode == "RH" &
      microdata_taxdue$transactionValue > low_bin &
      microdata_taxdue$transactionValue <= high_bin
  ])
  sum <- append(sum, sum_temp)
}

re_count_temp <- NULL
re_count <- NULL
for (j in seq_along(low_bin_re)) {
  low_bin <- low_bin_re[j]
  high_bin <- high_bin_re[j]
  re_count_temp <- length(microdata_taxdue$taxDue[
    microdata_taxdue$yearCode == base_year &
      microdata_taxdue$transactionTypeCode == "RH" &
      microdata_taxdue$transactionValue > low_bin &
      microdata_taxdue$transactionValue <= high_bin
  ])
  re_count <- append(re_count, re_count_temp)
}

tax_transaction_model <- sum / re_count

tax_transaction_outturn_d <- wra_wide_bins_re$Data[
  wra_wide_bins_re$Measure_Code %in% c("D") &
    wra_wide_bins_re$Transactiontype_Code == "RH" &
    wra_wide_bins_re$Transactionvalue_SortOrder != 100
]
tax_transaction_outturn_c <- wra_wide_bins_re$Data[
  wra_wide_bins_re$Measure_Code %in% c("C") &
    wra_wide_bins_re$Transactiontype_Code == "RH" &
    wra_wide_bins_re$Transactionvalue_SortOrder != 100
]
tax_transaction_outturn <- tax_transaction_outturn_d / tax_transaction_outturn_c

diff <- tax_transaction_model / (tax_transaction_outturn * 1000000)
diff[is.na(diff)] <- 1

for (j in seq_along(low_bin_re)) {
  microdata_taxdue$taxDue[
    microdata_taxdue$transactionTypeCode == "RH" &
      microdata_taxdue$transactionValue > low_bin_re[j] &
      microdata_taxdue$transactionValue <= high_bin_re[j]
  ] <-
    microdata_taxdue$taxDue[
      microdata_taxdue$transactionTypeCode == "RH" &
        microdata_taxdue$transactionValue > low_bin_re[j] &
        microdata_taxdue$transactionValue <= high_bin_re[j]
    ] / diff[j]
}

## Get data on non-residential tax due by wide value bands (StatsWales)
wra_wide_bins_nonres <- wra_wide_bins_nonres[
  wra_wide_bins_nonres$Period_Code == paste0(
    substr(base_year, 1, 2),
    as.numeric(substr(base_year, 3, 4)) - 1,
    substr(base_year, 3, 4)
  ),
]
wra_wide_bins_nonres <- wra_wide_bins_nonres[
  order(wra_wide_bins_nonres$Transactionvalue_SortOrder),
]

## Calculate difference between tax due and modelled amount for NRP transactions
low_bin_re <- c(0, 150000, 225000, 250000, 1000000, 2000000)
high_bin_re <- c(150000, 225000, 250000, 1000000, 2000000, Inf)
transaction_value_code <- c(201, 202, 203, 204, 205, 206)

sum_temp <- NULL
sum <- NULL
for (j in seq_along(low_bin_re)) {
  low_bin <- low_bin_re[j]
  high_bin <- high_bin_re[j]
  sum_temp <- sum(microdata_taxdue$taxDue[
    microdata_taxdue$yearCode == base_year &
      microdata_taxdue$transactionTypeCode == "NRP" &
      microdata_taxdue$transactionValue > low_bin &
      microdata_taxdue$transactionValue <= high_bin
  ])
  sum <- append(sum, sum_temp)
}

re_count_temp <- NULL
re_count <- NULL
for (j in seq_along(low_bin_re)) {
  low_bin <- low_bin_re[j]
  high_bin <- high_bin_re[j]
  re_count_temp <- length(microdata_taxdue$taxDue[
    microdata_taxdue$yearCode == base_year &
      microdata_taxdue$transactionTypeCode == "NRP" &
      microdata_taxdue$transactionValue > low_bin &
      microdata_taxdue$transactionValue <= high_bin
  ])
  re_count <- append(re_count, re_count_temp)
}

tax_transaction_model <- sum / re_count

tax_transaction_outturn_d <- wra_wide_bins_nonres$Data[
  wra_wide_bins_nonres$Measure_Code %in% c("D") &
    wra_wide_bins_nonres$Transactionvalue_SortOrder %in% transaction_value_code
]
tax_transaction_outturn_c <- wra_wide_bins_nonres$Data[
  wra_wide_bins_nonres$Measure_Code %in% c("C") &
    wra_wide_bins_nonres$Transactionvalue_SortOrder %in% transaction_value_code
]
tax_transaction_outturn <- tax_transaction_outturn_d / tax_transaction_outturn_c

diff <- tax_transaction_model / (tax_transaction_outturn * 1000000)
diff[is.na(diff)] <- 1

for (j in seq_along(low_bin_re)) {
  microdata_taxdue$taxDue[
    microdata_taxdue$transactionTypeCode == "NRP" &
      microdata_taxdue$transactionValue > low_bin_re[j] &
      microdata_taxdue$transactionValue <= high_bin_re[j]
  ] <-
    microdata_taxdue$taxDue[
      microdata_taxdue$transactionTypeCode == "NRP" &
        microdata_taxdue$transactionValue > low_bin_re[j] &
        microdata_taxdue$transactionValue <= high_bin_re[j]
    ] / diff[j]
}

## Calculate difference between tax due and modelled amount for NRN transactions
wra_wide_bins_nonres <- wra_wide_bins_nonres[
  order(wra_wide_bins_nonres$Transactionvalue_SortOrder),
]

sum <- NULL
sum <- sum(microdata_taxdue$taxDue[
  microdata_taxdue$yearCode == base_year &
    microdata_taxdue$transactionTypeCode == "NRN"
])

re_count <- NULL
re_count <- length(microdata_taxdue$taxDue[
  microdata_taxdue$yearCode == base_year &
    microdata_taxdue$transactionTypeCode == "NRN"
])

tax_transaction_model <- sum / re_count

tax_transaction_outturn_d <- wra_wide_bins_nonres$Data[
  wra_wide_bins_nonres$Measure_Code %in% c("D") &
    wra_wide_bins_nonres$Transactionvalue_SortOrder == 300
]
tax_transaction_outturn_c <- wra_wide_bins_nonres$Data[
  wra_wide_bins_nonres$Measure_Code %in% c("C") &
    wra_wide_bins_nonres$Transactionvalue_SortOrder == 300
]
tax_transaction_outturn <- tax_transaction_outturn_d / tax_transaction_outturn_c

diff <- tax_transaction_model / (tax_transaction_outturn * 1000000)
diff[is.na(diff)] <- 1

microdata_taxdue$taxDue[
  microdata_taxdue$transactionTypeCode == "NRN"
] <-
  microdata_taxdue$taxDue[
    microdata_taxdue$transactionTypeCode == "NRN"
  ] / diff[1]
microdata_taxdue$taxDue[is.na(microdata_taxdue$taxDue)] <- 0



# 3.08 CLEAN & WRITE OUTPUT TO XLSX FILE ---------------------------------------
microdata_final <- microdata_taxdue

fye <- seq(from = 2019, to = 2027, by = 1)

## Get data for Tax Summary sheet
l16a <- NULL
for (i in fye) {
  value <- sum(microdata_final$taxDue[microdata_final$yearCode == i]) / 1000000
  l16a <- append(l16a, value)
}
l17a <- NULL
for (i in fye) {
  value <- sum(microdata_final$taxDue[microdata_final$yearCode == i &
    microdata_final$transactionTypeCode %in% c("RM", "RH")]) / 1000000
  l17a <- append(l17a, value)
}
l18a <- NULL
for (i in fye) {
  value <- sum(microdata_final$taxDue[microdata_final$yearCode == i &
    microdata_final$transactionTypeCode %in% c("RM")]) / 1000000
  l18a <- append(l18a, value)
}
l19a <- NULL
for (i in fye) {
  value <- sum(microdata_final$taxDue[microdata_final$yearCode == i &
    microdata_final$transactionTypeCode %in% c("RH")]) / 1000000
  l19a <- append(l19a, value)
}
l20a <- NULL
for (i in fye) {
  value <- sum(microdata_final$taxDue[microdata_final$yearCode == i &
    microdata_final$transactionTypeCode %in% c("NRP", "NRN")]) / 1000000
  l20a <- append(l20a, value)
}
l21a <- NULL
for (i in fye) {
  value <- sum(microdata_final$taxDue[microdata_final$yearCode == i &
    microdata_final$transactionTypeCode %in% c("NRP")]) / 1000000
  l21a <- append(l21a, value)
}
l22a <- NULL
for (i in fye) {
  value <- sum(microdata_final$taxDue[microdata_final$yearCode == i &
    microdata_final$transactionTypeCode %in% c("NRN")]) / 1000000
  l22a <- append(l22a, value)
}

## Get data for Detailed Results sheet
l16b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM", "RH", "NRP")
  ])
  l16b <- append(l16b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c()
  ]) / 1000000
  l16b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM", "RH", "NRP", "NRN")
  ]) / 1000000
  l16b[n] <- value
  n <- n + 3
}

l17b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM", "RH")
  ])
  l17b <- append(l17b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM", "RH")
  ]) / 1000000
  l17b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM", "RH")
  ]) / 1000000
  l17b[n] <- value
  n <- n + 3
}

l18b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM")
  ])
  l18b <- append(l18b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM")
  ]) / 1000000
  l18b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM")
  ]) / 1000000
  l18b[n] <- value
  n <- n + 3
}

l19b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM") &
      microdata_final$transactionValue >= 0 &
      microdata_final$transactionValue <= 180000
  ])
  l19b <- append(l19b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM") &
      microdata_final$transactionValue >= 0 &
      microdata_final$transactionValue <= 180000
  ]) / 1000000
  l19b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM") &
      microdata_final$transactionValue >= 0 &
      microdata_final$transactionValue <= 180000
  ]) / 1000000
  l19b[n] <- value
  n <- n + 3
}

l20b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM") &
      microdata_final$transactionValue > 180000 &
      microdata_final$transactionValue <= 225000
  ])
  l20b <- append(l20b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM") &
      microdata_final$transactionValue > 180000 &
      microdata_final$transactionValue <= 225000
  ]) / 1000000
  l20b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM") &
      microdata_final$transactionValue > 180000 &
      microdata_final$transactionValue <= 225000
  ]) / 1000000
  l20b[n] <- value
  n <- n + 3
}

l21b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM") &
      microdata_final$transactionValue > 225000 &
      microdata_final$transactionValue <= 250000
  ])
  l21b <- append(l21b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM") &
      microdata_final$transactionValue > 225000 &
      microdata_final$transactionValue <= 250000
  ]) / 1000000
  l21b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM") &
      microdata_final$transactionValue > 225000 &
      microdata_final$transactionValue <= 250000
  ]) / 1000000
  l21b[n] <- value
  n <- n + 3
}

l22b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM") &
      microdata_final$transactionValue > 250000 &
      microdata_final$transactionValue <= 400000
  ])
  l22b <- append(l22b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM") &
      microdata_final$transactionValue > 250000 &
      microdata_final$transactionValue <= 400000
  ]) / 1000000
  l22b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM") &
      microdata_final$transactionValue > 250000 &
      microdata_final$transactionValue <= 400000
  ]) / 1000000
  l22b[n] <- value
  n <- n + 3
}

l23b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM") &
      microdata_final$transactionValue > 400000 &
      microdata_final$transactionValue <= 750000
  ])
  l23b <- append(l23b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM") &
      microdata_final$transactionValue > 400000 &
      microdata_final$transactionValue <= 750000
  ]) / 1000000
  l23b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM") &
      microdata_final$transactionValue > 400000 &
      microdata_final$transactionValue <= 750000
  ]) / 1000000
  l23b[n] <- value
  n <- n + 3
}

l24b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM") &
      microdata_final$transactionValue > 750000 &
      microdata_final$transactionValue <= 1500000
  ])
  l24b <- append(l24b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM") &
      microdata_final$transactionValue > 750000 &
      microdata_final$transactionValue <= 1500000
  ]) / 1000000
  l24b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM") &
      microdata_final$transactionValue > 750000 &
      microdata_final$transactionValue <= 1500000
  ]) / 1000000
  l24b[n] <- value
  n <- n + 3
}

l25b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM") &
      microdata_final$transactionValue > 1500000
  ])
  l25b <- append(l25b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM") &
      microdata_final$transactionValue > 1500000
  ]) / 1000000
  l25b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RM") &
      microdata_final$transactionValue > 1500000
  ]) / 1000000
  l25b[n] <- value
  n <- n + 3
}



l26b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RH")
  ])
  l26b <- append(l26b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RH")
  ]) / 1000000
  l26b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RH")
  ]) / 1000000
  l26b[n] <- value
  n <- n + 3
}

l27b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RH") &
      microdata_final$transactionValue >= 0 &
      microdata_final$transactionValue <= 180000
  ])
  l27b <- append(l27b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RH") &
      microdata_final$transactionValue >= 0 &
      microdata_final$transactionValue <= 180000
  ]) / 1000000
  l27b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RH") &
      microdata_final$transactionValue >= 0 &
      microdata_final$transactionValue <= 180000
  ]) / 1000000
  l27b[n] <- value
  n <- n + 3
}

l28b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RH") &
      microdata_final$transactionValue > 180000 &
      microdata_final$transactionValue <= 225000
  ])
  l28b <- append(l28b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RH") &
      microdata_final$transactionValue > 180000 &
      microdata_final$transactionValue <= 225000
  ]) / 1000000
  l28b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RH") &
      microdata_final$transactionValue > 180000 &
      microdata_final$transactionValue <= 225000
  ]) / 1000000
  l28b[n] <- value
  n <- n + 3
}

l29b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RH") &
      microdata_final$transactionValue > 225000 &
      microdata_final$transactionValue <= 250000
  ])
  l29b <- append(l29b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RH") &
      microdata_final$transactionValue > 225000 &
      microdata_final$transactionValue <= 250000
  ]) / 1000000
  l29b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RH") &
      microdata_final$transactionValue > 225000 &
      microdata_final$transactionValue <= 250000
  ]) / 1000000
  l29b[n] <- value
  n <- n + 3
}

l30b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RH") &
      microdata_final$transactionValue > 250000 &
      microdata_final$transactionValue <= 400000
  ])
  l30b <- append(l30b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RH") &
      microdata_final$transactionValue > 250000 &
      microdata_final$transactionValue <= 400000
  ]) / 1000000
  l30b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RH") &
      microdata_final$transactionValue > 250000 &
      microdata_final$transactionValue <= 400000
  ]) / 1000000
  l30b[n] <- value
  n <- n + 3
}

l31b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RH") &
      microdata_final$transactionValue > 400000 &
      microdata_final$transactionValue <= 750000
  ])
  l31b <- append(l31b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RH") &
      microdata_final$transactionValue > 400000 &
      microdata_final$transactionValue <= 750000
  ]) / 1000000
  l31b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RH") &
      microdata_final$transactionValue > 400000 &
      microdata_final$transactionValue <= 750000
  ]) / 1000000
  l31b[n] <- value
  n <- n + 3
}

l32b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RH") &
      microdata_final$transactionValue > 750000 &
      microdata_final$transactionValue <= 1500000
  ])
  l32b <- append(l32b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RH") &
      microdata_final$transactionValue > 750000 &
      microdata_final$transactionValue <= 1500000
  ]) / 1000000
  l32b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RH") &
      microdata_final$transactionValue > 750000 &
      microdata_final$transactionValue <= 1500000
  ]) / 1000000
  l32b[n] <- value
  n <- n + 3
}

l33b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RH") &
      microdata_final$transactionValue > 1500000
  ])
  l33b <- append(l33b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RH") &
      microdata_final$transactionValue > 1500000
  ]) / 1000000
  l33b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("RH") &
      microdata_final$transactionValue > 1500000
  ]) / 1000000
  l33b[n] <- value
  n <- n + 3
}


l34b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRP")
  ])
  l34b <- append(l34b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c()
  ]) / 1000000
  l34b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRP", "NRN")
  ]) / 1000000
  l34b[n] <- value
  n <- n + 3
}

l35b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRP")
  ])
  l35b <- append(l35b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRP")
  ]) / 1000000
  l35b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRP")
  ]) / 1000000
  l35b[n] <- value
  n <- n + 3
}

l36b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRP") &
      microdata_final$transactionValue >= 0 &
      microdata_final$transactionValue <= 150000
  ])
  l36b <- append(l36b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRP") &
      microdata_final$transactionValue >= 0 &
      microdata_final$transactionValue <= 150000
  ]) / 1000000
  l36b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRP") &
      microdata_final$transactionValue >= 0 &
      microdata_final$transactionValue <= 150000
  ]) / 1000000
  l36b[n] <- value
  n <- n + 3
}

l37b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRP") &
      microdata_final$transactionValue > 150000 &
      microdata_final$transactionValue <= 225000
  ])
  l37b <- append(l37b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRP") &
      microdata_final$transactionValue > 150000 &
      microdata_final$transactionValue <= 225000
  ]) / 1000000
  l37b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRP") &
      microdata_final$transactionValue > 150000 &
      microdata_final$transactionValue <= 225000
  ]) / 1000000
  l37b[n] <- value
  n <- n + 3
}

l38b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRP") &
      microdata_final$transactionValue > 225000 &
      microdata_final$transactionValue <= 250000
  ])
  l38b <- append(l38b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRP") &
      microdata_final$transactionValue > 225000 &
      microdata_final$transactionValue <= 250000
  ]) / 1000000
  l38b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRP") &
      microdata_final$transactionValue > 225000 &
      microdata_final$transactionValue <= 250000
  ]) / 1000000
  l38b[n] <- value
  n <- n + 3
}

l39b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRP") &
      microdata_final$transactionValue > 250000 &
      microdata_final$transactionValue <= 1000000
  ])
  l39b <- append(l39b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRP") &
      microdata_final$transactionValue > 250000 &
      microdata_final$transactionValue <= 1000000
  ]) / 1000000
  l39b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRP") &
      microdata_final$transactionValue > 250000 &
      microdata_final$transactionValue <= 1000000
  ]) / 1000000
  l39b[n] <- value
  n <- n + 3
}

l40b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRP") &
      microdata_final$transactionValue > 1000000 &
      microdata_final$transactionValue <= 2000000
  ])
  l40b <- append(l40b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRP") &
      microdata_final$transactionValue > 1000000 &
      microdata_final$transactionValue <= 2000000
  ]) / 1000000
  l40b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRP") &
      microdata_final$transactionValue > 1000000 &
      microdata_final$transactionValue <= 2000000
  ]) / 1000000
  l40b[n] <- value
  n <- n + 3
}

l41b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRP") &
      microdata_final$transactionValue > 2000000
  ])
  l41b <- append(l41b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRP") &
      microdata_final$transactionValue > 2000000
  ]) / 1000000
  l41b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRP") &
      microdata_final$transactionValue > 2000000
  ]) / 1000000
  l41b[n] <- value
  n <- n + 3
}


l42b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRN")
  ])
  l42b <- append(l42b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRN")
  ]) / 1000000
  l42b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRN")
  ]) / 1000000
  l42b[n] <- value
  n <- n + 3
}

l43b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRN") &
      microdata_final$transactionValue >= 0 &
      microdata_final$transactionValue <= 150000
  ])
  l43b <- append(l43b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRN") &
      microdata_final$transactionValue >= 0 &
      microdata_final$transactionValue <= 150000
  ]) / 1000000
  l43b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRN") &
      microdata_final$transactionValue >= 0 &
      microdata_final$transactionValue <= 150000
  ]) / 1000000
  l43b[n] <- value
  n <- n + 3
}

l44b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRN") &
      microdata_final$transactionValue > 150000 &
      microdata_final$transactionValue <= 2000000
  ])
  l44b <- append(l44b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRN") &
      microdata_final$transactionValue > 150000 &
      microdata_final$transactionValue <= 2000000
  ]) / 1000000
  l44b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRN") &
      microdata_final$transactionValue > 150000 &
      microdata_final$transactionValue <= 2000000
  ]) / 1000000
  l44b[n] <- value
  n <- n + 3
}

l45b <- NULL
for (i in seq_along(fye)) {
  value <- length(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRN") &
      microdata_final$transactionValue > 2000000
  ])
  l45b <- append(l45b, c(value, 0, 0))
}
n <- 2
for (i in seq_along(fye)) {
  value <- sum(microdata_final$transactionValue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRN") &
      microdata_final$transactionValue > 2000000
  ]) / 1000000
  l45b[n] <- value
  n <- n + 3
}
n <- 3
for (i in seq_along(fye)) {
  value <- sum(microdata_final$taxDue[
    microdata_final$yearCode == fye[i] &
      microdata_final$transactionTypeCode %in% c("NRN") &
      microdata_final$transactionValue > 2000000
  ]) / 1000000
  l45b[n] <- value
  n <- n + 3
}

## Write output to Results Viewer
tax_summary <- rbind(l16a, l17a, l18a, l19a, l20a, l21a, l22a)
tax_details <- rbind(
  l16b, l17b, l18b, l19b, l20b, l21b, l22b, l23b, l24b,
  l25b, l26b, l27b, l28b, l29b, l30b, l31b, l32b, l33b,
  l34b, l35b, l36b, l37b, l38b, l39b, l40b, l41b, l42b,
  l43b, l44b, l45b
)
names(tax_summary) <- NULL
wb <- loadWorkbook("output/results_viewer.xlsx")
writeData(wb, sheet = "1.TaxSummary", x = paste0(Sys.time()), xy = c(3, 5))
writeData(wb, sheet = "1.TaxSummary", x = paste0(base_year), xy = c(3, 6))
writeData(wb,
  sheet = "1.TaxSummary", x = tax_summary, xy = c(5, 16),
  colNames = FALSE, rowNames = FALSE
)
writeData(wb, sheet = "2.DetailedResults", x = paste0(Sys.time()), xy = c(3, 5))
writeData(wb, sheet = "2.DetailedResults", x = paste0(base_year), xy = c(3, 6))
writeData(wb,
  sheet = "2.DetailedResults", x = tax_details, xy = c(6, 16),
  colNames = FALSE, rowNames = FALSE
)

writeData(wb,
  sheet = "3.TaxSchedule", x = tax_schedule, xy = c(2, 7),
  colNames = TRUE, rowNames = FALSE
)
writeData(wb,
  sheet = "4.ForecastParameters", x = parameters, xy = c(2, 7),
  colNames = TRUE, rowNames = FALSE
)
saveWorkbook(
  wb,
  "output/results_viewer.xlsx",
  overwrite = TRUE
)