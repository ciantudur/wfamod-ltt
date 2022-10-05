# ---------------------------------------------------------------------------- #
# WFAMOD-LTT ----------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# 01.prepare_input_data.R

# DATE CREATED:  21 September 2022
# LAST MODIFIED: 04 October 2022
# AUTHOR: Cian Sion (post@ciantudur.com)


# DESCRIPTION:
# This script fetches and transforms the input data required to generate the
# model microdata.

# CAUTIONS:
# ! Remember to set working directory using setwd().
# ! Always manually check wra_data_cleaned after running the script. Changes to
#   formatting / how value bins are recorded in the input data may invalidate
#   the output.



# 1.01 INITIALIZE SCRIPT -------------------------------------------------------

## Load required packages
packages_required <- c("utils", "readODS", "dplyr", "stringr")
for (pkg in packages_required) {
  require(pkg, character.only = TRUE)
}



# 1.02 FETCH INPUT DATA --------------------------------------------------------

## Download data from StatsWales
utils::download.file(
  paste0("https://gov.wales/sites/default/files/statistics-and-research/2022-",
         "08/land-transaction-tax-statistics-detailed-analysis-of-transactions-",
         "by-transaction-value.ods"),
  destfile = "data/input/wra_data_raw.ods",
  method = "curl"
)

## Read StatsWales .ods file and convert to dataframe
message("Parsing data...")
wra_data_raw <- readODS::read_ods("data/input/wra.ods",
  sheet = "Table_1",
  skip = 3
)



# 1.03 CLEAN DATA --------------------------------------------------------------

## Rename variables using camelCase and suppress output
invisible(
  wra_data <- wra_data_raw %>% dplyr::rename(
    periodCode = PeriodCode,
    periodDescription = PeriodDescription,
    transactionTypeCode = TransactionTypeCode,
    transactionTypeDescription = TransactionTypeDescription,
    valueBinCode = ValueBinCode,
    valueBinDescription = ValueBinDescription,
    measureCode = MeasureCode,
    measureCodeDescription = MeasureCodeDescription,
    dataValue = DataValue
  )
)

## Convert data character string to numeric & replace suppressed sums
wra_data$dataValue[wra_data$dataValue == "[c]"] <- NA # Value suppressed
wra_data$dataValue[wra_data$dataValue == "[low]" &
  wra_data$measureCode == "D"] <- 0.025 # Rounds to 0
wra_data$dataValue[wra_data$dataValue == "[low]" &
  wra_data$measureCode %in% c("N", "V")] <- 0.25
wra_data$dataValue[wra_data$dataValue == "[low]" &
  wra_data$measureCode == "C"] <- 3
wra_data$dataValue <- as.numeric(wra_data$dataValue)

## Only keep years ending 31 March
wra_data <- wra_data[stringr::str_ends(wra_data$periodCode, "0331"), ]

## Remove Higher rates before refunds rows
wra_data <- wra_data[wra_data$transactionTypeCode != "RHG", ]



# 1.04 MAP PRICE BINS AND YEAR CODES TO NUMERIC VALUES -------------------------

## Generate lowBin numeric variable from valueBinDescription character string
low_bin <- stringr::word(wra_data$valueBinDescription, 1, sep = " to")
low_bin <- gsub("£", "", low_bin)
low_bin <- gsub(" and over", "", low_bin)
low_bin[low_bin == "0"] <- 0
low_bin <- gsub(",", "", low_bin)
low_bin <- as.numeric(low_bin)
wra_data$lowBin <- low_bin

## Generate highBin numeric variable from valueBinDescription character string
high_bin <- stringr::word(wra_data$valueBinDescription, -1, sep = "to ")
high_bin <- gsub("£", "", high_bin)
high_bin[stringr::str_ends(high_bin, "and over")] <- NA
high_bin[high_bin == "0"] <- 0
high_bin <- gsub(",", "", high_bin)
high_bin <- as.numeric(high_bin)
wra_data$highBin <- high_bin

## Generate baseYear numeric variable from periodCode character string
base_year <- gsub("YE", "", wra_data$periodCode)
base_year <- gsub("0331", "", base_year)
base_year <- as.numeric(base_year)
wra_data$baseYear <- base_year



# 1.05 COMBINE RESIDENTIAL PRICE BINS TO MATCH HIGHER BINS ---------------------

## Combine residential price bins to match Higher Residential price bins
missing_pricebins <- c(71, 73, 75, 77, 79, 87)
new_bin_description <- c(
  "£350,001 to Â£360,000",
  "Â£360,001 to Â£370,000",
  "Â£370,001 to Â£380,000",
  "Â£380,001 to Â£390,000",
  "Â£390,001 to Â£400,000",
  "Â£650,001 to Â£750,000"
)
new_low_bin <- c(350001, 360001, 370001, 380001, 390001, 650001)
res_temp <- NULL

for (i in 1:length(missing_pricebins)) {
  res_temp <- wra_data[wra_data$transactionTypeCode == "RE" &
    wra_data$valueBinCode %in% c(
      missing_pricebins[i],
      missing_pricebins[i] + 1
    ), ]
  res_temp <- as.data.frame(res_temp %>%
    group_by(baseYear, measureCode) %>%
    mutate(dataValue = cumsum(dataValue)))
  res_temp <- res_temp[res_temp$valueBinCode == missing_pricebins[i] + 1, ]
  res_temp$lowBin <- new_low_bin[i]
  res_temp$valueBinDescription <- new_bin_description[i]

  wra_data <- wra_data[!(wra_data$transactionTypeCode == "RE" &
    wra_data$valueBinCode %in% c(
      missing_pricebins[i],
      missing_pricebins[i] + 1
    )), ]
  wra_data <- rbind(wra_data, res_temp)
}

## Combine residential price bins between Â£5K and Â£20K to match Higher data
res_temp <- wra_data[wra_data$transactionTypeCode == "RE" &
  wra_data$valueBinCode %in% c(2, 3, 4), ]
res_temp <- as.data.frame(res_temp %>%
  group_by(baseYear, measureCode) %>%
  mutate(dataValue = cumsum(dataValue)))
res_temp <- res_temp[res_temp$valueBinCode == 4, ]
res_temp$lowBin <- 5001
res_temp$valueBinDescription <- "Â£5,001 to Â£20,000"

wra_data <- wra_data[!(wra_data$transactionTypeCode == "RE" &
  wra_data$valueBinCode %in% c(2, 3, 4)), ]
wra_data <- rbind(wra_data, res_temp)

## Check if price RE & RH price bins match
check_value_bins <- try(unique(wra_data$valueBinCode[
  wra_data$transactionTypeCode == "RH"
]) ==
  unique(wra_data$valueBinCode[wra_data$transactionTypeCode == "RE"]))
if (unique(check_value_bins) != TRUE) {
  message("Warning: Price bins for RE & RH transactions don't match!")
}



# 1.06 DEDUCT HIGHER RATES FROM RESIDENTIAL TOTAL TO GET MAIN RATE VALUES ------

## Subset RE and RH transactions into two data frames
residential <- subset(
  wra_data,
  wra_data$transactionTypeCode == "RE"
)
residential$transactionTypeCode <- "RM"
residential$transactionTypeDescription <- "Main rates residential"

highres <- subset(
  wra_data,
  wra_data$transactionTypeCode == "RH"
)

## Arrange both data frames consistently
residential <- arrange(
  residential,
  baseYear,
  valueBinCode,
  measureCode,
  transactionTypeCode
)
highres <- arrange(
  highres,
  baseYear,
  valueBinCode,
  measureCode,
  transactionTypeCode
)

## Deduct Higher Rate values from RE total to get Main Residential values
residential_main <- residential
residential_main$dataValue <- as.numeric(residential$dataValue) -
  as.numeric(highres$dataValue)

## Append RM values to main dataset
wra_data <- rbind(wra_data, residential_main)



# 1.07 SAVE CLEANED DATA -------------------------------------------------------

## Reorder data.frame
wra_data <- arrange(
  wra_data, baseYear, transactionTypeCode,
  valueBinCode, measureCode
)

## Save cleaned data for use in next script
wra_data_cleaned <- wra_data
write.csv(wra_data_cleaned, "data/temp/wra_data_cleaned.csv")



## END OF SCRIPT
