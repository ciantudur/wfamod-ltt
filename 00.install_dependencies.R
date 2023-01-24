# ---------------------------------------------------------------------------- #
# WFAMOD-LTT ----------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# 00.install_dependencies.R

# DATE CREATED:  27 September 2022
# LAST MODIFIED: 05 September 2022
# AUTHOR: Cian Sion


# DESCRIPTION:
# This script checks if required packages are installed and installs them if
# necessary. It only needs to be run once.



# 0.01 INSTALL PACKAGES --------------------------------------------------------

list_of_packages <- c("dplyr",
                      "openxlsx",
                      "readODS",
                      "stringr",
                      "stats",
                      "statswalesr",
                      "tidyr",
                      "utils")

new_packages <- list_of_packages[!(
  list_of_packages %in% installed.packages()[, "Package"])]

if (length(new_packages)) install.packages(new_packages, dependencies = TRUE)
