# Create the datasets -----------------------------------------------------

library(readr)
library(dplyr)
library(stringr)

source("r/demographics.R")

file_demog <- "https://esa.un.org/unpd/wpp/DVD/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2017_TotalPopulationBySex.csv"

# create ------------------------------------------------------------------

demographics <- cr_demog(file_demog)

# join --------------------------------------------------------------------



# write -------------------------------------------------------------------

write_rds(demographics, "data/demographics.rds")
