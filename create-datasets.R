# Create the datasets -----------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(stringr)

source("r/demographics.R")
source("r/income.R")
source("r/companies.R")

file_cw   <- "data/cw-countries.csv"
file_non  <- "data/non-countries.csv"
file_demo <- "https://esa.un.org/unpd/wpp/DVD/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2017_TotalPopulationBySex.csv"
file_inc  <- "https://www.conference-board.org/retrievefile.cfm?filename=TED_FLATFILE_ORI_MAR20181.txt&type=subsite"
file_comp <- "data/global-2000.csv"

# create ------------------------------------------------------------------

# demographics <- cr_demog(file_demo, file_non)
demographics <- read_rds("data/demographics.rds")  # temp ~~~~~
companies <- cr_companies(file_comp, file_cw)

# join --------------------------------------------------------------------

by_join <- c("country_name", "year")

combined <- demographics %>% 
  left_join(companies, by = by_join)

# write -------------------------------------------------------------------

write_rds(demographics, "data/demographics.rds")
write_rds(companies, "data/companies.rds")
write_rds(combined, "data/combined.rds")
