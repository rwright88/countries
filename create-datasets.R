# Create the datasets -----------------------------------------------------

library(readr)
library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

source("r/demographics.R")
source("r/income.R")
source("r/companies.R")
source("r/migration.R")

file_cw   <- "data-raw/cw-countries3.csv"
file_demo <- "https://esa.un.org/unpd/wpp/DVD/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2017_TotalPopulationBySex.csv"
file_inc  <- "https://www.conference-board.org/retrievefile.cfm?filename=TED_FLATFILE_ORI_MAR20181.txt&type=subsite"
file_comp <- "data-raw/global-2000.csv"
file_migr <- "data-raw/UN_MigrantStockByOriginAndDestination_2017.xlsx"

# create ------------------------------------------------------------------

cw_countries <- read_csv(file_cw, col_types = "ccccccccc")

demographics <- cr_demog(file_demo, cw_countries)
income       <- cr_income(file_inc, cw_countries)
migration    <- cr_migration(file_migr, cw_countries)
companies    <- cr_companies(file_comp, cw_countries)

# join --------------------------------------------------------------------

by_join <- c("country_code", "country_name", "year")

combined <- demographics %>% 
  left_join(income, by = by_join) %>% 
  left_join(migration, by = by_join) %>% 
  left_join(companies, by = by_join)

# write -------------------------------------------------------------------

write_rds(demographics, "data/demographics.rds")
write_rds(income, "data/income.rds")
write_rds(migration, "data/migration.rds")
write_rds(companies, "data/companies.rds")
write_rds(combined, "data/combined.rds")
