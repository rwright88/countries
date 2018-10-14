# Create the datasets -----------------------------------------------------

library(readr)
library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

source("R/demographics.R")
source("R/income.R")
source("R/companies.R")
source("R/migration.R")
source("R/obesity.R")
source("R/homicides.R")

file_cw   <- "data-raw/cw-countries3.csv"
file_demo <- "https://esa.un.org/unpd/wpp/DVD/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2017_TotalPopulationBySex.csv"
file_inc  <- "https://www.conference-board.org/retrievefile.cfm?filename=TED_FLATFILE_ORI_MAR20181.txt&type=subsite"
file_comp <- "data-raw/global-2000.csv"
file_migr <- "data-raw/UN_MigrantStockByOriginAndDestination_2017.xlsx"
file_obes <- "data-raw/obesity.csv"
file_homi <- "data-raw/homicide_report_total_and_sex.xlsx"

# create ------------------------------------------------------------------

cw_countries <- read_csv(file_cw, col_types = "ccccccccc")

demographics <- cr_demog(file_demo, cw_countries)
income       <- cr_income(file_inc, cw_countries)
migration    <- cr_migration(file_migr, cw_countries)
companies    <- cr_companies(file_comp, cw_countries)
obesity      <- cr_obesity(file_obes, cw_countries)
homicides    <- cr_homicides(file_homi, cw_countries)

# join and rates ----------------------------------------------------------

by_join <- c("country_code", "country_name", "year")

combined <- demographics %>% 
  left_join(income, by = by_join) %>% 
  left_join(migration, by = by_join) %>% 
  mutate(migrants_pp = migrant_stock / population) %>% 
  left_join(companies, by = by_join) %>% 
  left_join(obesity, by = by_join) %>% 
  left_join(homicides, by = by_join) %>% 
  mutate(homicide_rate = homicides / population)

# write -------------------------------------------------------------------

write_rds(demographics, "data/demographics.rds")
write_rds(income, "data/income.rds")
write_rds(migration, "data/migration.rds")
write_rds(companies, "data/companies.rds")
write_rds(obesity, "data/obesity.rds")
write_rds(homicides, "data/homicides.rds")
write_rds(combined, "data/combined.rds")
