# Functions to create the migration dataset -------------------------------

cr_migration <- function(file, file_cw, file_non_cou) {
  raw <- read_excel(file, sheet = "Table 1", skip = 15)
  cw_countries <- read_csv(file_cw, col_types = "cccc")
  non_countries <- read_csv(file_non_cou, col_types = "ic") %>% 
    .[["iso_code"]]
  
  stock <- raw %>% 
    select(country_name = 3, year = 1, migrant_stock = 7) %>% 
    mutate_if(is.character, str_to_lower) %>% 
    mutate(migrant_stock = if_else(migrant_stock == "..", 0L, as.integer(migrant_stock))) %>% 
    left_join(cw_countries, by = c("country_name" = "name_un")) %>% 
    select(country_name = name_custom, year, migrant_stock)
}
