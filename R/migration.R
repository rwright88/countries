# Functions to create the migration dataset -------------------------------

cr_migration <- function(file, cw_countries) {
  raw <- read_excel(file, sheet = "Table 1", col_types = "text", skip = 15, na = "..")
  
  stock <- raw %>% 
    select(code = 5, country_name = 3, year = 1, migrant_stock = 7) %>%
    mutate_all(str_to_lower) %>% 
    mutate(
      code = str_pad(code, width = 3, pad = "0"),
      year = as.integer(year),
      migrant_stock = fix_na_int(migrant_stock)
    ) %>% 
    left_join(cw_countries, by = c("code" = "code_iso")) %>%
    filter(!is.na(code3_iso)) %>% 
    select(
      country_code = code3_iso,
      country_name = name_iso,
      year,
      migrant_stock
    )
  
  stock
}

fix_na_int <- function(x) {
  x <- as.integer(x)
  x[is.na(x)] <- 0L
  x
}
