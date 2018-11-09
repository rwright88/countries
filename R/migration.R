# Functions to create the migration dataset -------------------------------

cr_migration <- function(file, cw_countries) {
  raw <- read_excel(file, sheet = "Table 1", col_types = "text", skip = 15, na = "..")
  
  stock <- raw %>% 
    select(code = 5, country_name = 3, year = 1, migrants = 7) %>%
    mutate_all(str_to_lower) %>% 
    mutate(
      code = str_pad(code, width = 3, pad = "0"),
      year = as.integer(year),
      migrants = fix_na_int(migrants)
    ) %>% 
    left_join(cw_countries, by = c("code" = "code_iso")) %>%
    filter(!is.na(code3_iso)) %>% 
    select(
      country_code = code3_iso,
      country_name = name_iso,
      year,
      migrants
    )
  
  stock
}

fix_na_int <- function(x) {
  x <- as.integer(x)
  x[is.na(x)] <- 0L
  x
}
