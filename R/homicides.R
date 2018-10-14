# Functions to create the homicide dataset --------------------------------

cr_homicides <- function(file, cw_countries) {
  raw <- read_excel(file, sheet = 1, col_types = "text", skip = 5)
  
  homicides <- raw %>% 
    select(country_name = 3, matches("^\\d{4}$")) %>% 
    gather("year", "homicides", -country_name) %>% 
    mutate_if(is.character, str_to_lower) %>% 
    mutate(
      year = as.integer(year),
      homicides = as.numeric(homicides)
    ) %>% 
    left_join(cw_countries, by = c("country_name" = "name_iso")) %>% 
    filter(!is.na(code3_iso)) %>% 
    select(
      country_code = code3_iso,
      country_name,
      year,
      homicides
    )
  
  homicides
}
