# Functions to create demographics dataset --------------------------------

cr_demog <- function(file, cw_countries) {
  raw <- read_csv(file, col_types = "icicidddd")
  
  demographics <- raw %>% 
    rename_all(str_to_lower) %>% 
    mutate_if(is.character, str_to_lower) %>% 
    filter(variant == "medium") %>%
    mutate(population = round(poptotal * 1e3)) %>% 
    left_join(cw_countries, by = c("location" = "name_un")) %>% 
    filter(!is.na(code3_iso)) %>% 
    select(
      country_code = code3_iso,
      country_name = name_iso,
      year = time,
      population
    )
  
  demographics
}
