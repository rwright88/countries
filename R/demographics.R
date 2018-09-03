# Functions to create demographics dataset --------------------------------

cr_demog <- function(file, file_non) {
  raw <- read_csv(file, col_types = "icicidddd")
  
  non_countries <- read_csv(file_non, col_types = "ic") %>% 
    .[["iso_code"]]
  
  demographics <- raw %>% 
    rename_all(str_to_lower) %>% 
    mutate_if(is.character, str_to_lower) %>% 
    filter(variant == "medium", !(locid %in% non_countries)) %>% 
    mutate(population = round(poptotal * 1e3)) %>% 
    select(
      country_code = locid,
      country_name = location,
      year = time,
      population
    )
  
  demographics
}
