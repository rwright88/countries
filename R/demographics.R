# Functions to create demographics dataset --------------------------------

cr_demog <- function(file) {
  raw <- read_csv(file, col_types = "icicidddd")
  
  demographics <- raw %>% 
    rename_all(str_to_lower) %>% 
    mutate_if(is.character, str_to_lower) %>% 
    filter(variant == "medium") %>% 
    mutate(population = poptotal * 1e3) %>% 
    select(
      country_code = locid,
      country_name = location,
      year = time,
      population
    )
  
  demographics
}
