# Functions to create the income dataset ----------------------------------

cr_income <- function(file, file_cw) {
  raw <- read_tsv(file, col_types = "cccccccid")
  cw_countries <- read_csv(file_cw, col_types = "cccc")
  
  inds <- c("emp", "avghr", "pc_eks", "lp_eksl", "lp_eksh")
  
  cleaned <- raw %>% 
    rename_all(str_to_lower) %>% 
    mutate_if(is.character, str_to_lower) %>% 
    filter(country_iso != "chn2", indicator_short %in% inds) %>% 
    select(country_iso, country_name = country, year, indicator_short, value) %>% 
    spread(indicator_short, value) %>% 
    mutate(emp = emp * 1e3) %>% 
    left_join(cw_countries, by = c("country_name" = "name_ted")) %>% 
    select(country_name = name_custom, year, inds)
  
  cleaned
}
