# Functions to create the income dataset ----------------------------------

cr_income <- function(file) {
  raw <- read_tsv(file_inc, col_types = "cccccccid")
  
  inds <- c("emp", "avghr", "pc_eks", "lp_eksl", "lp_eksh")
  
  cleaned <- raw %>% 
    rename_all(str_to_lower) %>% 
    mutate_if(is.character, str_to_lower) %>% 
    filter(country_iso != "chn2", indicator_short %in% inds) %>% 
    select(country_iso, country_name = country, year, indicator_short, value) %>% 
    spread(indicator_short, value) %>% 
    mutate(emp = emp * 1e3) %>% 
    select(country_iso, country_name, year, inds)
  
  cleaned
}
