# Functions to create the income dataset ----------------------------------

cr_income <- function(file, cw_countries) {
  raw <- read_tsv(file, col_types = "cccccccid")
  inds <- c("emp", "pc_eks", "lp_eksl")
  renamed <- c("employed", "gdppc", "gdppw")
  
  cleaned <- raw %>% 
    rename_all(str_to_lower) %>% 
    mutate_if(is.character, str_to_lower) %>% 
    filter(country_iso != "chn2", indicator_short %in% inds) %>% 
    mutate(country_iso = if_else(country_iso == "chn1", "chn", country_iso)) %>% 
    select(country_iso, year, indicator_short, value)
  
  wide <- cleaned %>% 
    spread(indicator_short, value) %>% 
    mutate(emp = emp * 1e3) %>% 
    left_join(cw_countries, by = c("country_iso" = "code3_iso")) %>% 
    select(country_iso, name_iso, year, inds) %>% 
    set_names(c("country_code", "country_name", "year", renamed))
  
  wide
}
