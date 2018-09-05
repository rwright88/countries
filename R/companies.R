# Functions to create companies dataset -----------------------------------

cr_companies <- function(file, file_cw) {
  raw <- read_csv(file, col_types = "ccccccc")
  cw_countries <- read_csv(file_cw, col_types = "cccc")
  
  cleaned <- raw %>% 
    mutate_if(is.character, str_to_lower) %>% 
    mutate_at(vars(sales:market_cap), fix_dollars) %>% 
    left_join(cw_countries, by = c("country_name" = "name_forbes")) %>% 
    select(country_name = name_custom, sales:market_cap)
  
  country_sums <- cleaned %>% 
    group_by(country_name) %>% 
    summarise(
      year = 2017L,
      companies = n(),
      sales = sum(sales),
      profits = sum(profits),
      market_cap = sum(market_cap)
    ) %>% 
    ungroup()
  
  country_sums
}

fix_dollars <- function(x) {
  removed <- x %>% 
    str_remove_all("\\$") %>% 
    str_remove_all(",")
  
  cleaned <- suppressWarnings(case_when(
    str_detect(removed, "b") ~ as.numeric(str_remove_all(removed, "b")) * 1e9,
    str_detect(removed, "m") ~ as.numeric(str_remove_all(removed, "m")) * 1e6,
    str_detect(removed, "^-$") ~ 0,
    TRUE ~ NA_real_
  ))
  
  cleaned
}
