# Functions to create the obesity dataset ---------------------------------

cr_obesity <- function(file, cw_countries) {
  raw <- read_csv(file, col_types = "ccid")
  
  obesity <- raw %>% 
    mutate_if(is.character, str_to_lower) %>% 
    set_names(c("country", "code", "year", "obesity_rate")) %>% 
    mutate(obesity_rate = obesity_rate / 100) %>% 
    left_join(cw_countries, by = c("code" = "code3_iso")) %>%
    filter(!is.na(code)) %>% 
    select(
      country_code = code,
      country_name = name_iso,
      year,
      obesity_rate
    )
  
  obesity
}
