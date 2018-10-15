# Functions to create the homicide dataset --------------------------------

cr_homicides <- function(file, cw_countries) {
  raw <- read_excel(file, sheet = 1, col_types = "text", skip = 5)
  
  homicides <- raw %>% 
    select(country_name = 3, matches("^\\d{4}$")) %>% 
    gather("year", "homicides", -country_name) %>% 
    mutate_if(is.character, str_to_lower) %>% 
    mutate(
      country_name = fix_names_hom(str_remove_all(country_name, "\\*|\\#")),
      year = as.integer(year),
      homicides = as.numeric(homicides)
    ) %>% 
    left_join(cw_countries, by = c("country_name" = "name_un")) %>% 
    filter(!is.na(code3_iso)) %>% 
    select(
      country_code = code3_iso,
      country_name = name_iso,
      year,
      homicides
    )
  
  homicides
}

# temporary, should be in country crosswalk
fix_names_hom <- function(x) {
  x[x == "congo (democratic republic of the)"] <- "democratic republic of the congo"
  x[x == "united kingdom of great britain and northern ireland"] <- "united kingdom"
  x[x == "korea (republic of)"] <- "republic of korea"
  # x[x == "united republic of tanzania"] <- "united republic of tanzania"
  x[x == "democratic people's republic of korea"] <- "dem. people's republic of korea"
  # x[x == "china, taiwan province of china"] <- "china, taiwan province of china"
  x
}
