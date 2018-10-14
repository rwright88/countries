# Data coverage by region by year -----------------------------------------

library(tidyverse)

combined <- read_rds("data/combined.rds")

cw_countries <- read_csv("data-raw/cw-countries3.csv", col_types = "ccccccccc") %>% 
  select(code3_iso, region, sub_region) %>% 
  filter(!is.na(sub_region))

calc_coverage <- function(x, pop) {
  sum(pop[!is.na(x)]) / sum(pop)
}

coverage <- combined %>% 
  left_join(cw_countries, by = c("country_code" = "code3_iso")) %>% 
  group_by(sub_region, year) %>% 
  summarise(
    pop_cov = calc_coverage(population, population),
    gdppc_cov = calc_coverage(gdppc, population),
    migrants_cov = calc_coverage(migrant_stock, population),
    companies_cov = calc_coverage(companies, population),
    obesity_cov = calc_coverage(obesity_rate, population),
    homicide_cov = calc_coverage(homicide_rate, population)
  ) %>% 
  ungroup()

cov_var <- sym("gdppc_cov")

coverage %>% 
  filter(year <= 2018, !sub_region %in% c("melanesia", "polynesia", "micronesia")) %>% 
  ggplot(aes(year, !!cov_var)) +
  geom_line(size = 1.1, color = "#1f77b4") +
  facet_wrap(~sub_region, ncol = 5) +
  theme_bw()
