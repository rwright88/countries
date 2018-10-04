# Data coverage by year ---------------------------------------------------

library(tidyverse)

combined <- read_rds("data/combined.rds")

cw_countries <- read_csv("data-raw/cw-countries3.csv", col_types = "ccccccccc") %>% 
  select(code3_iso, region, sub_region) %>% 
  filter(!is.na(sub_region))

coverage_subreg <- combined %>% 
  left_join(cw_countries, by = c("country_code" = "code3_iso")) %>% 
  group_by(sub_region, year) %>% 
  summarise(
    pop = sum(population),
    pop_inc = sum(population[!is.na(gdppc)])
  ) %>% 
  ungroup() %>% 
  mutate(cov_ratio = pop_inc / pop)

coverage_subreg %>% 
  filter(year <= 2018, !sub_region %in% c("melanesia", "polynesia", "micronesia")) %>% 
  ggplot(aes(year, cov_ratio)) +
  geom_line(size = 1.1, color = "#1f77b4") +
  facet_wrap(~sub_region) +
  theme_bw()
