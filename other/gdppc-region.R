# GDP per capita by region ------------------------------------------------

library(tidyverse)

combined <- read_rds("data/combined.rds")

cw_countries <- read_csv("data-raw/cw-countries3.csv", col_types = "ccccccccc") %>% 
  select(code3_iso, region, sub_region) %>% 
  filter(!is.na(sub_region))

gdppc_subreg <- combined %>% 
  left_join(cw_countries, by = c("country_code" = "code3_iso")) %>% 
  filter(!is.na(gdppc)) %>% 
  group_by(year, sub_region) %>% 
  summarise(
    n = n(),
    pop = sum(population),
    gdppc = weighted.mean(gdppc, population)
  ) %>% 
  ungroup()

gdppc_subreg %>% 
  filter(year == max(year)) %>% 
  arrange(desc(pop))

order_subreg <- gdppc_subreg %>% 
  filter(year == max(year)) %>% 
  arrange(desc(gdppc)) %>% 
  .[["sub_region"]]

gdppc_subreg %>% 
  mutate(sub_region = factor(sub_region, order_subreg)) %>% 
  ggplot(aes(year, gdppc)) +
  geom_line(size = 1.1, color = "#1f77b4") +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  coord_cartesian(ylim = c(1e3, 1e5)) +
  facet_wrap(~sub_region) +
  theme_bw()
