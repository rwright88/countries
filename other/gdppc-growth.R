# GDP per capita growth trends --------------------------------------------

library(tidyverse)

codes <- c("usa", "can")

combined %>% 
  calc_aapc("gdppc", 10) %>% 
  filter(country_code %in% codes, year <= 2018) %>% 
  ggplot(aes(year, gdppc_aapc, color = country_code)) + 
  geom_point(size = 2) +
  geom_line(size = 1.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_color_brewer(type = "qual", palette = "Set1") +
  theme_bw()
