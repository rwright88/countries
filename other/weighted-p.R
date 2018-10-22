# Population weighted percentiles -----------------------------------------

library(tidyverse)
library(Hmisc)

combined <- read_rds("data/combined.rds")

var <- sym("gdppc")
year1 <- 2017

combined %>% 
  select(1:4, !!var) %>% 
  filter(year == year1, population >= 1e6) %>% 
  group_by(year) %>% 
  mutate(r = wtd.rank(!!var, population)) %>% 
  mutate(p = r / max(r, na.rm = TRUE)) %>% 
  arrange(desc(p))
