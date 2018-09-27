# Plot stuff --------------------------------------------------------------

library(tidyverse)

file1 <- "data/combined.rds"

# calc per person and changes ---------------------------------------------

combined <- read_rds(file1) %>% 
  mutate(migrants_pp = migrant_stock / population)

calc_aapc <- function(df, x, n = 10) {
  x <- sym(x)
  df %>% 
    mutate(aapc = aapc(!!x, n))
}

aapc <- function(x, n) {
  ((x / lag(x, n)) ^ (1 / n) - 1) * 100
}

# data available ----------------------------------------------------------

avail <- combined %>% 
  group_by(year) %>% 
  summarise(
    gdppc_avail = sum(population[!is.na(gdppc)] / sum(population)),
    migrants_avail = sum(population[!is.na(migrant_stock)] / sum(population)),
    companies_avail = sum(population[!is.na(companies)] / sum(population))
  )

# setup -------------------------------------------------------------------

plot_current <- function(df, x, y) {
  x <- sym(x)
  y <- sym(y)
  df %>% 
    filter(!is.na(!!x), !is.na(!!y)) %>% 
    ggplot(aes(!!x, !!y)) +
    geom_point(aes(size = population), show.legend = FALSE, color = "#1f77b4", alpha = 0.8) + 
    geom_smooth(method = "lm", size = 0.5, color = "#1f77b4", alpha = 0.2) + 
    geom_text(aes(label = country_code), size = 3, hjust = 0, vjust = 0) +
    scale_x_log10() + 
    scale_y_log10() + 
    scale_size_continuous(range = c(1, 20)) +
    theme_bw()
}

plot_change <- function(df, x, y) {
  x <- sym(x)
  y <- sym(y)
  df %>% 
    filter(!is.na(!!x), !is.na(!!y)) %>% 
    ggplot(aes(!!x, !!y)) +
    geom_point(aes(size = population), show.legend = FALSE, color = "#1f77b4", alpha = 0.8) + 
    geom_smooth(method = "lm", size = 0.5, color = "#1f77b4", alpha = 0.2) + 
    geom_text(aes(label = country_code), size = 3, hjust = 0, vjust = 0) +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    scale_x_log10() + 
    scale_y_continuous() +
    scale_size_continuous(range = c(1, 20)) +
    theme_bw()
}

# plot current ------------------------------------------------------------

combined %>% 
  filter(year == 2017, population >= 1e6) %>% 
  plot_current("population", "gdppc")

combined %>% 
  filter(year == 2017, population >= 1e6) %>% 
  plot_current("population", "migrant_stock")

combined %>% 
  filter(year == 2017, population >= 1e6) %>% 
  plot_current("population", "migrants_pp")

combined %>% 
  filter(year == 2017, population >= 1e6) %>% 
  plot_current("population", "market_cap")

combined %>% 
  filter(year == 2017, population >= 1e6, country_code != "ven") %>% 
  mutate(market_cap_pp = market_cap / population) %>% 
  plot_current("population", "market_cap_pp")

combined %>% 
  filter(year == 2017, population >= 1e6) %>% 
  plot_current("gdppc", "migrants_pp")

combined %>% 
  filter(year == 2017, population >= 1e6, country_code != "ven") %>% 
  mutate(market_cap_pp = market_cap / population) %>% 
  plot_current("gdppc", "market_cap_pp")

# plot changes ------------------------------------------------------------

combined %>% 
  calc_aapc("population") %>% 
  filter(year == 2017, population >= 1e6, aapc < 6) %>% 
  plot_change("population", "aapc")
