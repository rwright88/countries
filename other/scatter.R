# Plot stuff --------------------------------------------------------------

library(tidyverse)

file1 <- "data/combined.rds"

# calc per person and changes ---------------------------------------------

combined <- read_rds(file1) %>% 
  mutate(migrants_pp = migrant_stock / population)

calc_aapc <- function(df, vars, n = 10) {
  names_aapc <- str_c(vars, "_aapc")
  n_rows <- nrow(df)
  df[names_aapc] <- vector("numeric", n_rows)
  for (i in seq_along(vars)) {
    name <- names_aapc[[1]]
    var <- vars[[1]]
    df[[name]] <- aapc(df[[var]], n)
  }
  df
}

aapc <- function(x, n) {
  ((x / lag(x, n)) ^ (1 / n) - 1) * 100
}

# data coverage -----------------------------------------------------------

calc_coverage <- function(x, pop) {
  sum(pop[!is.na(x)]) / sum(pop)
}

coverage <- combined %>% 
  group_by(year) %>% 
  summarise(
    gdppc_cov = calc_coverage(gdppc, population),
    migrants_cov = calc_coverage(migrant_stock, population),
    companies_cov = calc_coverage(companies, population),
    obesity_cov = calc_coverage(obesity_rate, population)
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
    geom_text(aes(label = country_code), size = 3.5, hjust = 0, vjust = 0) +
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
    geom_text(aes(label = country_code), size = 3.5, hjust = 0, vjust = 0) +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    scale_x_log10() + 
    scale_y_continuous(breaks = seq(-100, 100, 2)) +
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
  filter(year == 2016, population >= 1e6) %>% 
  plot_current("population", "obesity_rate") +
  scale_y_continuous(limits = c(0, NA))

combined %>% 
  filter(year == 2017, population >= 1e6) %>% 
  plot_current("gdppc", "migrants_pp")

combined %>% 
  filter(year == 2017, population >= 1e6) %>% 
  plot_current("migrants_pp", "gdppc")

combined %>% 
  filter(year == 2017, population >= 1e6, country_code != "ven") %>% 
  mutate(market_cap_pp = market_cap / population) %>% 
  plot_current("gdppc", "market_cap_pp")

combined %>% 
  filter(year == 2016, population >= 1e6) %>% 
  plot_current("gdppc", "obesity_rate") +
  scale_y_continuous(limits = c(0, NA))

# plot changes ------------------------------------------------------------

combined %>% 
  calc_aapc("population") %>% 
  filter(year == 2017, population >= 1e6, population_aapc < 6) %>% 
  plot_change("population", "population_aapc")

combined %>% 
  calc_aapc("gdppc") %>% 
  filter(year == 2017, population >= 1e6) %>% 
  plot_change("gdppc", "gdppc_aapc") + 
  coord_cartesian(ylim = c(-3, 7))

combined %>% 
  calc_aapc("gdppc") %>% 
  filter(year == 2017, population >= 1e6) %>% 
  plot_change("migrants_pp", "gdppc_aapc")
