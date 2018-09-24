# Plot stuff --------------------------------------------------------------

library(tidyverse)

# read --------------------------------------------------------------------

combined <- read_rds("data/combined.rds")

# setup -------------------------------------------------------------------

plot_xy <- function(df, x, y) {
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

# population vs income ----------------------------------------------------

combined %>% 
  filter(year == 2017, population >= 1e6) %>% 
  plot_xy("population", "gdppc")

# population vs migrants --------------------------------------------------

combined %>% 
  filter(year == 2017, population >= 1e6) %>% 
  plot_xy("population", "migrant_stock")

combined %>% 
  filter(year == 2017, population >= 1e6) %>% 
  mutate(migrants_pp = migrant_stock / population) %>% 
  plot_xy("population", "migrants_pp")

# population vs companies -------------------------------------------------

combined %>% 
  filter(year == 2017, population >= 1e6) %>% 
  plot_xy("population", "market_cap")

# income vs migrants --------------------------------------------------

combined %>% 
  filter(year == 2017, population >= 1e6) %>% 
  mutate(migrants_pp = migrant_stock / population) %>% 
  plot_xy("gdppc", "migrants_pp")

# income vs companies -----------------------------------------------------

combined %>% 
  filter(year == 2017, population >= 1e6, !str_detect(country_name, "venez")) %>% 
  mutate(market_cap_pp = market_cap / population) %>% 
  plot_xy("gdppc", "market_cap_pp")
