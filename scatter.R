# Plot stuff --------------------------------------------------------------

library(tidyverse)

combined <- read_rds("data/combined.rds")

# setup -------------------------------------------------------------------

plot_xy <- function(df, x, y) {
  x <- sym(x)
  y <- sym(y)
  df %>% 
    filter(!is.na(!!x), !is.na(!!y)) %>% 
    ggplot(aes(!!x, !!y)) +
    geom_point(size = 2, color = "#1f77b4") + 
    geom_smooth(method = "lm", size = 1.1, color = "#1f77b4", alpha = 0.2) + 
    geom_text(aes(label = str_sub(country_name, 1, 3)), size = 3.5, hjust = 0, vjust = 0) +
    scale_x_log10() + 
    scale_y_log10() + 
    theme_bw()
}

# population vs companies -------------------------------------------------

combined %>% 
  filter(year == 2017, population >= 1e6) %>% 
  plot_xy("population", "companies")

combined %>% 
  filter(year == 2017, population >= 1e6) %>% 
  plot_xy("population", "sales")

combined %>% 
  filter(year == 2017, population >= 1e6) %>% 
  plot_xy("population", "profits")

combined %>% 
  filter(year == 2017, population >= 1e6) %>% 
  plot_xy("population", "market_cap")
