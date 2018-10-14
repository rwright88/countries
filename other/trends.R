# Trends ------------------------------------------------------------------

library(tidyverse)

file1 <- "data/combined.rds"

# data --------------------------------------------------------------------

combined <- read_rds(file1)

# setup -------------------------------------------------------------------

plot_trends <- function(dat, x) {
  x <- sym(x)
  dat %>% 
    filter(!is.na(!!x)) %>% 
    mutate(country_code = reorder(country_code, desc(!!x))) %>% 
    ggplot(aes(year, !!x, color = country_code)) +
    geom_point(size = 2) +
    geom_line(size = 1.1) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    theme_bw()
}

# plot trends -------------------------------------------------------------

codes <- c("usa", "deu", "gbr", "fra")

combined %>% 
  filter(country_code %in% codes, year <= 2018) %>% 
  plot_trends("population") +
  scale_y_log10()

combined %>% 
  filter(country_code %in% codes) %>% 
  plot_trends("gdppc") +
  scale_y_log10(breaks = c(seq(1e3, 1e4, 1e3), seq(1e4, 1e5, 1e4)), minor_breaks = NULL)

combined %>% 
  filter(country_code %in% codes) %>% 
  plot_trends("migrants_pp") +
  scale_y_continuous(limits = c(0, NA))

combined %>% 
  filter(country_code %in% codes, population >= 1e6) %>%
  plot_trends("obesity_rate") +
  scale_y_continuous(limits = c(0, NA))

combined %>% 
  filter(country_code %in% codes, population >= 1e6) %>%
  plot_trends("homicide_rate") +
  scale_y_continuous(limits = c(0, NA))
