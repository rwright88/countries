# Relationships and correlates --------------------------------------------

library(tidyverse)

file1     <- "data/combined.rds"
file_gdwb <- "../other/data/API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_10081045.csv"
file_mcwb <- "data-raw/API_CM.MKT.LCAP.CD_DS2_en_csv_v2_10143648.csv"
file_cw   <- "data-raw/cw-countries3.csv"

# data --------------------------------------------------------------------

cw_region <- read_csv(file_cw, col_types = "ccccccccc") %>% 
  select(code3_iso, region, sub_region)

gdppc_wb <- suppressMessages(read_csv(file_gdwb, skip = 4)) %>% 
  select(2, (length(.) - 1)) %>% 
  mutate_if(is.character, str_to_lower) %>% 
  set_names(c("code3_iso", "gdppc_wb")) %>% 
  mutate(year = 2017L)

mcap_wb <- suppressMessages(read_csv(file_mcwb, skip = 4)) %>% 
  select(2, 5:(length(.) - 1)) %>% 
  gather("year", "mcap_wb", -1, convert = TRUE) %>% 
  set_names(c("code3_iso", "year", "mcap_wb")) %>% 
  mutate_if(is.character, str_to_lower) %>% 
  mutate(mcap_wb = as.numeric(mcap_wb))

combined <- read_rds(file1) %>% 
  left_join(gdppc_wb, by = c("country_code" = "code3_iso", "year")) %>% 
  left_join(mcap_wb, by = c("country_code" = "code3_iso", "year")) %>% 
  left_join(cw_region, by = c("country_code" = "code3_iso")) %>%
  mutate(mcap_wb_pp = mcap_wb / population)

# data coverage -----------------------------------------------------------

calc_coverage <- function(x, pop) {
  sum(pop[!is.na(x)]) / sum(pop)
}

coverage <- combined %>% 
  group_by(year) %>% 
  summarise(
    pop_cov = calc_coverage(population, population),
    gdppc_cov = calc_coverage(gdppc, population),
    gdppc_wb_cov = calc_coverage(gdppc_wb, population),
    migrants_cov = calc_coverage(migrant_stock, population),
    companies_cov = calc_coverage(companies, population),
    obesity_cov = calc_coverage(obesity_rate, population),
    homicide_cov = calc_coverage(homicide_rate, population),
    mcap_wb_cov = calc_coverage(mcap_wb, population)
  )

# funs --------------------------------------------------------------------

plot_current <- function(dat, x, y) {
  x <- sym(x)
  y <- sym(y)
  dat %>% 
    filter(!is.na(!!x), !is.na(!!y)) %>% 
    ggplot(aes(!!x, !!y, color = region)) +
    geom_point(aes(size = population), alpha = 0.8) + 
    geom_smooth(method = "loess", span = 1, size = 0.5, color = "#1f77b4", se = FALSE) +
    scale_x_log10() + 
    scale_y_log10() + 
    scale_size_continuous(range = c(1, 20)) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    guides(size = "none") +
    theme_bw()
}

plot_change <- function(dat, x, y) {
  plot_current(dat, x, y) +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    scale_y_continuous(breaks = seq(-100, 100, 2))
}

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

# plot current ------------------------------------------------------------

combined %>% 
  filter(year == 2017, population >= 1e6) %>% 
  plot_current("population", "gdppc")

combined %>% 
  filter(year %in% 2013:2017, population >= 1e6) %>% 
  group_by(country_code, region) %>% 
  summarise(
    population = mean(population),
    gdppc_wb = mean(gdppc_wb, na.rm = TRUE),
    mcap_wb_pp = mean(mcap_wb_pp, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  plot_current("gdppc_wb", "mcap_wb_pp")

combined %>% 
  filter(year == 2016, population >= 1e6) %>% 
  plot_current("gdppc", "obesity_rate") +
  scale_y_continuous(limits = c(0, NA))

combined %>% 
  filter(year %in% 2008:2017, population >= 1e6) %>% 
  group_by(country_code, region) %>% 
  summarise(
    population = mean(population),
    gdppc_wb = mean(gdppc_wb, na.rm = TRUE),
    homicide_rate = mean(homicide_rate, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  plot_current("gdppc_wb", "homicide_rate")

# plot changes ------------------------------------------------------------

combined %>% 
  calc_aapc("population") %>% 
  filter(year == 2017, population >= 1e6, population_aapc < 6) %>% 
  plot_change("population", "population_aapc")

combined %>% 
  calc_aapc("gdppc", 37) %>% 
  filter(year == 2017, population >= 1e6, gdppc < 1e5) %>% 
  plot_change("gdppc", "gdppc_aapc") +
  coord_cartesian(xlim = c(1e3, 1e5), ylim = c(-2, 6))
