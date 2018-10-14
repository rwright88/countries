# World map ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(spData)

file1 <- "data/combined.rds"
file_gdwb <- "../other/data/API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_10081045.csv"
file_mcwb <- "data-raw/API_CM.MKT.LCAP.CD_DS2_en_csv_v2_10143648.csv"

# gddpc data from world bank ----------------------------------------------

gdppc_wb <- suppressMessages(read_csv(file_gdwb, skip = 4)) %>% 
  select(2, (length(.) - 1)) %>% 
  mutate_if(is.character, str_to_lower) %>% 
  set_names(c("code3_iso", "gdppc_wb")) %>% 
  mutate(year = 2017)

# market cap data from world bank -----------------------------------------

mcap_wb <- suppressMessages(read_csv(file_mcwb, skip = 4)) %>% 
  select(2, (length(.) - 1)) %>% 
  mutate_if(is.character, str_to_lower) %>% 
  set_names(c("code3_iso", "mcap_wb")) %>% 
  mutate(year = 2017)

# data --------------------------------------------------------------------

cw_countries <- read_csv("../countries/data-raw/cw-countries3.csv") %>% 
  select(1, 2, 3)

combined <- read_rds(file1) %>% 
  left_join(gdppc_wb, by = c("country_code" = "code3_iso", "year")) %>% 
  left_join(mcap_wb, by = c("country_code" = "code3_iso", "year")) %>% 
  left_join(cw_countries, by = c("country_code" = "code3_iso")) %>%
  mutate(mcap_wb_pp = mcap_wb / population)

names_world <- names(world)
names_world[names_world == "geom"] <- "geometry"
names(world) <- names_world
st_geometry(world) <- "geometry"

world$iso_a2[world$name_long == "Norway"] <- "no"
world$iso_a2[world$name_long == "France"] <- "fr"

world_robin <- world %>% 
  filter(!name_long %in% c("Northern Cyprus", "Somaliland")) %>% 
  mutate(iso_a2 = str_to_lower(iso_a2)) %>% 
  st_transform(world, crs = "+proj=robin")

# plot --------------------------------------------------------------------

map_world <- function(.data, x, year1, limits, reverse = FALSE, world = world_robin) {
  x <- sym(x)
  dat <- filter(.data, year == year1)
  dat[[x]][dat[[x]] < limits[[1]]] <- limits[[1]]
  dat[[x]][dat[[x]] > limits[[3]]] <- limits[[3]]
  world_data <- left_join(world, dat, by = c("iso_a2" = "code2_iso"))
  colors <- RColorBrewer::brewer.pal(11, "RdBu")
  if (reverse == TRUE) {
    colors <- colors[11:1]
  }
  
  ggplot(world_data) +
    geom_sf(aes(fill = !!x), size = 0.1, color = "#333333", alpha = 1) +
    scale_fill_gradientn(
      trans = "log",
      limits = limits[c(1, 3)],
      breaks = limits,
      colors = colors
    ) +
    coord_sf(ylim = c(-5.5e6, 8e6)) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank()
    )
}

map_world(combined, "gdppc", 2017, c(3e3, 1.5e4, 7.5e4))
map_world(combined, "gdppc_wb", 2017, c(3e3, 1.5e4, 7.5e4))
map_world(combined, "migrants_pp", 2017, c(0.0034, 0.034, 0.34))
map_world(combined, "mcap_wb_pp", 2017, c(1.3e3, 1.3e4, 1.3e5))
map_world(combined, "obesity_rate", 2016, c(4, 13, 40), reverse = TRUE)
map_world(combined, "homicide_rate", 2015, c(5e-6, 5e-5, 5e-4), reverse = TRUE)

# ggsave("other/images/test-map.png", dpi = 300, width = 19, height = 9)
