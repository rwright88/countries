# World map ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(spData)

file1 <- "data/combined.rds"
file_wb <- "../other/data/API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_10081045.csv"

# data --------------------------------------------------------------------

cw_countries <- read_csv("../countries/data-raw/cw-countries3.csv") %>% 
  select(1, 2, 3)

gdppc_wb <- suppressMessages(read_csv(file_wb, skip = 4)) %>% 
  select(2, (length(.) - 1)) %>% 
  mutate_if(is.character, str_to_lower) %>% 
  set_names(c("code3_iso", "gdppc_wb"))

combined <- read_rds(file1) %>%
  mutate(migrants_pp = migrant_stock / population) %>% 
  mutate(market_cap_pp = market_cap / population) %>% 
  left_join(gdppc_wb, by = c("country_code" = "code3_iso")) %>% 
  left_join(cw_countries, by = c("country_code" = "code3_iso"))

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

map_world <- function(.data, x, year1, limits, world = world_robin) {
  x <- sym(x)
  dat <- filter(.data, year == year1)
  dat[[x]][dat[[x]] < limits[[1]]] <- limits[[1]]
  dat[[x]][dat[[x]] > limits[[3]]] <- limits[[3]]
  world_data <- left_join(world, dat, by = c("iso_a2" = "code2_iso"))
  
  ggplot(world_data) +
    geom_sf(aes(fill = !!x), size = 0.1, color = "#333333", alpha = 1) +
    scale_fill_gradientn(
      trans = "log",
      limits = limits[c(1, 3)],
      breaks = limits,
      colors = RColorBrewer::brewer.pal(11, "RdBu")
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
map_world(combined, "migrants_pp", 2017, c(0.003, 0.03, 0.3))
map_world(combined, "market_cap_pp", 2017, c(1e3, 1e4, 1e5))
map_world(combined, "obesity_rate", 2016, c(4, 13, 40))

ggsave("other/test-map.png", dpi = 300, width = 19, height = 9)
