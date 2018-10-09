# World map ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(spData)

file1 <- "data/combined.rds"

# data --------------------------------------------------------------------

cw_countries <- read_csv("../countries/data-raw/cw-countries3.csv") %>% 
  select(1, 2, 3)

combined <- read_rds(file1) %>%
  mutate(migrants_pp = migrant_stock / population) %>% 
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

map_world <- function(x, year1, .data = combined, world = world_robin) {
  x <- sym(x)
  dat <- filter(.data, year == year1)
  
  world_data <- world %>% 
    left_join(dat, by = c("iso_a2" = "code2_iso"))
  
  colors <- RColorBrewer::brewer.pal(11, "RdYlBu")
  
  ggplot(world_data) +
    geom_sf(aes(fill = !!x), size = 0.1, color = "#444444") +
    scale_fill_gradientn(colors = colors, trans = "log", limits = c(0.0005, 0.9)) +
    coord_sf(ylim = c(-5.5e6, 8e6)) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank()
    )
}

map_world("migrants_pp", 2017L)

ggsave("world-map.png", dpi = 300, width = 19, height = 9)
