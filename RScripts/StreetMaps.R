install.packages("osmdata")
library(tidyverse)
library(osmdata)

available_tags("highway")

available_features()

getbb("Würzburg Germany")


streets <- getbb("Würzburg Germany")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()
streets

small_streets <- getbb("Würzburg Germany")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

river <- getbb("Würzburg Germany")%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .4,
          alpha = .8) +
  coord_sf(xlim = c(9.871628, 10.01443), 
           ylim = c(49.710684, 49.84546),
           expand = FALSE)

ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "#7fc0ff",
          size = .4,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#ffbe7f",
          size = .4,
          alpha = .6) +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "#ffbe7f",
          size = .2,
          alpha = .5) +
  coord_sf(xlim = c(9.871628, 10.01443), 
           ylim = c(49.710684, 49.84546),
           expand = FALSE)+
  theme_void()+
  theme(
    plot.background = element_rect(fill = "#282828")
  )

ggsave("map.png", width = 6, height = 6)
