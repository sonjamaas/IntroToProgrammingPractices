install.packages("sf")
library(sf)
library(ggplot2)
library(rworldmap)
library(ggspatial)
#install.packages("rworldmap")
world <- getMap()

world
class(world)
plot(world)
View(world)

ger <- world[(which(world$NAME=="Germany")),]                                    #select germany from the world dataset
ger_sf <- st_as_sf(ger)

ggplot() +
  geom_sf(data = ger_sf, fill = "lightblue", color = "black") +
  theme_minimal() +
  ggtitle("Germany")                                                             #plot germany map


ggplot()+
  geom_sf(data=ger_sf)+
  annotation_scale(location="bl", width_hint=0.5)+
  annotation_north_arrow(location="bl", which_north="true",
                         pad_x=unit(0.2,"in"), pad_y=unit(0.5,"in"),
                         style=north_arrow_fancy_orienteering)                   #plot germany with north arrow and scale bar

