install.packages("sf")
library(sf)
library(ggplot2)
library(rworldmap)
#install.packages("rworldmap")
world <- getMap()

world
class(world)
plot(world)
View(world)

ger <- world[(which(world$NAME=="Germany")),]                                    #select germany from the world dataset


ggplot() +
  geom_sf(data = ger_sf, fill = "lightblue", color = "black") +
  theme_minimal() +
  ggtitle("Germany")



