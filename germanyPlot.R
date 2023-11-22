library(ggplot2)
library(rworldmap)
#install.packages("rworldmap")
world <- getMap()

world
class(world)
plot(world)
View(world)

ger_index <- which(world$NAME=="Germany")

#Check if Germany is found
if (length(ger_index) > 0) {
  # Subset the world data for Germany
  ger <- world[ger_index, ]
  
  # Convert to sf object
  ger_sf <- st_as_sf(ger)
  
  # Plot with ggplot and geom_sf
  ggplot() +
    geom_sf(data = ger_sf, fill = "lightblue", color = "black") +
    theme_minimal() +
    ggtitle("Germany")
} else {
  print("Germany not found in the world data.")
}



#ger <- world[world$NAME=="Germany",]
#View(ger)

#ger.sf <- st_as_sf(ger)

#ggplot()+
#  geom_sf(data=ger.sf,aes(x=LON,y=LAT,group=REGION,fill=value),
#               colour="black",size=0.1)+
#  coord_map(xlim=c(~13,35),ylim=c(32,71))
