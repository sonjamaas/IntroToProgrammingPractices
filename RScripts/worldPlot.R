library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale="medium",returnclass="sf")                           #get natural earth world country polygons
class(world)                                                                     #get the class of object "world"

ggplot(data=world)+
  geom_sf()                                                                      #plot the worldmap with ggplot2

ggplot(data=world)+
  geom_sf(color="grey",fill="darkgreen")                                         #plt the world map in green with grey boundaries

ggplot(data=world)+
  geom_sf(aes(fill=pop_est))+
  scale_fill_viridis_c(option="inferno", trans="sqrt")                           #plot the world map coloured by population

ggplot(data=world)+
  geom_sf()+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("World Map", 
          subtitle=paste0("(Displaying ",
                          length(unique(world$name)), 
                          " countries)"))                                        #plot the world map and add axis text and title that says how many countries are plotted

ggplot(data=world)+
  geom_sf()+
  coord_sf(crs="+proj=laea 
           +lat_0=52 
           +lon_0=10 
           +x_0=4321000 
           +y_0=3210000 
           +ellps=GRS80 
           +units=m +no_defs ")                                                  #plot the world map as a globe

