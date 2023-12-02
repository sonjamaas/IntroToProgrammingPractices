library(ggplot2)
library(sf)
library(maps)

usa=st_as_sf(map('usa',plot=FALSE,fill=TRUE))                                    #get a sf object of the usa
ggplot()+
  geom_sf(data=usa)                                                              #plot the usa boundaries with ggplot2

laea=st_crs("+proj=laea +lat_0=30 +lon_0=-95")                                   #set the coordinate system
usa <- st_transform(usa,laea)                                                    #transform the usa data to the laea coordinate system

ggplot()+
  geom_sf(data=usa)                                                              #plot the usa boundaries in the real projection (curved like on a globe)

ggplot()+
  geom_sf(data = usa, aes(fill=ID))+
  scale_y_continuous()
