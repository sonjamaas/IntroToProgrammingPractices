## Get into nature the fastest way possible
install.packages("mapsf")
library(tidyverse)
library(osmdata)
library(sf)
library(nominatimlite)
library(tidyverse)
library(mapsf)


# get bounding box coordinates
boundingBox <- getbb("Grombühl Würzburg Germany")
xlim <- c(boundingBox[1,1],boundingBox[1,2])
ylim <- c(boundingBox[2,1],boundingBox[2,2])

# make these coordinates to a polygon
aoi <- bbox_to_poly(boundingBox)


# extracting forest types from osm
forest <- boundingBox%>%
  opq()%>%
   add_osm_feature(key="landuse" , 
                   value = "forest" ) %>%
  osmdata_sf()


mixedforest <- boundingBox%>%
  opq()%>%
  add_osm_feature(key="leaf_type",
                  value = "mixed") %>%
  osmdata_sf()


coniferousforest <- boundingBox%>%
  opq()%>%
  add_osm_feature(key="leaf_type",
                  value = "coniferous") %>%
  osmdata_sf()


deciduousforest <- boundingBox%>%
  opq()%>%
  add_osm_feature(key="leaf_type",
                  value = "deciduous") %>%
  osmdata_sf()


# plotting forest
ggplot() +
  geom_sf(data = forest$osm_polygons,
          inherit.aes = FALSE,
          color = "darkgreen",
          size = .4,
          alpha = .8) +
  geom_sf(data = mixedforest$osm_polygons,
          inherit.aes = FALSE,
          color = "green",
          size = .4,
          alpha = .8) +
  geom_sf(data = coniferousforest$osm_polygons,
          inherit.aes = FALSE,
          color = "blue",
          size = .4,
          alpha = .8) +
  geom_sf(data = deciduousforest$osm_polygons,
          inherit.aes = FALSE,
          color = "pink",
          size = .4,
          alpha = .8) +
  coord_sf(xlim,
           ylim,
           expand = FALSE)
