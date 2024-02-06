## Get into nature the fastest way possible
library(tidyverse)
library(osmdata)
library(sf)
library(nominatimlite)
library(tidyverse)
library(mapsf)
library(osrm)
library(geos)

# get bounding box coordinates
# boundingBox <- getbb("Grombühl Würzburg Germany")
# xlim <- c(boundingBox[1,1],boundingBox[1,2])
# ylim <- c(boundingBox[2,1],boundingBox[2,2])

# make these coordinates to a polygon for map layout
# aoi <- bbox_to_poly(boundingBox)

# extract aoi boundary from osm
#boundary <- aoi%>%
#  opq()%>%
#  add_osm_feature(key="boundary" , 
#                  value = "administrative" ) %>%
#  osmdata_sf()

# set x and y coordinates
y <- 53.613834
x <- 10.113107

# make bounding box out of coordinates
coord <- data.frame(x=x, y=y)
pov <- st_as_sf(coord, coords = c("x","y"), crs = 4326)
aoibbox <- st_bbox(st_buffer(pov, 500), crs= 4326)
aoi <- st_as_sfc(aoibbox)
aoi <- as.vector(aoi)

#set x and y limits
xlim <- c(aoibbox$xmin, aoibbox$xmax)
ylim <- c(aoibbox$ymin, aoibbox$ymax)

# extract roads and rivers from osm
streets <- aoi%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary",
                            "residential", "living_street",
                            "unclassified",
                            "service", "footway",
                            "trunk", "motorway_link",
                            "trunk_link", "primary_link",
                            "secondary_link","tertiary_link",
                            "pedestrian","track",
                            "road","bridleway",
                            "steps","path","sidewalk")) %>%
  osmdata_sf()

river <- aoi%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

# extracting forest types from osm
forest <- aoi%>%
  opq()%>%
   add_osm_feature(key="landuse" , 
                   value = "forest" ) %>%
  osmdata_sf()

specialforest <- aoi%>%
  opq()%>%
  add_osm_feature(key="leaf_type",
                  value = c("mixed","coniferous","deciduous")) %>%
  osmdata_sf()

wood <- aoi%>%
  opq()%>%
  add_osm_feature(key="natural",
                  value = "wood") %>%
  osmdata_sf()

# extracting other nature types from osm
grasslandAndBushes <- aoi%>%
  opq()%>%
  add_osm_feature(key="nature",
                  value = c("grassland","scrub","shrubbery")) %>%
  osmdata_sf()

singleTree <- aoi%>%
  opq()%>%
  add_osm_feature(key="natural",
                  value = "tree") %>%
  osmdata_sf()

treeLine <- aoi%>%
  opq()%>%
  add_osm_feature(key="natural",
                  value = "tree_row") %>%
  osmdata_sf()

meadow <- aoi%>%
  opq()%>%
  add_osm_feature(key="landuse",
                  value = "meadow") %>%
  osmdata_sf()

wine <- aoi%>%
  opq()%>%
  add_osm_feature(key="landuse",
                  value = "vineyard") %>%
  osmdata_sf()

# extracting leisure areas from osm
park <- aoi%>%
  opq()%>%
  add_osm_feature(key="leisure",
                  value = "park") %>%
  osmdata_sf()

garden <- aoi%>%
  opq()%>%
  add_osm_feature(key="leisure",
                  value = "garden") %>%
  osmdata_sf()

natureReserve <- aoi%>%
  opq()%>%
  add_osm_feature(key="leisure",
                  value = "nature_reserve") %>%
  osmdata_sf()

colorGreen <- rgb(0,0,1)

# plotting
ggplot() +
  # Streets and Rivers
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "snow4",
          lwd = .8) +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "deepskyblue3",
          size = 4) +
  # Forest areas
  geom_sf(data = forest$osm_polygons,
          inherit.aes = FALSE,
          fill = "chartreuse4",
          size = 2) +
  geom_sf(data = specialforest$osm_polygons,
          inherit.aes = FALSE,
          fill = "chartreuse4",
          size = 2) +
  geom_sf(data = wood$osm_polygons,
          inherit.aes = FALSE,
          fill = "chartreuse4",
          size = 1) +
  # Other tree areas
  geom_sf(data = singleTree$osm_points,
          inherit.aes = FALSE,
          color = "chartreuse3") +
  geom_sf(data = treeLine$osm_lines,
          inherit.aes = FALSE,
          color = "chartreuse3",
          size = 1) +
  # Grasslands, Meadows, Parks, Gardens
  geom_sf(data = grasslandAndBushes$osm_lines,
          inherit.aes = FALSE,
          color = "olivedrab2",
          size = 1) +
  geom_sf(data = meadow$osm_polygons,
          inherit.aes = FALSE,
          fill = "olivedrab2",
          size = 1) +
  geom_sf(data = park$osm_polygons,
          inherit.aes = FALSE,
          fill = "olivedrab2",
          size = 1) +
  geom_sf(data = garden$osm_polygons,
          inherit.aes = FALSE,
          fill = "olivedrab1",
          size = 1) +
  geom_sf(data = natureReserve$osm_polygons,
          inherit.aes = FALSE,
          fill = "olivedrab3",
          size = 1) +
  #geom_sf(data = wine$osm_polygons,
  #        inherit.aes = FALSE,
  #        fill = "palevioletred2",
  #        size = 1,
  #        alha=.8) +
  # POV
  geom_sf(data = pov,
          inherit.aes = FALSE,
          color = "red",
          size = 4) +
  coord_sf(xlim,
           ylim,
           expand = FALSE)
 
 