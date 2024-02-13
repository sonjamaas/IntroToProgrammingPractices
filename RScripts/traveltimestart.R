## Get into nature the fastest way possible
install.packages("geos")
# load required libraries
library(tidyverse)
library(osmdata)
library(sf)
library(nominatimlite)
library(mapsf)
library(osrm)
library(geos)

#############################
### AOI related functions ###
#############################

# function for the bounding box
spatialExtent <- function(xcoord, ycoord, buffer){
  
  # make bounding box out of coordinates
  coord <- data.frame(x=xcoord, y=ycoord)
  pov <- st_as_sf(coord, coords = c("x","y"), crs = 4326)
  aoibbox <- st_bbox(st_buffer(pov, buffer), crs= 4326)
  aoi <- st_as_sfc(aoibbox)
  aoi <- as.vector(aoi)
  
  return(aoi)
}

# function to set x and y limits
xyLimits <- function(xcoord, ycoord, buffer){
  
  # make bounding box out of coordinates
  coord <- data.frame(x=xcoord, y=ycoord)
  pov <- st_as_sf(coord, coords = c("x","y"), crs = 4326)
  aoibbox <- st_bbox(st_buffer(pov, buffer), crs= 4326)
  
  # set x and y limits
  xlim <- c(aoibbox$xmin, aoibbox$xmax)
  ylim <- c(aoibbox$ymin, aoibbox$ymax)
  
  return(c(xlim,ylim))
}

# function for POV
getPOV <- function(xcoord, ycoord){
  
  # make bounding box out of coordinates
  coord <- data.frame(x=xcoord, y=ycoord)
  pov <- st_as_sf(coord, coords = c("x","y"), crs = 4326)
  return(pov)
}

#####################################
### OSM-feature related functions ###
#####################################

# function to extract streets
extractStreets <- function(aoi){
  streets <- aoi%>%
    opq()%>%
    add_osm_feature(key = "highway", 
                    value = c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", # These are the principal tags for the road network. They range from the most to least important.
                              "motorway_link", "trunk_link", "primary_link", "secondary_link", "tertiary_link", # link roads
                              "living_street", "service", "pedestrian",  "track", "bus_guideway", "escape", "raceway", "road", "busway", # Special road types
                              "footway", "bridleway", "steps", "cooridor", "path", "via_ferrata", # paths
                              "ladder", "mini_roundabout", "motorway_junction", "turning_circle" # other highway features
                              )) %>%
    osmdata_sf()
  
  return(streets)
}

# function to extract rivers and water bodies
extractRiver <- function(aoi){
  river <- aoi%>%
    opq()%>%
    add_osm_feature(key = "waterway", value = "river") %>%
    osmdata_sf()
  
  return(river)
}

# functions to extract forest areas from osm
extractForest <- function(aoi){
  forest <- aoi%>%
    opq()%>%
    add_osm_feature(key="landuse" , 
                    value = "forest" ) %>%
    osmdata_sf()
  
  return(forest)
}

extractSpecialForest <- function(aoi){
  specialForest <- aoi%>%
    opq()%>%
    add_osm_feature(key="leaf_type",
                    value = c("mixed","coniferous","deciduous")) %>%
    osmdata_sf()
  
  return(specialForest)
}

extractWood <- function(aoi){
  wood <- aoi%>%
    opq()%>%
    add_osm_feature(key="natural",
                    value = "wood") %>%
    osmdata_sf()
  
  return(wood)
}

# functions to extract other nature areas from OSM
extractGrassland <- function(aoi){
  grasslandAndBushes <- aoi%>%
    opq()%>%
    add_osm_feature(key="nature",
                    value = c("grassland","scrub","shrubbery")) %>%
    osmdata_sf()
  
  return(grasslandAndBushes)
}

extractSingleTree <- function(aoi){
  singleTree <- aoi%>%
    opq()%>%
    add_osm_feature(key="natural",
                    value = "tree") %>%
    osmdata_sf()
  
  return(singleTree)
}

extractTreeLine <- function(aoi){
  treeLine <- aoi%>%
    opq()%>%
    add_osm_feature(key="natural",
                    value = "tree_row") %>%
    osmdata_sf()
  
  return(treeLine)
}

extractMeadow <- function(aoi){
  meadow <- aoi%>%
    opq()%>%
    add_osm_feature(key="landuse",
                    value = "meadow") %>%
    osmdata_sf()
  
  return(meadow)
}

extractWine <- function(aoi){
  wine <- aoi%>%
    opq()%>%
    add_osm_feature(key="landuse",
                    value = "vineyard") %>%
    osmdata_sf()
  
  return(wine)
}

# functions to extract leisure areas from osm
extractPark <- function(aoi){
  park <- aoi%>%
    opq()%>%
    add_osm_feature(key="leisure",
                    value = "park") %>%
    osmdata_sf()
  
  return(park)
}

extractGarden <- function(aoi){
  garden <- aoi%>%
    opq()%>%
    add_osm_feature(key="leisure",
                    value = "garden") %>%
    osmdata_sf()
  
  return(garden)
}

extractNatureReserve <- function(aoi){
  natureReserve <- aoi%>%
    opq()%>%
    add_osm_feature(key="leisure",
                    value = "nature_reserve") %>%
    osmdata_sf()
  
  return(natureReserve)
}

#########################
### Plotting function ###
#########################

finalPlot <- function(streets, river, forest, 
                      specialForest, wood, singleTree, 
                      treeLine, grasslandAndBushes, 
                      meadow, park, garden, 
                      natureReserve, pov, xlim, ylim){
  plot <-  ggplot() +
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
    geom_sf(data = specialForest$osm_polygons,
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
  
  return(plot)
}

# function to extract all features from osm and plots 
whereRthetrees <- function(x,y,buffer){
  
  # get spatial variables
  aoi <- spatialExtent(x,y,buffer)
  
  xylim <- xyLimits(x,y,buffer)
  xlim <- c(xylim[1],xylim[2])
  ylim <- c(xylim[3],xylim[4])
  
  pov <- getPOV(x,y)
  
  # extract features from OSM
  streets <- extractStreets(aoi)
  river <- extractRiver(aoi)
  forest <- extractForest(aoi)
  specialForest <- extractSpecialForest(aoi)
  wood <- extractWood(aoi)
  grassland <- extractGrassland(aoi)
  garden <- extractGarden(aoi)
  natureReserve <- extractNatureReserve(aoi)
  meadow <- extractMeadow(aoi)
  wine <- extractWine(aoi)
  singleTree <- extractSingleTree(aoi)
  treeLine <- extractTreeLine(aoi)
  park <- extractPark(aoi)
  
  # plot the osm data
  plotFinal <- finalPlot(streets, river, forest, 
                         specialForest, wood, singleTree, 
                         treeLine, grassland, 
                         meadow, park, garden, 
                         natureReserve, pov, xlim, ylim)
  return(plotFinal)
}

#############################
### Testing the functions ###
#############################

whereRthetrees(9.183377, 45.476302, 500)


areaofint <- spatialExtent(12.179503, 53.147339, 500)
pov <- getPOV(12.179503, 53.147339)
dst <- extractForest(areaofint)


# simple travel distance and closest coordinate extraction
distA <- osrmTable(pov,dst$osm_points,
                   measure = c('distance'),
                   osrm.profile = "foot")

# find closest point via travel distance
distAsorted <- as.list(distA$distances[,order(distA$distances[1,])])
nameclosest <- names(distAsorted)[1]

coordClosest <- distA$destinations[c(nameclosest),]


closestForest <- c(distAsorted[1])

############################
### Code as non-function ###
############################

# # extract roads and rivers from osm
# streets <- aoi%>%
#   opq()%>%
#   add_osm_feature(key = "highway",
#                   value = c("motorway", "primary",
#                             "secondary", "tertiary",
#                             "residential", "living_street",
#                             "unclassified",
#                             "service", "footway",
#                             "trunk", "motorway_link",
#                             "trunk_link", "primary_link",
#                             "secondary_link","tertiary_link",
#                             "pedestrian","track",
#                             "road","bridleway",
#                             "steps","path","sidewalk")) %>%
#   osmdata_sf()

# river <- aoi%>%
#   opq()%>%
#   add_osm_feature(key = "waterway", value = "river") %>%
#   osmdata_sf()

# # extracting forest types from osm
# forest <- aoi%>%
#   opq()%>%
#    add_osm_feature(key="landuse" , 
#                    value = "forest" ) %>%
#   osmdata_sf()
# 
# specialforest <- aoi%>%
#   opq()%>%
#   add_osm_feature(key="leaf_type",
#                   value = c("mixed","coniferous","deciduous")) %>%
#   osmdata_sf()
# 
# wood <- aoi%>%
#   opq()%>%
#   add_osm_feature(key="natural",
#                   value = "wood") %>%
#   osmdata_sf()

# # extracting other nature types from osm
# grasslandAndBushes <- aoi%>%
#   opq()%>%
#   add_osm_feature(key="nature",
#                   value = c("grassland","scrub","shrubbery")) %>%
#   osmdata_sf()
# 
# singleTree <- aoi%>%
#   opq()%>%
#   add_osm_feature(key="natural",
#                   value = "tree") %>%
#   osmdata_sf()
# 
# treeLine <- aoi%>%
#   opq()%>%
#   add_osm_feature(key="natural",
#                   value = "tree_row") %>%
#   osmdata_sf()
# 
# meadow <- aoi%>%
#   opq()%>%
#   add_osm_feature(key="landuse",
#                   value = "meadow") %>%
#   osmdata_sf()
# 
# wine <- aoi%>%
#   opq()%>%
#   add_osm_feature(key="landuse",
#                   value = "vineyard") %>%
#   osmdata_sf()
# 
# # extracting leisure areas from osm
# park <- aoi%>%
#   opq()%>%
#   add_osm_feature(key="leisure",
#                   value = "park") %>%
#   osmdata_sf()
# 
# garden <- aoi%>%
#   opq()%>%
#   add_osm_feature(key="leisure",
#                   value = "garden") %>%
#   osmdata_sf()
# 
# natureReserve <- aoi%>%
#   opq()%>%
#   add_osm_feature(key="leisure",
#                   value = "nature_reserve") %>%
#   osmdata_sf()

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
 
 