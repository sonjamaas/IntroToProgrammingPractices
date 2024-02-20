## Get into nature the fastest way possible

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
spatialExtent <- function(xcoord, ycoord, buffer){                              # spatialExtent becomes a function using x and y coordinates as well as the buffer, 
                                                                                # which is the area in which the nature points will be searched for
  # make bounding box out of coordinates
  coord <- data.frame(x=xcoord, y=ycoord)                                       # coord becomes a dataframe containing the x and y coordinates
  pov <- st_as_sf(coord, coords = c("x","y"), crs = 4326)                       # pov becomes a sf object of coord with a set coordinate system
  aoibbox <- st_bbox(st_buffer(pov, buffer), crs= 4326)                         # aoibbox becomes the bounding box around the pov with the radius of the buffer and a coordinate system
  aoi <- st_as_sfc(aoibbox)                                                     # make aoi a sf object from aoibbox
  aoi <- as.vector(aoi)                                                         # aoi becomes a vector object
  
  return(aoi)                                                                   # function returns the area of interest, aka the bounding box around the pov with the set radius
}

# function to set x and y limits
xyLimits <- function(xcoord, ycoord, buffer){                                   # xyLimits becomes a function using x and y coordinates and the buffer
  
  # make bounding box out of coordinates
  coord <- data.frame(x=xcoord, y=ycoord)                                       # same variables as in spatialExtend
  pov <- st_as_sf(coord, coords = c("x","y"), crs = 4326)                       
  aoibbox <- st_bbox(st_buffer(pov, buffer), crs= 4326)
  
  # set x and y limits
  xlim <- c(aoibbox$xmin, aoibbox$xmax)                                         # x and y limits are calculated from the aoibbox minima and maxima
  ylim <- c(aoibbox$ymin, aoibbox$ymax)
  
  return(c(xlim,ylim))                                                          # function returns a vector of two variables, which contain the x and y limits of the aoi
}

# function for POV
getPOV <- function(xcoord, ycoord){                                             # getPOV is a simple function for just returning the POV as sf object

  coord <- data.frame(x=xcoord, y=ycoord)
  pov <- st_as_sf(coord, coords = c("x","y"), crs = 4326)
  return(pov)
}

#####################################
### OSM-feature related functions ###
#####################################

# function to extract streets
extractStreets <- function(aoi){                                                # function to extract all Streets in the area of interest
  streets <- aoi%>%                                                             # variable streets: for the aoi, 
    opq()%>%                                                                    # build a new overpass query object
    add_osm_feature(key = "highway",                                            # and add multiple osm features with the key "highway" and all possible values/types
                    value = c("motorway", "trunk", "primary", "secondary",      # Principal tags for the road network, range from the most to least important
                              "tertiary", "unclassified", "residential",        
                              "motorway_link", "trunk_link", "primary_link",    # link roads
                              "secondary_link", "tertiary_link",                
                              "living_street", "service", "pedestrian",         # Special road types
                              "track", "bus_guideway", "escape", "raceway", 
                              "road", "busway",                                 
                              "footway", "bridleway", "steps", "cooridor",      # paths
                              "path", "via_ferrata",                            
                              "ladder", "mini_roundabout", "motorway_junction", # other highway features
                              "turning_circle"                                  
                              )) %>%
    osmdata_sf()                                                                # make the osm data into sf objects
  
  return(streets)                                                               # return the streets variable, now containing a sf collection of all streets in the aoi
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
  specialforest <- aoi%>%
    opq()%>%
    add_osm_feature(key="leaf_type",
                    value = c("mixed","coniferous","deciduous")) %>%
    osmdata_sf()
  
  return(specialforest)
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

finalPlot <- function(streets, river, forest,                                   # finalPlot becomes a function using all the osm objects and the spatial variables
                      specialForest, wood, singleTree, 
                      treeLine, grassland, 
                      meadow, park, garden, 
                      natureReserve, pov, xlim, ylim, closestNature, route){
  
  #plot
  plot <- ggplot()                                                              # variable plot becomes an empty ggplot object
  
  if(!is_empty(streets$osm_lines)){                                             # if the streets$osm_lines object is NOT empty
    plot <- plot+geom_sf(data = streets$osm_lines,                              # add a geom_sf to the plot, based on the data in streets$osm_lines
                         aes(color="Streets"),                                  # with the color "Streets" that is defined later in scale_color_manual
                         lwd = 2)                                               # and the linewidth 
  }
  
  if(!is_empty(river$osm_lines)){                                               # repeat for all other osm objects...
    plot <- plot+geom_sf(data = river$osm_lines,
                         aes(color="River"),
                         lwd = 2)
  }
  
  if(!is_empty(forest$osm_polygons)){
    plot <- plot+geom_sf(data = forest$osm_polygons,
                         aes(fill="Forest")
                         )
  }
  
  if(!is_empty(specialForest$osm_polygons)){
    plot <- plot+geom_sf(data = specialForest$osm_polygons,
                         aes(fill="Special Forest")
                         )
  }
  
  if(!is_empty(wood$osm_polygons)){
    plot <- plot+geom_sf(data = wood$osm_polygons,
                         aes(fill="Wood")
                         ) 
  }
  
  if(!is_empty(singleTree$osm_points)){
    plot <- plot+geom_sf(data = singleTree$osm_points,
                         aes(color="Single Tree"),
                         size = 2,
                         key_glyph=draw_key_rect) 
  }
  
  if(!is_empty(treeLine$osm_lines)){
    plot <- plot+geom_sf(data = treeLine$osm_lines,
                         aes(color="Tree Line"),
                         lwd=1.5) 
  }
  
  if(!is_empty(grassland$osm_polygons)){
    plot <- plot+geom_sf(data = grassland$osm_polygons,
                         aes(fill="Grassland")
                         ) 
  }
  
  if(!is_empty(meadow$osm_polygons)){
    plot <- plot+geom_sf(data = meadow$osm_polygons,
                         aes(fill="Meadow")
                         )
  }
  
  if(!is_empty(park$osm_polygons)){
    plot <- plot+geom_sf(data = park$osm_polygons,
                         aes(fill="Park")
                         ) 
  }
  
  if(!is_empty(garden$osm_polygons)){
    plot <- plot+geom_sf(data = garden$osm_polygons,
                         aes(fill="Garden")
                         ) 
  }
  
  if(!is_empty(natureReserve$osm_polygons)){
    plot <- plot+geom_sf(data = natureReserve$osm_polygons,
                         aes(fill="Nature Reserve")
                         ) 
  }
  
  plot <- plot +                                                                # the objects that are never empty are added
    geom_sf(data = route,
            aes(color = "Route"),
            lwd = 2) +
    geom_sf(data = pov,
            aes(color = "You are here"),
            size = 4) +
    geom_sf(data = closestNature,
            aes(color = "Closest Nature"),
            size = 4) +
    coord_sf(xlim,ylim,expand = FALSE)+                                         # coordinate system is added to the plot with the x and y limits
    
    labs(title = "Where R the Trees?",                                          # adding title, subtitle and caption
         subtitle = paste("Nature is only", 
                          closestNature$dist, 
                          "m away from you!", 
                          sep = " "),
         caption = "This map shows the closest public nature area/point reachable by car, therefore there might
         be a gap between your position and the start of the route and points that might be closer if you are by foot. 
         For foot or bike profile please set up a server.
         \nPlot made with: ggplot2, based on data from Open Street Maps.")+                                         
    theme(plot.title = element_text(size=20, face = "bold", hjust = 0.5),       # specifiyng the appearance of  title, subtitle and caption
          plot.subtitle = element_text(size=10, face = "plain", hjust = 0.5),
          plot.caption= element_text(size=8, face = "italic", hjust = 0.5),
          panel.background = element_rect(fill= "ivory"),                       # changing background and grid colour
          panel.grid.major = element_line(color = "ivory2"),
          plot.tag = element_text(size = 8, face = "italic", hjust = 0))+      
    scale_fill_manual(values=c("Forest" = "darkolivegreen",                     # manually setting the legend for polygon objects
                               "Special Forest" = "darkolivegreen4",
                               "Wood" = "darkolivegreen3",
                               "Grassland" = "green", 
                               "Meadow" = "green2", 
                               "Park" = "green3", 
                               "Garden" = "darkseagreen3",
                               "Nature Reserve" = "green4"),
                      guide = guide_legend(title = NULL, order =2))+            # removing legend title and setting its position as second
    scale_color_manual(values = c("Streets" = "snow4",                          # manually setting the legend for the line and point objects
                                  "River" = "deepskyblue3", 
                                  "Single Tree" = "springgreen3",
                                  "Tree Line" = "springgreen3",
                                  "You are here" = "red", 
                                  "Closest Nature" = "orange",
                                  "Route" = "yellow"),
                       guide = guide_legend(title = "Legend", order = 1),       # setting the legend title and its position as first
                       breaks = c("You are here","Closest Nature","Route",      # reordering legend items
                                  "Streets","River","Single Tree","Tree Line"))       
                          
  return(plot)                                                                  # function returns the final plot
}

################################
### Route planning functions ###
################################

# function to find the closest nature point from osm
findNature <- function(streets, river, forest, specialForest, wood, singleTree, # findNature fuction uses the osm features and the pov
                       treeLine, grassland, meadow, park, garden, 
                       natureReserve, pov){
  
  # check if osm_points is empty and if not get distances
  if(nrow(forest$osm_points)==0){                                               # if forest$osm_points is empty, thus has no rows, keep same data structure as if it was not empty:
    
    distClosestForest <- NA                                                     # assign NA to the distClosestForest variable (distance to the closest forest point)
    nameClosestForest <- NA                                                     # assign NA to the nameClosestForest variable (name of the closest forest point)
    coordClosestForest <- as.list(c(lon=NA,lat=NA))                             # assing an empty list to the coordClosestForest variable 
    print("No forest in this area")                                             # give feedback on missing forest
    
  }else{                                                                        # if it is not empty:
    # get the distance tables from osrmTable
    distForest <- osrmTable(pov,forest$osm_points,                              # use osrmTable function to get distance matrix (distances from pov to the osm points)
                            measure = c('distance'),                            # distance as measurement for the closest point, but duration is also possible
                            osrm.profile = "car")                               # the osrm demo server that I am using is only supporting the "car" profile for routing, 
                                                                                # even tho the table function is usable with all profiles... 
    # get closest forest point
    distForestsorted <- as.list(                                                # make a list that contains the distances of the distance matrix sorted ascending
      distForest$distances[,order(distForest$distances[1,])])                   
    distClosestForest <- as.numeric(distForestsorted[1])                        # get the distance to the closest point as numeric value
    nameClosestForest <- names(distForestsorted)[1]                             # get the name of the closest point
    coordClosestForest <- distForest$destinations[c(nameClosestForest),]        # get the coordinates of the closest point
  }
  
  
  if(nrow(specialForest$osm_points)==0){
    nameClosestSpecialForest <- NA
    distClosestSpecialForest <- NA
    coordClosestSpecialForest <- as.list(c(lon=NA,lat=NA))
    print("No forest specified as broad- or needleleafed in this area")
  }else{
    # get the distance tables from osrmTable
    distSpecialForest <- osrmTable(pov,specialForest$osm_points,
                                   measure = c('distance'),
                                   osrm.profile = "car")
    # get closest special forest point
    distSpecialForestsorted <- as.list(distSpecialForest$distances[,order(distSpecialForest$distances[1,])])
    distClosestSpecialForest <- as.numeric(distSpecialForestsorted[1])
    nameClosestSpecialForest <- names(distSpecialForestsorted)[1]
    coordClosestSpecialForest <- distSpecialForest$destinations[c(nameClosestSpecialForest),]
  }
  
  if(nrow(wood$osm_points)==0){
    nameClosestWood <- NA
    distClosestWood <- NA
    coordClosestWood <- as.list(c(lon=NA,lat=NA))
    print("No woods in this area")
  }else{
    # get the distance tables from osrmTable
    distWood <- osrmTable(pov,wood$osm_points,
                          measure = c('distance'),
                          osrm.profile = "car")
    # get closest wood point
    distWoodsorted <- as.list(distWood$distances[,order(distWood$distances[1,])])
    distClosestWood <- as.numeric(distWoodsorted[1])
    nameClosestWood <- names(distWoodsorted)[1]
    coordClosestWood <- distWood$destinations[c(nameClosestWood),]
  }
  
  if(nrow(grassland$osm_points)==0){
    nameClosestGrassland <- NA
    distClosestGrassland <- NA
    coordClosestGrassland <- as.list(c(lon=NA,lat=NA))
    print("No grassland in this area")
  }else{
    # get the distance tables from osrmTable
    distGrassland <- osrmTable(pov,grassland$osm_points,
                               measure = c('distance'),
                               osrm.profile = "car")
    # get closest grassland point
    distGrasslandsorted <- as.list(distGrassland$distances[,order(distGrassland$distances[1,])])
    distClosestGrassland <- as.numeric(distGrasslandsorted[1])
    nameClosestGrassland <- names(distGrasslandsorted)[1]
    coordClosestGrassland <- distGrassland$destinations[c(nameClosestGrassland),]
  }
  
  if(nrow(garden$osm_points)==0){
    nameClosestGarden <- NA
    distClosestGarden <- NA
    coordClosestGarden <- as.list(c(lon=NA,lat=NA))
    print("No public garden in this area")
  }else{
    # get the distance tables from osrmTable
    distGarden <- osrmTable(pov,garden$osm_points,
                            measure = c('distance'),
                            osrm.profile = "car")
    # get closest garden point
    distGardensorted <- as.list(distGarden$distances[,order(distGarden$distances[1,])])
    distClosestGarden <- as.numeric(distGardensorted[1])
    nameClosestGarden <- names(distGardensorted)[1]
    coordClosestGarden <- distGarden$destinations[c(nameClosestGarden),]
  }
  
  if(nrow(natureReserve$osm_points)==0){
    nameClosestNatureReserve <- NA
    distClosestNatureReserve <- NA
    coordClosestNatureReserve <- as.list(c(lon=NA,lat=NA))
    print("No nature reserve in this area")
  }else{
    # get the distance tables from osrmTable
    distNatureReserve <- osrmTable(pov,natureReserve$osm_points,
                                   measure = c('distance'),
                                   osrm.profile = "car")
    # get closest garden point
    distNatureReservesorted <- as.list(distNatureReserve$distances[,order(distNatureReserve$distances[1,])])
    distClosestNatureReserve <- as.numeric(distNatureReservesorted[1])
    nameClosestNatureReserve <- names(distNatureReservesorted)[1]
    coordClosestNatureReserve <- distNatureReserve$destinations[c(nameClosestNatureReserve),]
  }
  
  if(nrow(meadow$osm_points)==0){
    nameClosestMeadow <- NA
    distClosestMeadow <- NA
    coordClosestMeadow <- as.list(c(lon=NA,lat=NA))
    print("No meadow in this area")
  }else{
    # get the distance tables from osrmTable
    distMeadow <- osrmTable(pov,meadow$osm_points,
                            measure = c('distance'),
                            osrm.profile = "car")
    # get closest garden point
    distMeadowsorted <- as.list(distMeadow$distances[,order(distMeadow$distances[1,])])
    distClosestMeadow <- as.numeric(distMeadowsorted[1])
    nameClosestMeadow <- names(distMeadowsorted)[1]
    coordClosestMeadow <- distMeadow$destinations[c(nameClosestMeadow),]
  }
  
  if(nrow(wine$osm_points)==0){
    nameClosestWine <- NA
    distClosestWine <- NA
    coordClosestWine <- as.list(c(lon=NA,lat=NA))
    print("No wine yard in this area")
  }else{
    # get the distance tables from osrmTable
    distWine <- osrmTable(pov,wine$osm_points,
                          measure = c('distance'),
                          osrm.profile = "car")
    # get closest wine yard point
    distWinesorted <- as.list(distWine$distances[,order(distWine$distances[1,])])
    distClosestWine <- as.numeric(distWinesorted[1])
    nameClosestWine <- names(distWinesorted)[1]
    coordClosestWine <- distWine$destinations[c(nameClosestWine),]
  }
  
  if(nrow(singleTree$osm_points)==0){
    nameClosestSingleTree <- NA
    distClosestSingleTree <- NA
    coordClosestSingleTree <- as.list(c(lon=NA,lat=NA))
    print("No single trees in this area")
  }else{
    # get the distance tables from osrmTable
    distSingleTree <- osrmTable(pov,singleTree$osm_points,
                                measure = c('distance'),
                                osrm.profile = "car")
    # get closest single tree point
    distSingleTreesorted <- as.list(distSingleTree$distances[,order(distSingleTree$distances[1,])])
    distClosestSingleTree <- as.numeric(distSingleTreesorted[1])
    nameClosestSingleTree <- names(distSingleTreesorted)[1]
    coordClosestSingleTree <- distSingleTree$destinations[c(nameClosestSingleTree),]
  }
  
  if(nrow(treeLine$osm_points)==0){
    nameClosestTreeLine <- NA
    distClosestTreeLine <- NA
    coordClosestTreeLine <- as.list(c(lon=NA,lat=NA))
    print("No tree lines in this area")
  }else{
    # get the distance tables from osrmTable
    distTreeLine <- osrmTable(pov,treeLine$osm_points,
                              measure = c('distance'),
                              osrm.profile = "car")
    # get closest tree line point
    distTreeLinesorted <- as.list(distTreeLine$distances[,order(distTreeLine$distances[1,])])
    distClosestTreeLine <- as.numeric(distTreeLinesorted[1])
    nameClosestTreeLine <- names(distTreeLinesorted)[1]
    coordClosestTreeLine <- distTreeLine$destinations[c(nameClosestTreeLine),]
  }
  
  if(nrow(park$osm_points)==0){
    nameClosestPark <- NA
    distClosestPark <- NA
    coordClosestPark <- as.list(c(lon=NA,lat=NA))
    print("No parks in this area")
  }else{
    # get the distance tables from osrmTable
    distPark <- osrmTable(pov,park$osm_points,
                          measure = c('distance'),
                          osrm.profile = "car")
    # get closest park point
    distParksorted <- as.list(distPark$distances[,order(distPark$distances[1,])])
    distClosestPark <- as.numeric(distParksorted[1])
    nameClosestPark <- names(distParksorted)[1]
    coordClosestPark <- distPark$destinations[c(nameClosestPark),]
  }
  
  # compare all closest class points by putting them into one data frame
  allPoints <- data.frame(Name = c(nameClosestForest,nameClosestSpecialForest,  # allPoints becomes a data frame with the first column containing the names of closest points
                                   nameClosestWood,nameClosestGrassland,        # of each osm feature group
                                   nameClosestGarden,nameClosestNatureReserve,
                                   nameClosestMeadow,nameClosestWine,
                                   nameClosestSingleTree,nameClosestTreeLine,
                                   nameClosestPark),
                          lon = c(coordClosestForest$lon,                       # the second column containing the longitude of the points
                                  coordClosestSpecialForest$lon,
                                  coordClosestWood$lon,
                                  coordClosestGrassland$lon,
                                  coordClosestGarden$lon,
                                  coordClosestNatureReserve$lon,
                                  coordClosestMeadow$lon,
                                  coordClosestWine$lon,
                                  coordClosestSingleTree$lon,
                                  coordClosestTreeLine$lon,
                                  coordClosestPark$lon),
                          lat = c(coordClosestForest$lat,                       # the third column containing the latitude of the points
                                  coordClosestSpecialForest$lat,
                                  coordClosestWood$lat,
                                  coordClosestGrassland$lat,
                                  coordClosestGarden$lat,
                                  coordClosestNatureReserve$lat,
                                  coordClosestMeadow$lat,
                                  coordClosestWine$lat,
                                  coordClosestSingleTree$lat,
                                  coordClosestTreeLine$lat,
                                  coordClosestPark$lat),
                          dist = c(distClosestForest,                           # the fourth column containing the distance of the points
                                   distClosestSpecialForest,distClosestWood,
                                   distClosestGrassland,distClosestGarden,
                                   distClosestNatureReserve,distClosestMeadow,
                                   distClosestWine,distClosestSingleTree,
                                   distClosestTreeLine,distClosestPark))
  
  # filter the smallest value of the distance column, ignore the NA values
  closestNature <- subset(allPoints,dist== min(allPoints$dist, na.rm=TRUE))     # subsetting the allPoints data frame for the column with the smallest distance, while ignoring the columns with NA values
  
  # make the subset of the dataframe into a sf object
  closestNature <- st_as_sf(closestNature,coords=c("lon","lat"),crs=4326)       # subset becomes a sf object with a set coordinate system
  
  return(closestNature)                                                         # function returns the sf object of the closest nature point
}

# function to extract all features from osm, calculate the closest nature point and plot
whereRthetrees <- function(x,y,buffer){                                         # whereRtheTrees function uses the x and y coordinates as well as the buffer area
  
  # get spatial variables
  aoi <- spatialExtent(x,y,buffer)                                              # get the aoi
  
  xylim <- xyLimits(x,y,buffer)                                                 # get the x and y limits
  xlim <- c(xylim[1],xylim[2])
  ylim <- c(xylim[3],xylim[4])
  
  pov <- getPOV(x,y)                                                            # get the pov
  
  # extract features from OSM
  streets <- extractStreets(aoi)                                                # extract the osm features for the specific aoi
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
  
  closestNature <- findNature(streets, river, forest, specialForest, wood,      # find the closest nature point
                              singleTree, treeLine, grassland, meadow, park, 
                              garden, natureReserve, pov)
  
  # calculate shortest route to nature point
  route <- osrmRoute(pov,closestNature, overview = "simplified")                # calculate the shortest route
  
  # plot the osm data
  plotFinal <- finalPlot(streets, river, forest,                                # get the final plot
                         specialForest, wood, singleTree, 
                         treeLine, grassland, 
                         meadow, park, garden, 
                         natureReserve, pov, xlim, ylim,closestNature, route) 
  
  return(plotFinal)                                                             # function returns the final plot
}

#############################
### Testing the functions ###
#############################

whereRthetrees(8.671087, 52.113866, 200)
