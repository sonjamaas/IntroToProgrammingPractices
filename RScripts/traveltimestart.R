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

finalPlot <- function(streets, river, forest, 
                      specialForest, wood, singleTree, 
                      treeLine, grassland, 
                      meadow, park, garden, 
                      natureReserve, pov, xlim, ylim, closestNature, route){
  
  #plot
  plot <- ggplot()
  
  if(!is_empty(streets$osm_lines)){
    plot <- plot+geom_sf(data = streets$osm_lines,
                         aes(color="Streets"),
                         #inherit.aes = FALSE,
                         lwd = .8)
  }
  
  if(!is_empty(river$osm_lines)){
    plot <- plot+geom_sf(data = river$osm_lines,
                         aes(color="River"),
                         #inherit.aes = FALSE,
                         lwd = 2)
  }
  
  if(!is_empty(forest$osm_polygons)){
    plot <- plot+geom_sf(data = forest$osm_polygons,
                         aes(fill="Forest")
                         #inherit.aes = FALSE
                         )
  }
  
  if(!is_empty(specialForest$osm_polygons)){
    plot <- plot+geom_sf(data = specialForest$osm_polygons,
                         aes(fill="Special Forest")
                         #inherit.aes = FALSE
                         )
  }
  
  if(!is_empty(wood$osm_polygons)){
    plot <- plot+geom_sf(data = wood$osm_polygons,
                         aes(fill="Wood")
                         #inherit.aes = FALSE
                         ) 
  }
  
  if(!is_empty(singleTree$osm_points)){
    plot <- plot+geom_sf(data = singleTree$osm_points,
                         aes(color="Single Tree"),
                         #inherit.aes = FALSE,
                         size = 1) 
  }
  
  if(!is_empty(treeLine$osm_lines)){
    plot <- plot+geom_sf(data = treeLine$osm_lines,
                         aes(color="Tree Line"),
                         #inherit.aes = FALSE,
                         lwd=1.5) 
  }
  
  if(!is_empty(grassland$osm_polygons)){
    plot <- plot+geom_sf(data = grassland$osm_polygons,
                         aes(fill="Grassland")
                         #inherit.aes = FALSE
                         ) 
  }
  
  if(!is_empty(meadow$osm_polygons)){
    plot <- plot+geom_sf(data = meadow$osm_polygons,
                         aes(fill="Meadow")
                         #inherit.aes = FALSE
                         )
  }
  
  if(!is_empty(park$osm_polygons)){
    plot <- plot+geom_sf(data = park$osm_polygons,
                         aes(fill="Park")
                         #inherit.aes = FALSE
                         ) 
  }
  
  if(!is_empty(garden$osm_polygons)){
    plot <- plot+geom_sf(data = garden$osm_polygons,
                         aes(fill="Garden")
                         #inherit.aes = FALSE
                         ) 
  }
  
  if(!is_empty(natureReserve$osm_polygons)){
    plot <- plot+geom_sf(data = natureReserve$osm_polygons,
                         aes(fill="Nature Reserve")
                         #inherit.aes = FALSE
                         ) 
  }
  
  plot <- plot + 
    geom_sf(data = route,
            aes(color = "Route"),
            #inherit.aes = FALSE,
            lwd = 2) +
    geom_sf(data = pov,
            aes(color = "Point of View"),
            #inherit.aes = FALSE,
            size = 4) +
    geom_sf(data = closestNature,
            aes(color = "Closest Nature"),
            #inherit.aes = FALSE,
            size = 4) +
    coord_sf(xlim,ylim,expand = FALSE)+
    labs(title = "Where R the Trees?")+
    theme(plot.title = element_text(size=20, face = "bold", hjust = 0.5))+
    scale_color_manual(values = c("Streets" = "snow4",
                                  "River" = "deepskyblue3", 
                                  "Single Tree" = "springgreen3",
                                  "Tree Line" = "springgreen3",
                                  "Point of View" = "red", 
                                  "Closest Nature" = "orange",
                                  "Route" = "yellow"),
                       guide = guide_legend(title = "Legend"))+
    # guides(color = guide_legend( title = "Legend",
    #                              override.aes = list(
    #                                shape = c(16, 16, NA, NA), # Use NA to remove the point symbols
    #                                #linetype = c(0, 0, 1, 1), # Use solid lines
    #                                size = c(3, 3, 3, 3) # Adjust size if needed
    #                                )))+
    scale_fill_manual(values=c("Forest" = "darkolivegreen", 
                               "Special Forest" = "darkolivegreen4",
                               "Wood" = "darkolivegreen3",
                               "Grassland" = "green", 
                               "Meadow" = "green2", 
                               "Park" = "green3", 
                               "Garden" = "darkseagreen3",
                               "Nature Reserve" = "green4"),
                      guide = guide_legend(title = NULL))
    
  return(plot)
}

# function to find the closest nature point from osm
findNature <- function(streets, river, forest, specialForest, wood, singleTree, treeLine, 
                       grassland, meadow, park, garden, natureReserve, pov){
  
  # check if osm_points is empty and if not get distances
  if(nrow(forest$osm_points)==0){
    distClosestForest <- NA
    nameClosestForest <- NA
    coordClosestForest <- as.list(c(lon=NA,lat=NA))
    print("No forest around")
  }else{
    # get the distance tables from osrmTable
    distForest <- osrmTable(pov,forest$osm_points,
                            measure = c('distance'),
                            osrm.profile = "foot")
    # get closest forest point
    distForestsorted <- as.list(distForest$distances[,order(distForest$distances[1,])])
    distClosestForest <- as.numeric(distForestsorted[1])
    nameClosestForest <- names(distForestsorted)[1]
    coordClosestForest <- distForest$destinations[c(nameClosestForest),]
  }
  
  
  if(nrow(specialForest$osm_points)==0){
    nameClosestSpecialForest <- NA
    distClosestSpecialForest <- NA
    coordClosestSpecialForest <- as.list(c(lon=NA,lat=NA))
    print("No special forest around")
  }else{
    # get the distance tables from osrmTable
    distSpecialForest <- osrmTable(pov,specialForest$osm_points,
                                   measure = c('distance'),
                                   osrm.profile = "foot")
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
    print("No wood around")
  }else{
    # get the distance tables from osrmTable
    distWood <- osrmTable(pov,wood$osm_points,
                          measure = c('distance'),
                          osrm.profile = "foot")
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
    print("No grassland around")
  }else{
    # get the distance tables from osrmTable
    distGrassland <- osrmTable(pov,grassland$osm_points,
                               measure = c('distance'),
                               osrm.profile = "foot")
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
    print("No garden around")
  }else{
    # get the distance tables from osrmTable
    distGarden <- osrmTable(pov,garden$osm_points,
                            measure = c('distance'),
                            osrm.profile = "foot")
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
    print("No nature reserve around")
  }else{
    # get the distance tables from osrmTable
    distNatureReserve <- osrmTable(pov,natureReserve$osm_points,
                                   measure = c('distance'),
                                   osrm.profile = "foot")
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
    print("No meadow around")
  }else{
    # get the distance tables from osrmTable
    distMeadow <- osrmTable(pov,meadow$osm_points,
                            measure = c('distance'),
                            osrm.profile = "foot")
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
    print("No wine yard around")
  }else{
    # get the distance tables from osrmTable
    distWine <- osrmTable(pov,wine$osm_points,
                          measure = c('distance'),
                          osrm.profile = "foot")
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
    print("No single trees around")
  }else{
    # get the distance tables from osrmTable
    distSingleTree <- osrmTable(pov,singleTree$osm_points,
                                measure = c('distance'),
                                osrm.profile = "foot")
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
    print("No tree lines around")
  }else{
    # get the distance tables from osrmTable
    distTreeLine <- osrmTable(pov,treeLine$osm_points,
                              measure = c('distance'),
                              osrm.profile = "foot")
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
    print("No parks around")
  }else{
    # get the distance tables from osrmTable
    distPark <- osrmTable(pov,park$osm_points,
                          measure = c('distance'),
                          osrm.profile = "foot")
    # get closest park point
    distParksorted <- as.list(distPark$distances[,order(distPark$distances[1,])])
    distClosestPark <- as.numeric(distParksorted[1])
    nameClosestPark <- names(distParksorted)[1]
    coordClosestPark <- distPark$destinations[c(nameClosestPark),]
  }
  
  # compare all closest class points by putting them into one data frame
  allPoints <- data.frame(Name = c(nameClosestForest,nameClosestSpecialForest,nameClosestWood,
                                   nameClosestGrassland,nameClosestGarden,nameClosestNatureReserve,
                                   nameClosestMeadow,nameClosestWine,nameClosestSingleTree,nameClosestTreeLine,
                                   nameClosestPark),
                          lon = c(coordClosestForest$lon,coordClosestSpecialForest$lon,coordClosestWood$lon,
                                  coordClosestGrassland$lon,coordClosestGarden$lon,coordClosestNatureReserve$lon,
                                  coordClosestMeadow$lon,coordClosestWine$lon,coordClosestSingleTree$lon,coordClosestTreeLine$lon,
                                  coordClosestPark$lon),
                          lat = c(coordClosestForest$lat,coordClosestSpecialForest$lat,coordClosestWood$lat,
                                  coordClosestGrassland$lat,coordClosestGarden$lat,coordClosestNatureReserve$lat,
                                  coordClosestMeadow$lat,coordClosestWine$lat,coordClosestSingleTree$lat,coordClosestTreeLine$lat,
                                  coordClosestPark$lat),
                          dist = c(distClosestForest,distClosestSpecialForest,distClosestWood,
                                   distClosestGrassland,distClosestGarden,distClosestNatureReserve,
                                   distClosestMeadow,distClosestWine,distClosestSingleTree,distClosestTreeLine,
                                   distClosestPark))
  
  # filter the smallest value of the distance column, ignore the NA values
  closestNature <- subset(allPoints,dist== min(allPoints$dist, na.rm=TRUE))
  
  # make the subset of the dataframe into a sf object
  closestNature <- st_as_sf(closestNature,coords=c("lon","lat"),crs=4326)
  
  return(closestNature)
}

# function to extract all features from osm, calculate the closest nature point and plot
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
  
  closestNature <- findNature(streets, river, forest, specialForest, wood, singleTree, treeLine, 
                         grassland, meadow, park, garden, natureReserve, pov)
  
  # calculate shortest route to nature point
  route <- osrmRoute(pov,closestNature, overview = "simplified")
  
  # plot the osm data
  plotFinal <- finalPlot(streets, river, forest, 
                         specialForest, wood, singleTree, 
                         treeLine, grassland, 
                         meadow, park, garden, 
                         natureReserve, pov, xlim, ylim,closestNature, route)
  return(plotFinal)
}

#############################
### Testing the functions ###
#############################

whereRthetrees(9.943326, 49.802764, 200)
