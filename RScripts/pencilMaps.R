#download the dev version of cartography
devtools::install_github("riatelab/cartography")
library(cartography)
library(sf)

# import a vector layer (here a shapefile)
mtq <- st_read("C:/Users/sonja/Documents/africa_countries.shp")

# display this POLYGON layer
plot(st_geometry(mtq), col = 1:8)

mtq_pencil <- getPencilLayer(x = mtq, size = 10, buffer=.01)

# display this MULTILINESTRING layer
plot(st_geometry(mtq_pencil), col = 1:8)

# and a add the original borders
plot(st_geometry(mtq), col = NA, add=T)

