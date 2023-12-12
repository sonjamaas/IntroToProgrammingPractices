#download the dev version of cartography
devtools::install_github("riatelab/cartography")
library(cartography)
library(sf)

# import a vector layer (here a shapefile)
mtq <- st_read("C:/Users/sonja/OneDrive/Dokumente/EAGLE_Msc/Semester1/Intro_to_Programming/GitPractices/IntroToProgrammingPractices/Data/AfricaShp.shp")

# display this POLYGON layer
plot(st_geometry(mtq), col = 1:8)

mtq_pencil <- getPencilLayer(x = mtq, size = 300, buffer=.01)

# display this MULTILINESTRING layer
plot(st_geometry(mtq_pencil), col = 1:8)

# and a add the original borders
plot(st_geometry(mtq), col = NA, add=T)

