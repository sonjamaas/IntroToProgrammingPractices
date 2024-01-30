library(terra)
library(raster)
library(cluster)
library(sf)
library(RStoolbox)
install.packages("rgdal")

#################################
## Unsupervised Classification ##
#################################

# get data
landsat <- lsat
names(landsat)

# calculate ndvi from bands b3 and b4
ndvi <- spectralIndices(landsat,red="B3_dn",nir="B4_dn", indices = "NDVI")

# make the ndvi values into a raster and plot it
ndvi_raster <- raster(ndvi)
plot(ndvi_raster)

# do a unsupervised classification and plot it
uc <- unsuperClass(lsat, nClasses=5)
ggR(uc$map, forceCat=TRUE, geom_raster=TRUE)

###############################
## Supervised Classification ##
###############################

# load the raster data (.tif) and plot it
raster <- rast("C:/Users/sonja/Documents/Dokumente/Studium/Master/Intro_to_programming/outputClassificationData.tif")
map1 <- ggRGB(raster, stretch="lin")+ggtitle("RGB Image")

# load training and validation data
train <- read_sf("C:/Users/sonja/Downloads/data_book/vector_data/training_2011.shp")
vali <- read_sf("C:/Users/sonja/Downloads/data_book/vector_data/validation_2011.shp")

# do the supervised classification and plot it
class <- superClass(raster,train,validation=vali,responseCol='id')

map2 <- ggR(class$map, forceCat=TRUE, geom_raster=TRUE)+ggtitle("Classification")

library(ggpubr)
ggarrange(map1,map2,ncol=2,common.legend = TRUE, legend="right")
