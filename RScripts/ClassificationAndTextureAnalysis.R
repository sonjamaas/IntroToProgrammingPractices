library(terra)
library(raster)
library(cluster)
library(sf)
library(RStoolbox)
library(ggplot2)
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

# Laptop:
 raster <- rast("C:/Users/sonja/Documents/Dokumente/Studium/Master/Intro_to_programming/outputClassificationData.tif")

# PC:
# raster <- rast("C:/Users/sonja/OneDrive/Dokumente/EAGLE_Msc/Semester1/Intro_to_Programming/GitPractices/IntroToProgrammingPractices/Data/outputClassificationData.tif")

map1 <- ggRGB(raster, stretch="lin")+ggtitle("RGB Image")


# load training and validation data

# Laptop:
 train <- read_sf("C:/Users/sonja/Downloads/data_book/vector_data/training_2011.shp")
 vali <- read_sf("C:/Users/sonja/Downloads/data_book/vector_data/validation_2011.shp")

# PC:
#train <- read_sf("C:/Users/sonja/OneDrive/Dokumente/EAGLE_Msc/Semester1/Intro_to_Programming/GitPractices/IntroToProgrammingPractices/Data/ClassificationExercise/training_2011.shp")
#vali <- read_sf("C:/Users/sonja/OneDrive/Dokumente/EAGLE_Msc/Semester1/Intro_to_Programming/GitPractices/IntroToProgrammingPractices/Data/ClassificationExercise/validation_2011.shp")

# do the supervised classification and plot it
class <- superClass(raster,train,validation=vali,responseCol='id')

map2 <- ggR(class$map, forceCat=TRUE, geom_raster=TRUE)+ggtitle("Classification")

library(ggpubr)
ggarrange(map1,map2,ncol=2,common.legend = TRUE, legend="right")

######################
### Calculate NDVI ###
######################

# load files
b1 <- rast("C:/Users/sonja/Downloads/data_book/raster_data/LT52240632011210/LT52240632011210CUB01_B1.TIF")
b2 <- rast("C:/Users/sonja/Downloads/data_book/raster_data/LT52240632011210/LT52240632011210CUB01_B2.TIF")
b3 <- rast("C:/Users/sonja/Downloads/data_book/raster_data/LT52240632011210/LT52240632011210CUB01_B3.TIF")
b4 <- rast("C:/Users/sonja/Downloads/data_book/raster_data/LT52240632011210/LT52240632011210CUB01_B4.TIF")
b5 <- rast("C:/Users/sonja/Downloads/data_book/raster_data/LT52240632011210/LT52240632011210CUB01_B5.TIF")
b6 <- rast("C:/Users/sonja/Downloads/data_book/raster_data/LT52240632011210/LT52240632011210CUB01_B6.TIF")
b7 <- rast("C:/Users/sonja/Downloads/data_book/raster_data/LT52240632011210/LT52240632011210CUB01_B7.TIF")

rasterList <- list(b1,b2,b3,b4,b5,b6,b7)
rasterStack <- rast(rasterList)

ndvi <- spectralIndices(rasterStack ,red="LT52240632011210CUB01_B3", nir= "LT52240632011210CUB01_B4", indices= "NDVI")

# make the ndvi values into a raster and plot it
ndvi_raster <- raster(ndvi)
plot(ndvi_raster)

########################
### texture analysis ###
########################

install.packages("glcm")
library(glcm)

# moving window 3x3
texture_result <- glcm(ndvi_raster)
plot(texture_result)

# moving window 11x11
texture_result11x11 <- glcm(ndvi_raster,window=c(11,11))
plot(texture_result11x11)

# moving window 17x17
texture_result17x17 <- glcm(ndvi_raster,window=c(17,17))
plot(texture_result17x17)

###########################
### spatial correlation ###
###########################

corr_mean_variance <- corLocal(ndvi_raster, texture_result$glcm_variance, ngb=3)
