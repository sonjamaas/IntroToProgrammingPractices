result1 <-5+6
result1
max()
seq(1,9,by=2)
plot(seq(100))
plot(seq(1,9,by=3))
temp_min <- c(-2,-2,0,3,7,10,12,12,8,5,1,-1)
plot(temp_min)
plot(temp_min,pch=9,cex=2,col='#00ff0060')
lines(lowess(temp_min,f=.1))

install.packages("terra")
install.packages("sf")
install.packages("geodata")
install.packages("rnaturalearth")
install.packages("rmarkdown")
library(terra)
library(rnaturalearth)
library(sf)

install.packages("sf")
install.packages("rnaturalearthdata")

#temperature curve and plot for Germany
ger <- ne_countries(country="Germany",scale="medium",returnclass="sf") #get country borders, other options can be found in the manual

plot(ger) #plot german boundaries

clim <- geodata::worldclim_global(var='tmin',res=10,download=T,path='.') #get temperature minimum data

plot(clim) #plot data

ger.r <- st_transform(ger,st_crs(clim)) #check if the project matches (transform ger into vector (sf) file) and reproject it

clim_ger_crop <- terra::crop (clim,ger.r)
plot(clim_ger_crop)

clim_ger_mask <- terra::mask(clim_ger_crop,ger.r) #mask of germany
plot(clim_ger_mask)

climGer_vect <-  terra::extract(clim_ger_mask,ger,mean)#ectract temp average of germany other statistict possible as well
climGer_vect
plot(unlist(climGer_vect[,2:13]))

#same thing for Italy
ital <- ne_countries(country="Italy",scale="medium",returnclass="sf") #get country borders, other options can be found in the manual
plot(ital) #plot german boundaries
clim <- geodata::worldclim_global(var='tmax',res=10,download=T,path='.') #get temperature maximum data
plot(clim) #plot data
ital.r <- st_transform(ital,st_crs(clim)) #check if the project matches (transform ger into vector (sf) file) and reproject it
clim_ital_crop <- terra::crop (clim,ital.r)
plot(clim_ital_crop)
clim_ital_mask <- terra::mask(clim_ital_crop,ital.r) #mask of italy
plot(clim_ital_mask)
climItal_vect <-  terra::extract(clim_ital_mask,ital,mean)#ectract temp average of germany other statistict possible as well
climItal_vect
plot(unlist(climItal_vect[,2:13]))
