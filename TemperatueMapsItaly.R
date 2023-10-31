library("terra") ## load the required libraries
library("rnaturalearth")
library("sf")
library("geodata")
library("rnaturalearth")
library("rnaturalearthdata")



ital <- ne_countries(country="Italy", scale= "medium", returnclass="sf") ##get the country borders of italy


plot(ital) ##plot the borders

clim <- geodata::worldclim_global(var='tmax', res=10, download=T, path='.') ##get global climate data

plot(clim) ##plot climal data

ital.r <- st_transform(ital,st_crs(clim)) ##Check if the project matches (transform "ital" into vector (sf) file) and reproject it.
clim_ital_crop <- terra::crop(clim, ital.r)
plot(clim_ital_crop) ##plot the new data


clim_ital_mask <- terra::mask(clim_ital_crop, ital.r) ##crop the data to italy and mask it
plot(clim_ital_mask)

climItal_vect <- terra::extract(clim_ital_mask, ital, mean) ##make a plot of the mean temperatures
plot(unlist(climItal_vect[,2:13]))

