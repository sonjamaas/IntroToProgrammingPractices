
library("terra")
library("rnaturalearth")
library("sf")
library("geodata")
library("rnaturalearth")
library("rnaturalearthdata")



ital <- ne_countries(country="Italy", scale= "medium", returnclass="sf")


plot(ital)

clim <- geodata::worldclim_global(var='tmax', res=10, download=T, path='.')

plot(clim)

ital.r <- st_transform(ital,st_crs(clim))
clim_ital_crop <- terra::crop(clim, ital.r)
plot(clim_ital_crop)


clim_ital_mask <- terra::mask(clim_ital_crop, ital.r)
plot(clim_ital_mask)

climItal_vect <- terra::extract(clim_ital_mask, ital, mean)
plot(unlist(climItal_vect[,2:13]))

