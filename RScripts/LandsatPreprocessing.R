# LANDSAT PREPROCESSING
#https://www.usgs.gov/landsat-missions/using-usgs-landsat-level-1-data-product
library(terra)
install.packages("mapview")
library(mapview)
source("C:/Users/sonja/Documents/Dokumente/Studium/Master/Intro_to_programming/GitPractice/GitPractices/RScripts/estimateHaze.R")

ls_dir <- paste0(getwd(),  "/Data/LT05_L1TP_194026_20111015_20200820_02_T1/")
#C:/Users/sonja/Documents/Dokumente/Studium/Master/Intro_to_programming/GitPractice/GitPractices
# pre-processing
ls_files <- list.files(path = ls_dir, full.names = T)

# get our meta data
ls_files_meta <- grep(pattern = "MTL.txt", x = ls_files, value = T)

# lets read everything we need from the MTL
meta <- readLines(ls_files_meta)
meta_fields <- c(
  "RADIANCE_MULT_BAND",
  "RADIANCE_ADD_BAND",
  "REFLECTANCE_MULT_BAND",
  "REFLECTANCE_ADD_BAND",
  "SUN_ELEVATION",
  "EARTH_SUN_DISTANCE",
  "K1_CONSTANT_BAND",
  "K2_CONSTANT_BAND"
)

# extract values from MTL
meta_vals <- lapply(meta_fields, function(field){
  meta_lines <- grep(field, meta, value=T)
  
  vals <- lapply(meta_lines, function(line){
    strsplit(line," = ")[[1]][2]
    })
  names(vals) <- sapply(meta_lines, function(line){
    gsub(" ", "", strsplit(line," = ")[[1]][1])
    })
  return(vals)
  })

#make it tidy
meta_vals <- unlist(meta_vals)
meta_arr <- attributes(meta_vals)
meta_vals <- as.numeric(meta_vals)
attributes(meta_vals) <- meta_arr

#get hands on raster
ls_files_bands <- list.files(path=ls_dir,
                             pattern=glob2rx("*_B*.TIF"),
                             full.names=T)

ls_dn <- rast(ls_files_bands)

#view RGB

#convert to top of atmosphere radiance (TOA)
ln_toa_rad <- rast(lapply(1:nlyr(ls_dn), function(band){
  (meta_vals[[paste0("RADIANCE_MULT_BAND_", band)]] * ls_dn[[band]]) +
    meta_vals[[paste0("RADIANCE_ADD_BAND_", band)]]
}))

