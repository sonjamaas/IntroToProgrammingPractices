library(sf)
library(terra)
library(raster)
library(mapview)
library(mapedit)
library(caret)
library(RStoolbox)

set.seed(455351) #why?

r <- brick("data/S2_ALL_10m-20m_T32UPA_20180507_UTM_WGS84_32N_AOI.tif")
bands <- c("B02", "B03", "B04",  "B08", "B05", "B06", "B07", "B08a", "B11", "B12")
names(r) <- bands

file_samples <- "data/samples20221220.gpkg"
if(!file.exists(file_samples)){
  # sampling:
  # forest
  forest <- drawFeatures(
    map = viewRGB(r, r=7, g=3, b=2, maxpixels = 1000000)
  )
  
  # urban
  urban <- drawFeatures(
    map = viewRGB(r, r=10, g=9, b=3, maxpixels = 1000000)
  )
  
  # soil
  soil <- drawFeatures(
    map = viewRGB(r, r=10, g=9, b=3, maxpixels = 1000000)
  )
  
  # agri
  agri <- drawFeatures(
    map = viewRGB(r, r=9, g=7, b=1, maxpixels = 100000000)
  )
  
  # water
  r_ndwi <- (r$B03 - r$B08) / (r$B03 + r$B08)
  samples$water <- drawFeatures(
    map = mapview(r_ndwi, maxpixels = 1000000)
  )
  
  # add landuse attribute
  forest$landuse <- "forest"
  urban$landuse <- "urban"
  soil$landuse <- "soil"
  agri$landuse <- "agri"
  water$landuse <- "water"
  
  # codify landuse as integers using mapvalues
  labeled_poly <- rbind(forest, urban, soil, agri, water)
  labeled_poly$classid <- as.numeric(
    plyr::mapvalues(labeled_poly$landuse, 
                    from = unique(labeled_poly$landuse), 
                    to = 1:length(unique(labeled_poly$landuse)))
  )
  
  # save
  st_write(labeled_poly, "data/samples20221220.gpkg")
}else{
  labeled_poly <- st_read(file_samples)
}

# load polygons
labeled_poly <- st_transform(labeled_poly, st_crs(r))

# what do you want to be your response variable? numeric!
labeled_poly$classid
labeled_poly$resp_var <- labeled_poly$classid

# we need labeled features for training:
# features = predictors (e.g. spectra)
# labels = response variable (e.g. classid, but could be continuous as well, e.g. species richness)

# to get labeled features, we need points to extract features for
# let's randomly select some points in our polygons and save there labels to them
labeled_points <- list()
for(i in unique(labeled_poly$resp_var)){
  message(paste0("Sampling points from polygons with resp_var=", i))
  
  # sample points for polygons of resp_var = i
  labeled_points[[i]] <- st_sample(
    x = labeled_poly[labeled_poly$resp_var == i,], 
    size = 100
  )
  labeled_points[[i]] <- st_as_sf(labeled_points[[i]])
  labeled_points[[i]]$resp_var <- i
}
labeled_points <- do.call(rbind, labeled_points)

# now that we have points we need to extract features
# we can use our RS data for this since we do not have any ground-truth from site
r <- normImage(r)
r <- rescaleImage(r, ymin = 0, ymax = 1)

# extract features and label them with our response variable!
unlabeled_features <- raster::extract(r, labeled_points, df = T)
unlabeled_features <- unlabeled_features[,-1] # no ID column needed
labeled_features <- cbind(
  resp_var = labeled_points$resp_var,
  unlabeled_features
)

# remove duplicates (in case multiple points fall into the same pixel)
dupl <- duplicated(labeled_features)
which(dupl)
length(which(dupl)) # number of duplicates in labeled features that we need to remove!
labeled_features <- labeled_features[!dupl,]

# x = features
x <- labeled_features[,2:ncol(labeled_features)] # remove ID column
y <- as.factor(labeled_features$resp_var) #we want caret to treat this as categories, thus factor
levels(y) <- paste0("class_", levels(y))


# fit the model, here a ranodm Forest
model <- train(
  x = x,
  y = y, 
  trControl = trainControl(
    p = 0.75, # percentage of samples used for training, rest for validation
    method  = "cv", #cross validation
    number  = 5, # 5-fold
    verboseIter = TRUE, # progress update per iteration
    classProbs = TRUE # probabilities for each example
  ),
  #preProcess = c("center", "scale"), #center/scale if not done by you on the raster (see previous code rescl)
  method = "rf" # used algorithm
) 

# performance
model
confusionMatrix(model)

# predict
r_class <- predict(r, model, type='raw')

# write 
writeRaster(r_class, filename = "data/S2_ALL_10m-20m_T32UPA_20180507_UTM_WGS84_32N_AOI_LANDUSE.tif",
            datatype = "INT1U")

cols <- c("darkgreen", "red", "green", "sandybrown", "blue")

# map
mapview(r_class, col.regions = cols)

# plot
ggplot() + ggR(r_class, geom_raster = T, ggLayer = T) +
  scale_fill_manual(values = cols, 
                    name = "Land Use") +
  coord_sf(crs = st_crs(r), datum = st_crs(4326))