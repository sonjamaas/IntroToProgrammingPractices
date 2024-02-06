# Before we start, we need to load the libraries that we will be using throughout the practical.
install.packages("osmextract")
install.packages("tmap")
library(tidyverse)
library(sf)
library(osmextract)
library(tmap)

# The first step is to define the place. In this case it is Leeds.
# A useful function is oe_match_pattern() to search for patterns in a provider's database. It is helpful in indicating a correct region name for a given provider.
osmextract::oe_match_pattern("leeds") 
osmextract::oe_match_pattern("yorkshire") # "yorkshire" is associated with several different geographic zones.
region_leeds = "Leeds" # Note the capital L. The function in which the object will be used is not case sensitive but I would still recommend using the exact string to minimize the likelihood of an error.

leeds = osmextract::oe_get(
  place = region_leeds,
  provider = "bbbike", # Indicates the provider; default is geofabrik
  layer = "lines", # Default; returns linestring geometries (highways, waterways, aerialways) 
  force_download = TRUE, # Updates the previously downloaded .osm.pbf file (default is FALSE)
  force_vectortranslate = TRUE # Forces the vectorization of a .pbf file to .gpbf even if there is a .gpbf file with the same name (default = FALSE)
)

# downloading a walking network
leeds_net_walking = osmextract::oe_get_network(
  place = region_leeds,
  mode = "walking", # What mode of transport is to be returned? Default is cycling
  provider = "bbbike",
  force_download = TRUE,
  force_vectortranslate = TRUE
)

# checking the keys that are stored in the 'extra_tags' column
osmextract::oe_get_keys(leeds)

# The recommended first step is to create a character vector with all the tags we want to be returned.
tags_needed = c("access",
                "foot",
                "service")

leeds_tn = osmextract::oe_get(
  place = region_leeds,
  provider = "bbbike", 
  layer = "lines", 
  force_download = TRUE, 
  force_vectortranslate = TRUE,
  extra_tags = tags_needed # indicating which additional tags we want to be returned
)

leeds_net_walking_manual = leeds_tn %>% 
  filter(!is.na(highway)) %>% # returning only non-NA highways 
  filter(
    ! highway %in% c(
      'abandonded', 'bus_guideway', 'byway', 'construction', 'corridor', 'elevator',
      'fixme', 'escalator', 'gallop', 'historic', 'no', 'planned', 'platform', 'proposed', 'raceway',
      'motorway', 'motorway_link'
    ) # do not return rows if highway tag values are in this character vector
  ) %>%
  filter(highway != "cycleway" | foot %in% c('yes', 'designated', 'permissive', 'destination')) %>% # do not return rows if highway is equal to 'cycleway' unless foot is equal to 'yes' 
  filter(! access %in% c('private', 'no')) %>% # do not return rows with access value in a character vector
  filter(! foot %in% c('private', 'no', 'use_sidepath', 'restricted')) %>%  # do not return rows with foot value in a character vector
  filter(! grepl('private', service) ) # do not return rows if the service tag has a value containing 'private' 

leeds_net_walking_manual %>% dim()
#> [1] 125581     13
leeds_net_walking %>% dim()
#> [1] 125581     13

tags_needed1 = c("footway")
leeds_net_walking_tn = osmextract::oe_get_network(
  place = region_leeds,
  mode = "walking", # What mode of transport is to be returned? Default is cycling
  provider = "bbbike",
  force_download = TRUE,
  force_vectortranslate = TRUE,
  extra_tags = tags_needed1
)

url5 = "https://github.com/udsleeds/openinfra/releases/download/v0.1/leeds_central_15-02-2022.geojson"
download.file(url5,
              destfile = "leeds_central_15-02-2022.geojson")
leeds_central = sf::read_sf("leeds_central_15-02-2022.geojson") # reading the file

# Let's plot all the highways that have a non-NA value in the `foot` column:
leeds_central %>% select(foot) %>% plot() # all non_NA values 

leeds_central %>% filter(foot == "yes") %>% select(foot) %>% plot() # returning only "yes" values in `foot`

# first, we need to set out tmap mode to interactive
tmap::tmap_mode("view") # all the maps after this will be interactive

leeds_interactive = leeds_central %>% 
  filter(!is.na(foot)) %>% 
  select(foot) %>% 
  tmap::qtm() # add `qtm()` at the end of to visualise your data
leeds_interactive

# if you want to your map to represent different types of foot values, then an additional `lines.col` argument is needed
leeds_interactive_color = leeds_central %>% 
  filter(!is.na(foot)) %>% 
  select(foot) %>% 
  tmap::qtm(lines.col = "foot") 
leeds_interactive_color

# static map
tmap_mode("plot") # all the maps after this will be static
leeds_central %>% 
  filter(!is.na(foot)) %>% 
  select(foot) %>% 
  tmap::qtm(lines.col = "foot")
