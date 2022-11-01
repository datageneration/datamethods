# Collecting and mapping Census data without using Census API
# Prerequisite: 
# 1. Google Maps API
#    Visit https://cloud.google.com/maps-platform/ to get Google Maps API
# 2. run readCensus1.R and create the function readCensus()
# install.packages(c("tidyverse", "ggmap","mapproj", "tidycensus","tigris"))
# lapply(c("tidyverse", "ggmap","mapproj"), require, character.only = TRUE)

library(tidyverse)
library(ggmap)
us=map_data("state")

# Create US map data frame
USmapDF <- data.frame(state.name, stringsAsFactors=F)
USmapDF$state=tolower(USmapDF$state.name)

# Create Black and White US map
library(mapproj)
us=map_data("state")

# Create US map data frame
USmapDF <- data.frame(state.name, stringsAsFactors=F)
USmapDF$state=tolower(USmapDF$state.name)

# Create US map
map.us=ggplot(USmapDF,aes(map_id=state))
map.us=map.us + 
  geom_map(map=us,fill="light yellow", color="black")
map.us=map.us + 
  expand_limits(x=us$long,y=us$lat)
map.us=map.us +
  coord_map() + 
  ggtitle("Basic Map of Continental USA") 
map.us=map.us +
  theme_bw()
map.us=map.us +
  theme(plot.title = element_text(hjust = 0.5)) # Does order matter?
map.us


# Read in 2015 Census population data by states

dfStates <- readCensus()
str(dfStates)

# make sure everything is lowercase
dfStates$state <- tolower(dfStates$stateName)

# Create Color US Population map
map.popColor = ggplot(dfStates, aes(map_id = state))
map.popColor = map.popColor + 
  geom_map(map = us, aes(fill=july15pop))
map.popColor = map.popColor +
  expand_limits(x = us$long, y = us$lat)
map.popColor = map.popColor + 
  coord_map() + 
  ggtitle("2015 State Populations")
map.popColor = map.popColor + 
  theme_bw()
map.popColor = map.popColor + 
  theme(plot.title = element_text(hjust = 0.5)) 
map.popColor

# Add a point by geometric location
map.popColor + geom_point(aes(x = -97, y = 33), col="firebrick")

# Add a point usng geocode and ggmap
# ggmap retrieves raster map tiles from online mapping services like Google Maps
library(ggmap)
# register_google(key = "key") 
# You can store API in .Renviron using GOOGLE_MAPS_API=key  (no quotes)

has_google_key()
latlon <- geocode("dallas, tx")
latlon
map.popColor + geom_point(aes(x = latlon$lon, y = latlon$lat), color="red", size = 2)
map.popColor
latlon2=geocode("new york, ny")
latlon2
map.popColor + geom_point(aes(x = latlon$lon, y = latlon$lat), color="#e87500", size = 2) + geom_point(aes(x = latlon2$lon, y = latlon2$lat), color="red", size = 2)

