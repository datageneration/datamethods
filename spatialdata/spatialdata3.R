# Collecting and mapping Census data using API: State data and maps
# install.packages(c("tidyverse", "ggmap","mapproj", "tidycensus","tigris", "tmap", "mapview"))
# lapply(c("tidyverse", "ggmap","mapproj", "tidycensus","tigris", "tmap", "mapview"), require, character.only = TRUE)
library(tidycensus)
options(tigris_use_cache = TRUE)


tx_income <- get_acs(
  geography = "tract", 
  variables = "B19013_001",
  state = "TX", 
  year = 2020,
  geometry = TRUE
)
tx_income
plot(tx_income["estimate"])


library(tmap)
tmap_mode("view")

dallas_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  year = 2020,
  state = "TX",
  county = "Dallas",
  geometry = TRUE
)

tm_shape(dallas_income) + 
  tm_fill(col = "estimate", palette = "YlOrRd",
          alpha = 0.5)

library(mapview)
mapview(dallas_income, zcol = "estimate")
