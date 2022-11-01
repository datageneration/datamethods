# Collecting and mapping Census data using API
# install.packages(c("tidyverse", "ggmap","mapproj", "tidycensus","tigris"))
# lapply(c("tidyverse", "ggmap","mapproj", "tidycensus","tigris"), require, character.only = TRUE)

# More on Census data: https://rconsortium.github.io/censusguide/r-packages-all.html
# an API key is required to get Census data for map creation
# Obtain the key at http://api.census.gov/data/key_signup.html
# Enter information about organization and email address, then consent
# Key will be provided to email, click on activate key (wait a few minutes to activate)
# Store the key using the following function:
# census_api_key("key", install = TRUE)
# API key will be stored in  .Renviron and can be accessed by Sys.getenv("CENSUS_API_KEY")

census_api_key("key", install = TRUE)
library(tidycensus)
library(tigris) # Load Census TIGER/Line Shapefiles
options(tigris_use_cache = TRUE)

# Get a list of American Community Survey (ACS) variables
acs19 = tidycensus::load_variables(2019, "acs5", cache = TRUE)
acs19_Profile = load_variables(2019 , "acs5/profile", cache = TRUE)
us_median_age <- get_acs(
  geography = "state",
  variables = "B01002_001",
  year = 2019,
  survey = "acs1",
  geometry = TRUE,
  resolution = "20m"
) %>%
  shift_geometry()


plot(us_median_age$geometry)
ggplot(data = us_median_age, aes(fill = estimate)) + 
  geom_sf(col="white") +  # Why color is white?
  theme_bw() +
  scale_fill_distiller(palette = "PuBuGn",  # Try other palette?
                       direction = 1) + 
  labs(title = "  Median Age by State, 2019",
       caption = "Data source: 2019 1-year ACS, US Census Bureau",
       fill = "ACS estimate", family="Palatino") +
  theme(legend.position=c(.9,.05), legend.direction="horizontal") +
  theme(text = element_text(family = "Palatino"), plot.title = element_text(hjust = 0.5))

# Create another map using 2009 data and do comparison?
