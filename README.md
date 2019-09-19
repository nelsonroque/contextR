# R package: contextR
Bind weather and lunar data to provide context to any dataset with an identifier column, a date column, GPS coordinates (lat/long).

## Special Thanks
- rOpenSci, creators of R package, rnoaa: https://github.com/ropensci/rnoaa
- Emmanuel Lazaridis (https://github.com/StatLaw) and Gábor Csárdi (https://github.com/gaborcsardi), creators/maintainers of R package, lunar

## Acknowledgements

Development supported by National Institute on Aging Grant T32 AG049676 to The Pennsylvania State University. 

## Roadmap

- read zipcode from dataset and use for search (also add zipcode left-join data to search by state)
- political data by zipcode
- top N news headlines for county/state/date
- social media sentiment for city/state/date

# Installation:
```r
devtools::install_github("nelsonroque/contextR", force=T)

```

# Usage:

```r
# load packages
library(contextR)

# create data structure required for function
test.df <- data.frame(user_id = c("1000","2000","3000"),
                      date = c("2018/2/1","2017/5/16","2018/2/1"),
                      gps_lat = c(40.825651,39.825651,25.790654),
                      gps_long = c(-77.887898,-76.887898,-80.1300455),
                      RT = c(1200,1100,3000),
                      Accuracy = c(.9,.99,.5))

# DATA PRIVACY MANIFESTO

# -- all records originate from consenting users
# -- minimize api calls
# -- respect API
# -- be fuzzy with GPS where possible

# ///////////////////////////////////////////////////////////////////////////////

# set API key
DARK_SKY_API_KEY = "ENTER_YOUR_API_KEY_HERE"
CENSUS_API_KEY = "ENTER_YOUR_API_KEY_HERE"

# ///////////////////////////////////////////////////////////////////////////////

# create / load vectorized versions of functions
create_fcc_api_call_v <- Vectorize(create_fcc_api_call)
create_darksky_api_call_v <- Vectorize(create_darksky_api_call)
create_photon_api_call_v <- Vectorize(create_photon_api_call)
get_lunar_context_from_date_v <- Vectorize(get_lunar_context_from_date)
get_season_from_date_v <- Vectorize(get_season_from_date)
hash_api_call_v <- Vectorize(hash_api_call)

# ///////////////////////////////////////////////////////////////////////////////

# create demo table of minimum necessary features
og.df <- data.frame(survey_result = c(90,80), # example of 'real data'
                     dob = c("1991-05-16","1991-04-04"), # optional
                     lat = c(40.7788,39.7788), # necessary for getting weather (see conversions)
                     lng = c(-77.84137,-77.84137), # necessary for getting weather (see conversions)
                     ts = c(1273687200,1273687200),
                    postal = c(33144,32837)) %>% # necessary for getting weather (see conversions)
  mutate(og_date = lubridate::as_datetime(ts)) # make sure to have ts in as.Date format for other calls

# ///////////////////////////////////////////////////////////////////////////////

# pull 'free' context, and prepare darksky API calls, just in case
context.df <- og.df %>%
  mutate(lat_lng = create_lat_lng_str(lat,lng),
         zodiac = get_zodiac_sign_from_dob(dob),
         birth_season = get_season_from_date_v(dob),
         current_season = get_season_from_date_v(og_date),
         darksky_api_call = create_darksky_api_call_v(DARK_SKY_API_KEY, lat, lng, ts)) %>%
  mutate(darksky_api_call_id = hash_api_call_v(darksky_api_call, algo="md5")) %>%
  mutate(photon_api_call = create_photon_api_call_v(lat, lng)) %>%
  mutate(photon_api_call_id = hash_api_call_v(photon_api_call, algo="md5")) %>%
  mutate(fcc_api_call = create_fcc_api_call_v(lat, lng)) %>%
  mutate(fcc_api_call_id = hash_api_call_v(fcc_api_call, algo="md5")) %>%
  mutate(lunar_result = purrr::pmap(list(dob), get_lunar_context_from_date)) %>%
  unnest(lunar_result) %>%
  mutate(geo_result = purrr::pmap(list(postal), u_zipcode_to_geo)) %>%
  unnest(geo_result)

# need multiple API calls? 
# no problem, just add a separate mutate call
# (dont forget the additional api_call_id)

# ///////////////////////////////////////////////////////////////////////////////

# UP TO THIS POINT, ALL CONTEXT IS FREE and UNRESTRICTED

# ///////////////////////////////////////////////////////////////////////////////

# GOING FORWARD, see:
# -----------------------------------------------------------

# ///////////////////////////////////////////////////////////////////////////////

# -= SEE FCC API RESTRICTIONS: https://geo.fcc.gov/api/census/#!/area/get_area

# get FCC census lookup features
context.fcc.df <- context.df %>%
  mutate(census_result = purrr::pmap(list(fcc_api_call), get_fcc_census_metadata)) %>%
  unnest(census_result)

# ///////////////////////////////////////////////////////////////////////////////

# -= SEE PHOTON API RESTRICTIONS: http://photon.komoot.de/

# get photon reverse geocode features
context.plus.df <- context.fcc.df %>%
  mutate(revgeo_result = purrr::pmap(list(photon_api_call), get_reverse_geo_features)) %>%
  unnest(revgeo_result)

# ///////////////////////////////////////////////////////////////////////////////

# -= DARKSKY API PRICING: https://darksky.net/dev

# get all weather data for each api call (returns list)
weather.list <- get_weather_context(context.df$darksky_api_call)

# stitch daily data with it!
og.plus.weather.daily <- weather.list$daily %>%
    inner_join(context.df)

# ///////////////////////////////////////////////////////////////////////////////

# -= CENSUS API RESTRICTIONS: https://cran.r-project.org/web/packages/censusapi/vignettes/getting-started.html

# ///////////////////////////////////////////////////////////////////////////////

# random utility functions

# ///////////////////////////////////////////////////////////////////////////////

# set home point
home_point <- c(40.7788,-77.84137)

# distance between two GPS coords (havershin distance -- butchered the name)
u_gps_pair_distance(40.7788,-77.84137,39.7788,-77.84137, return_type = "km")

u_gps_distance_origin(home_point, 39.7788,-77.84137, return_type = "km")

# zipcode to GPS coords + city/state
u_zipcode_to_geo(33133)
```