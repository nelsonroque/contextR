# load packages
library(tidyverse) # for everything dplyr
library(DescTools) # zodiac dependency
library(lunar) # all lunar data outside of darksky
library(digest) # for hashing API to create unique key for merging
library(jsonlite) # for interpreting JSON
library(geosphere) # for GPS coord distance calcs
library(revgeo)

# ///////////////////////////////////////////////////////////////////////////////

# set API key
DARK_SKY_API_KEY = "001ef0b8cd273a1551d5abe38a4b26a2"

# ///////////////////////////////////////////////////////////////////////////////

# create demo table of minimum necessary features
og.df <- data.frame(survey_result = c(90,80), # example of 'real data'
                     dob = c("1991-05-16","1991-04-04"), # optional
                     lat = c(40.7788,39.7788), # necessary for getting weather (see conversions)
                     lng = c(-77.84137,-77.84137), # necessary for getting weather (see conversions)
                     ts = c(1273687200,1273687200)) %>% # necessary for getting weather (see conversions)
  mutate(og_date = lubridate::as_datetime(ts)) # make sure to have ts in as.Date format for other calls

# ///////////////////////////////////////////////////////////////////////////////

# pull 'free' context, and prepare darksky API calls, just in case
context.df <- og.df %>%
  mutate(lat_lng = create_lat_lng_str(lat,lng),
         zodiac = get_zodiac_sign_from_dob(dob),
         birth_season = get_season_from_date_v(dob),
         current_season = get_season_from_date_v(og_date),
         api_call = create_darksky_api_call_v(DARK_SKY_API_KEY, lat, lng, ts)) %>%
  mutate(api_call_id = hash_api_call_v(api_call, algo="md5")) %>%
  mutate(photon_api_call = create_photon_api_call_v(lat, lng)) %>%
  mutate(photon_api_call_id = hash_api_call_v(photon_api_call, algo="md5")) %>%
  mutate(lunar_result = purrr::pmap(list(dob), get_lunar_context_from_date)) %>%
  unnest(lunar_result)

# need multiple API calls? 
# no problem, just add a separate mutate call
# (dont forget the additional api_call_id)

# ///////////////////////////////////////////////////////////////////////////////

# random utility functions
GPS_pair_distance(40.7788,-77.84137,39.7788,-77.84137)

# ///////////////////////////////////////////////////////////////////////////////

# UP TO THIS POINT, ALL CONTEXT IS FREE

# ///////////////////////////////////////////////////////////////////////////////

# GOING FORWARD, see:

# -= DARKSKY API PRICING: https://darksky.net/dev

# get all weather data for each api call (returns list)
weather.list <- get_weather_context(context.df$api_call)

# stitch daily data with it!
og.plus.weather.daily <- weather.list$daily %>%
    inner_join(context.df)

# -= SEE PHOTON API PRICING: http://photon.komoot.de/

# maybe just do this in the package ... 
# http://photon.komoot.de/reverse?lon=-77.84137&lat=39.7788"

hmm <- revgeo(context.df$lng, context.df$lat, 
              provider =  'photon', 
              output = 'frame') %>% 
  mutate(index = row_number(),
         country = as.character(country)) %>% 
  mutate(location = paste(city, state, sep = ", "))

# -= CENSUS API RESTRICTIONS: https://cran.r-project.org/web/packages/censusapi/vignettes/getting-started.html

# ///////////////////////////////////////////////////////////////////////////////

# wrapper of lat_lng:lunar_result
# ts
# lat
# lng
# dob
# og_date