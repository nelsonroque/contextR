# load packages
library(keyring)
library(lunar)
library(tibble)
library(digest)
library(jsonlite)
library(tidyverse)
library(DescTools)

# ///////////////////////////////////////////////////////////////////////////////

# set API key
DARK_SKY_API_KEY = "001ef0b8cd273a1551d5abe38a4b26a2"

# ///////////////////////////////////////////////////////////////////////////////

# create demo table of api calls to make
geo.df <- data.frame(neg_affect = c(90,80),
                     dob = c("1991-05-16","1991-04-04"),
                     lat = c(40.7788,39.7788),
                     lng = c(-77.84137,-77.84137),
                     ts = c(1273687200,1273687200)) %>%
  mutate(og_date = lubridate::as_datetime(ts)) %>%
  mutate(lat_lng = create_lat_lng_str(lat,lng),
         zodiac = get_zodiac_sign_from_dob(dob),
         birth_season = get_season_from_date_v(dob),
         current_season = get_season_from_date_v(og_date),
         api_call = create_darksky_api_call_v(DARK_SKY_API_KEY, lat, lng, ts)) %>%
  mutate(api_call_id = hash_api_call_v(api_call, algo="md5")) %>%
  mutate(lunar_result = purrr::pmap(list(dob), get_lunar_context_from_date)) %>%
  unnest(lunar_result)

# UP TO THIS POINT, ALL CONTEXT IS FREE, GOING FORWARD, see:
# -= DARKSKY API PRICING: https://darksky.net/dev
weather.list <- get_weather_context(geo.df$api_call)

weather.daily <- weather.list$daily %>% rename_all(
  funs(
    stringr::str_to_lower(.) %>%
      stringr::str_replace_all(., '\\.', '_')
  ))

og.plus.weather.daily <- weather.daily %>%
    inner_join(geo.df)

  
  
sanitize_cols <- function(df) {
  rdf <- df 
  return(rdf)
}

# -= CENSUS API RESTRICTIONS: https://cran.r-project.org/web/packages/censusapi/vignettes/getting-started.html

# ///////////////////////////////////////////////////////////////////////////////

# wrapper of lat_lng:lunar_result
# ts
# lat
# lng
# dob
# og_date