# load packages
library(lunar)
library(tibble)
library(digest)
library(jsonlite)
library(tidyverse)
library(DescTools)

# ///////////////////////////////////////////////////////////////////////////////
# ///////////////////////////////////////////////////////////////////////////////
# ///////////////////////////////////////////////////////////////////////////////
# ///////////////////////////////////////////////////////////////////////////////

# FUNCTION: create api call
create_api_call <- function(DARK_SKY_API_KEY, lat, lng, ts) {
  base_call = "https://api.darksky.net/forecast/"
  api_call = paste0(base_call, DARK_SKY_API_KEY, "/", lat, ",", lng, ",", ts)
  return(api_call)
}

# ///////////////////////////////////////////////////////////////////////////////

# FUNCTION: make string of latitude and longitude
create_lat_lng_str <- function(lat,lng,delimiter=","){
  return(paste(lat,lng,sep=delimiter))
}

# ///////////////////////////////////////////////////////////////////////////////

# FUNCTION: classify seasons based on solstice and geo
get_season_from_date <- function(date, hemi="northern"){
  if(hemi == "northern") {
    if(lubridate::month(date) == 12 | lubridate::month(date) == 1 | lubridate::month(date) == 2){
      season <- "winter"
    }
    if(lubridate::month(date) == 3 | lubridate::month(date) == 4 | lubridate::month(date) == 5){
      season <- "spring"
    }
    if(lubridate::month(date) == 6 | lubridate::month(date) == 7 | lubridate::month(date) == 8){
      season <- "summer"
    }
    if(lubridate::month(date) == 9 | lubridate::month(date) == 10 | lubridate::month(date) == 11){
      season <- "autumn"
    }
  } else {
    if(hemi != "northern") {
      if(lubridate::month(date) == 12 | lubridate::month(date) == 1 | lubridate::month(date) == 2){
        season <- "summer"
      }
      if(lubridate::month(date) == 3 | lubridate::month(date) == 4 | lubridate::month(date) == 5){
        season <- "autumn"
      }
      if(lubridate::month(date) == 6 | lubridate::month(date) == 7 | lubridate::month(date) == 8){
        season <- "winter"
      }
      if(lubridate::month(date) == 9 | lubridate::month(date) == 10 | lubridate::month(date) == 11){
        season <- "spring"
      }
    }
  }
  return(season)
}

get_season_from_date_pkg <- function(date){
  season <- lunar::terrestrial.season(as.Date(date))
  return(tolower(season))
}

# ///////////////////////////////////////////////////////////////////////////////

# FUNCTION: classify seasons based on solstice and geo
get_zodiac_sign_from_dob <- function(dob){
  return(tolower(DescTools::Zodiac(dob)))
}

# ///////////////////////////////////////////////////////////////////////////////

# FUNCTION: classify seasons based on solstice and geo
get_lunar_context_from_date <- function(date){
  inp <- tibble::tibble(date)
  lunar_phase_num <- lunar::lunar.phase(date)
  lunar_phase_4cat <- lunar::lunar.phase(date,name=4)
  lunar_phase_8cat <- lunar::lunar.phase(date,name=8)
  lunar_distance_cat <- lunar::lunar.distance(date,name=T)
  lunar_distance_num <- lunar::lunar.distance(date,name=F)
  lunar_illumination <- lunar::lunar.illumination(date)
  return(tibble(lunar_phase_num,
                lunar_phase_4cat,
                lunar_phase_8cat,
                lunar_distance_cat,
                lunar_distance_num,
                lunar_illumination))
}

# ///////////////////////////////////////////////////////////////////////////////

# FUNCTION: get data from api (operates off a table of API calls)
get_weather_context <- function(api_call, hash="md5"){
  
  # create blank dataframes
  daily.results <- data.frame()
  hourly.results <- data.frame()
  minutely.results <- data.frame()
  currently.results <- data.frame()
  alerts.results <- data.frame()
  
  for(i in 1:length(api_call)){
    raw.json <- jsonlite::fromJSON(api_call[i])
    
    # daily
    daily.df <- tibble::as_tibble(raw.json$daily$data) %>%
      mutate(api_call = api_call[i]) %>%
      mutate(api_call_id = digest::digest(api_call, algo=hash)) # save MD5 of call
    daily.results <- bind_rows(daily.results, daily.df)
    
    # hourly
    hourly.df <- tibble::as_tibble(raw.json$hourly$data) %>%
      mutate(api_call = api_call[i]) %>%
      mutate(api_call_id = digest::digest(api_call, algo=hash)) # save MD5 of call
    hourly.results <- bind_rows(hourly.results, hourly.df)
    
    # minutely
    minutely.df <- tibble::as_tibble(raw.json$minutely$data) %>%
      mutate(api_call = api_call[i]) %>%
      mutate(api_call_id = digest::digest(api_call, algo=hash)) # save MD5 of call
    minutely.results <- bind_rows(minutely.results, minutely.df)
    
    # current
    currently.df <- tibble::as_tibble(raw.json$currently$data) %>%
      mutate(api_call = api_call[i]) %>%
      mutate(api_call_id = digest::digest(api_call, algo=hash)) # save MD5 of call
    currently.results <- bind_rows(currently.results, currently.df)
    
    # alerts
    alerts.df <- tibble::as_tibble(raw.json$alerts$data) %>%
      mutate(api_call = api_call[i]) %>%
      mutate(api_call_id = digest::digest(api_call, algo=hash)) # save MD5 of call
    alerts.results <- bind_rows(alerts.results, alerts.df)
    
  }
  return(results=list(daily=daily.results,
                      minutely=minutely.results,
                      hourly=hourly.results,
                      currently=currently.results,
                      alerts=alerts.results))
}

# ///////////////////////////////////////////////////////////////////////////////
# ///////////////////////////////////////////////////////////////////////////////
# ///////////////////////////////////////////////////////////////////////////////
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
  mutate(api_call = create_api_call(DARK_SKY_API_KEY, lat, lng, ts)) %>%
  rowwise() %>%
  mutate(api_call_id = digest::digest(api_call, algo="md5")) %>%
  mutate(lat_lng = create_lat_lng_str(lat,lng)) %>%
  mutate(season = sapply(og_date, get_season_from_date)) %>%
  mutate(season2 = sapply(og_date, get_season_from_date_pkg)) %>%
  mutate(zodiac = sapply(dob, get_zodiac_sign_from_dob))

lunar.df <- map(geo.df$og_date, get_lunar_context_from_date)

lunar.df <- get_lunar_context_from_date(geo.df$og_date)

# ///////////////////////////////////////////////////////////////////////////////

# get daily context data
geo.all <- get_weather_context(geo.df$api_call)

# ///////////////////////////////////////////////////////////////////////////////

final.df <-  geo.df %>%
  inner_join(geo.all$daily)
