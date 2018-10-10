#' A Function to Provide Environmental Context to Data
#' @name weather2df
#' @param df class: data.frame description: must have columns labelled identically to: gps_lat, gps_long, user_id, date 
#' @param date_c class: string; variable name for column with date data
#' @param date_f class: string; date format
#' @param lat class: string; variable name for column with GPS Latitude data
#' @param long class: string; variable name for column with GPS Longitude data
#' @param radius class: numeric; how far out (in km) to look from GPS coords origin
#' @param clean class: boolean; T = returns entire day of data for record, removing observations where quality is marked as inappropriate (see NOAA documentation)
#' @import tidyverse rnoaa lubridate
#' @examples
#' weather2df(df,date_c="date", date_f="%Y/%m/%d", id='user_id', lat='gps_lat', long='gps_long', radius=5, clean=F)

#' @export
# function to search and bind data with df given (will optimize to search by zip instead of record)
weather2df <- function(df,id='user_id',date_c='date',date_f='%Y/%m/%d',lat='gps_lat',long='gps_long',radius=5,clean=F) {
  
  # search parameters #### 
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # weather station search parameters
  SEARCH_MAX = 10 # in km
  
  # data pre-procesing #### 
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # create structured date column
  if(date_f == "") {
    df.pp = df %>% 
      #mutate_at(vars(matches(date_c)), .funs = funs(DATE = as.Date(., date_f))) %>%
      mutate_at(vars(date_c), .funs = funs(YEAR = year(DATE))) %>%
      mutate_at(vars(date_c), .funs = funs(DATE_SEARCH = as.character(as.Date(DATE,"%Y%m%d"))))
  } else {
    df.pp = df %>% mutate_at(vars(matches(date_c)), .funs = funs(DATE = as.Date(., date_f))) %>%
      mutate_at(vars(date_c), .funs = funs(YEAR = year(DATE))) %>%
      mutate_at(vars(date_c), .funs = funs(DATE_SEARCH = as.character(as.Date(DATE,"%Y%m%d"))))
  }

  # bind data #### 
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  weather.df <- data.frame()
  for(record.id in 1:nrow(df.pp)){
    # isolate record
    cur.record <- df.pp[record.id,]
    cur.id <- cur.record %>% select(id)
    cur.date <- cur.record %>% select(DATE_SEARCH)
    cur.date.s <- gsub("-","",as.character(cur.date))
    cur.year <- cur.record %>% select(YEAR) %>% as.numeric
    cur.gps.lat <- cur.record %>% select(lat) %>% as.numeric
    cur.gps.long <- cur.record %>% select(long) %>% as.numeric
    
    print(paste0("PROCESSING: ",cur.id," | ",cur.date.s))
    
    # start search 5km from GPS coords
    SEARCH_RADIUS <- radius
    
    # create container for weather station results for this record
    wsr <- data.frame()
    
    # start searching weather stations (until max searches)
    search_counter = 0
    while(nrow(wsr) <= 0 & search_counter < SEARCH_MAX) {
      # begin station search
      wsr <- isd_stations_search(lat = cur.gps.lat,
                                 lon = cur.gps.long,
                                 radius = SEARCH_RADIUS) %>% filter(wban != "99999" & usaf != "999999")
      SEARCH_RADIUS <- SEARCH_RADIUS + round((radius/2),0)
      search_counter <- search_counter + 1
    }
    
    if(nrow(wsr) < 1) {
      results.day <- data.frame(RESULT = paste0("no results after ", SEARCH_MAX, 'searches'))
      results.summary <- data.frame(RESULT = paste0("no results after ", SEARCH_MAX, 'searches'))
      warning(paste0("no results for current record| ",cur.id," | ",cur.date.s))
    } else {
      # get weather data for closest station at given year; using: wsr$usaf[1]
      results.df <- isd(usaf = wsr$usaf[1],
                          wban = wsr$wban[1],
                          year = cur.year)
      
      # merge results with weather station info
      results.df <- merge(results.df, wsr, by.x=c("usaf_station","wban_station"), by.y=c("usaf","wban"))
      
      # isolate set of columns of interest              
      results.slim <- results.df %>% select(date, time,
                                            station_name,ctry,state,elev_m, begin, end, distance,
                                            latitude.x, longitude.x, usaf_station, wban_station,
                                            air_pressure, air_pressure_quality,
                                            elevation, ceiling_height, ceiling_height_quality,
                                            wind_code, wind_speed, wind_speed_quality, wind_direction, wind_direction_quality,
                                            visibility_code, visibility_distance, visibility_distance_quality,
                                            temperature, temperature_quality, temperature_dewpoint, temperature_dewpoint_quality)#,
                                            #KC1_code, KC1_condition_code, KC1_extreme_temp_month, KC1_temp, KC1_temp_quality,
                                            #AU2_precipitation_code, AU2_intensity_and_proximity_code,
                                            #GA1_cloud_type_code, GA1_cloud_type_quality_code,
                                            #GA2_cloud_type_code, GA2_cloud_type_quality_code,
                                            #AA1_precipitation_liquid, AA1_period_quantity_hrs, AA1_depth, AA1_condition_quality, AA1_quality_code,
                                            #AA2_precipitation_liquid, AA2_period_quantity_hrs, AA2_depth, AA2_condition_quality, AA2_quality_code)
      
      # only give data for date of record
      results.day <- results.slim %>% filter(date == cur.date.s)
    } # <END> else
    
    # bind weather and current record
    cur.record.final <- cbind(cur.record,results.day)
    
    # mark all with poor quality
    if(clean) {
      # remove all where quality
      print("cleaning where quality is equal to 9 (i.e., NOAA bad data flag)")
    }
    
    # bind weather data for given record with full dataframe
    weather.df <- bind_rows(mutate_all(weather.df, as.character), mutate_all(cur.record.final, as.character))
    #bind_rows(weather.df, cur.record.final)
    
  } # <END> for loop
  
  return(weather.df)
} # <END> function