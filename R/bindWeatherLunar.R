#' A Function to Provide Environmental Context to Data
#' @param df class: data.frame description: must have columns labelled identically to: gps_lat, gps_long, user_id, date 
#' @param nLunar class: integer; description: number of lunar phase factors to return
#' @param returnAll class: boolean; T = returns list with many steps along the way to creating final dataframe; F = returns dataframe
#' @import tidyverse rnoaa lubridate
#' @importFrom lunar lunar.distance.mean lunar.phase
#' @examples
#' bind.WeatherLunar(df,nLunar=8,returnAll=F) # just give me my dataset back with contextual data
#' bind.WeatherLunar(df,nLunar=8,returnAll=T) # I want to see all the behind the scenes data and do more

#' @export
# function to search and bind data with df given
bind.WeatherLunar <- function(df,nLunar=8,returnAll=F) {

  # add other date format to original data
  df$date_format = gsub("-","",as.Date(df$date,"%Y/%m/%d"))

  # weather station search parameters
  SEARCH_RADIUS = 5 # in km
  SEARCH_RADIUS_EXTEND = 5 # in km

  # create holder for all NOAA & LUNAR data
  all.NOAA_STATIONS <- data.frame()
  all.NOAA_STATIONS_FINAL <- data.frame()
  all.NOAA <- data.frame()
  all.NOAA_SUMMARY <- data.frame()
  all.LUNAR <- data.frame()

  # search for NOAA stations for each record
  for(current_record in 1:length(df$gps_lat)) {

    # get search year from record
    PART_ID = df$user_id[current_record]
    SEARCH_YEAR = as.numeric(year(df$date[current_record]))
    SEARCH_DATE = as.Date(df$date[current_record])
    SEARCH_DATE_C = gsub("-","",as.Date(df$date[current_record],"%Y/%m/%d"))

    # initialize station results
    weather_station_results <- data.frame()

    # initialize station search counter
    station_search = 0

    # get station results (if any)
    # if at least one record or more not returned, keep extending by 5km
    while(nrow(weather_station_results) < 1) {
      SEARCH_RADIUS <- SEARCH_RADIUS + SEARCH_RADIUS_EXTEND # extend by 5 km

      # status msg
      print(paste0("STATUS: SEARCHING FOR WEATHER STATIONS NEAR GPS COORDS WITH ",SEARCH_RADIUS," KM RADIUS"))

      # search for stations
      weather_station_results <- isd_stations_search(lat = df$gps_lat[current_record],
                                                     lon = df$gps_long[current_record],
                                                     radius = SEARCH_RADIUS)
      # increment station search counter
      station_search <- station_search + 1
    }

    # clean weather results to get wban != 99999
    weather_station_final <- weather_station_results %>%
                        filter(wban != "99999" & usaf != "999999")

    # status msg
    print(paste0("STATUS: SEARCHING WEATHER DATA FROM NEAREST STATION. WILL TAKE A WHILE IF FIRST TIME LOADING FROM THIS STATION AND YEAR."))

    # extract all NOAA data from first station
    noaa_results <- isd(usaf = weather_station_final$usaf[1],
                        wban = weather_station_final$wban[1],
                        year = SEARCH_YEAR) %>%
                    # select only certain columns (new columns will be added over time)
                    select(date,time,
                           temperature,
                           air_pressure,
                           wind_speed,wind_direction,
                           ceiling_height) %>%
                    # select only rows requested
                    filter(date == SEARCH_DATE_C) %>%
                    # remove out of range values
                    mutate(wind_speed_clean = ifelse(wind_speed == "9999", NA, as.numeric(wind_speed)),
                           wind_direction_clean = ifelse(wind_direction == "999", NA, as.numeric(wind_direction)),
                           air_pressure_clean = ifelse(air_pressure == "99999", NA, as.numeric(air_pressure)),
                           ceiling_height_clean = ifelse(ceiling_height == "99999", NA, as.numeric(ceiling_height)),
                           temperature_clean = ifelse(temperature == "+9999", NA, gsub("[[:punct:]]","",temperature))) %>%
                    # convert temperature
                    mutate(temperature_C = as.numeric(temperature_clean)/10) %>%    # scale temperature based on NOAA API info
                    mutate(temperature_F = round(c.2.f(temperature_C), 2)) %>% # convert to degrees F
                    # get month day and year as seperate columns for graphing
                    mutate(month = month(as.Date(date,"%Y%m%d"),label=F),
                           day = day(as.Date(date,"%Y%m%d")),
                           year = year(as.Date(date,"%Y%m%d")))

    # merge identifiers to NOAA data from lookup
    noaa_results$user_id <- df$user_id[current_record]
    noaa_results$gps_lat <- df$gps_lat[current_record]
    noaa_results$gps_lat <- df$gps_lat[current_record]
    noaa_results$gps_long <- df$gps_long[current_record]

    # merge station ids
    noaa_results$usaf <- weather_station_final$usaf
    noaa_results$wban <- weather_station_final$wban
    noaa_results$station_name <- weather_station_final$station_name
    noaa_results$state <- weather_station_final$state
    noaa_results$icao <- weather_station_final$icao
    noaa_results$latitude <- weather_station_final$latitude
    noaa_results$longitude <- weather_station_final$longitude
    noaa_results$elev_m <- weather_station_final$elev_m

    # status msg
    print(paste0("STATUS: SEARCHING FOR LUNAR DATA"))

    # extract all lunar phase results
    lunar_results <- data.frame(date = gsub("-","",as.character(SEARCH_DATE)),
                                   date.as.date = SEARCH_DATE,
                                   lunar_dist_earth_radii = lunar.distance.mean(SEARCH_DATE), #units of earth radii
                                   lunar_phase = lunar.phase(SEARCH_DATE, name=nLunar))

    # bind results to master data frames
    all.NOAA_STATIONS <- rbind(all.NOAA_STATIONS,weather_station_results)
    all.NOAA_STATIONS_FINAL <- rbind(all.NOAA_STATIONS_FINAL,weather_station_final)
    all.NOAA <- rbind(all.NOAA, noaa_results)
    all.LUNAR <- rbind(all.LUNAR, lunar_results)
  }

  # status msg
  print(paste0("STATUS: SUMMARISING NOAA DATA"))

  # get summary results
  all.NOAA_SUMMARY <- all.NOAA %>%
    select(date,month,day,year,
           gps_lat,gps_long,
           temperature_F,
           wind_speed_clean,
           wind_direction_clean,
           air_pressure_clean,
           ceiling_height_clean) %>%
    group_by(date,gps_lat,gps_long) %>%
    summarise_at(vars(temperature_F,wind_speed_clean,wind_direction_clean,air_pressure_clean,ceiling_height_clean),
                 funs(mean(.,na.rm=T),
                      sd(.,na.rm=T),
                      min(.,na.rm=T),
                      max(.,na.rm=T)))

  # status msg
  print(paste0("STATUS: MERGING DATASETS"))

  # bind NOAA + LUNAR
  NOAA_LUNAR <- merge(all.NOAA,all.LUNAR,by="date",all=T) %>% distinct()

  # bind NOAA Summary + LUNAR
  NOAA_SUMMARY_LUNAR <- merge(all.NOAA_SUMMARY,all.LUNAR,by=c("date"),all=T) %>% distinct()

  # bind NOAA + LUNAR + ORIGINAL DATA
  DF_NOAA_LUNAR <- merge(df, NOAA_LUNAR,
              by.x="date_format",
              by.y="date",
              all=T)

  # bind NOAA SUMMARY + LUNAR + ORIGINAL DATA
  DF_NOAA_SUMMARY_LUNAR <- merge(df, NOAA_SUMMARY_LUNAR,
             by.x=c("date_format","gps_lat","gps_long"),
             by.y=c("date","gps_lat","gps_long"),
             all=T)

  if(returnAll) {
    # return data
    return(list(original_data = df,
                NOAA_data = all.NOAA,
                NOAA_summary = all.NOAA_SUMMARY,
                LUNAR_data = all.LUNAR,
                NOAA_LUNAR_only = NOAA_LUNAR,
                NOAA_LUNAR_ORIGINAL = DF_NOAA_LUNAR,
                NOAA_SUMMARY_LUNAR_ORIGINAL = DF_NOAA_SUMMARY_LUNAR,
                weather_station_data = all.NOAA_STATIONS,
                weather_station_clean = all.NOAA_STATIONS_FINAL))
  } else {
    return(DF_NOAA_SUMMARY_LUNAR)
  }
}
