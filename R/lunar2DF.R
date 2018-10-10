#' A Function to Provide Environmental Context to Data
#' @name lunar2df
#' @param df class: data.frame description: must have columns labelled identically to: gps_lat, gps_long, user_id, date 
#' @param date_c class: string; variable name for column with date data
#' @param date_f class: string; date format
#' @param lat class: string; variable name for column with GPS Latitude data
#' @param long class: string; variable name for column with GPS Longitude data
#' @import tidyverse lubridate lunar
#' @examples
#' lunar2df(df,date_c="date", date_f="%Y/%m/%d", id='user_id', lat='gps_lat', long='gps_long')

#' @export
# function to search and bind data with df given
lunar2df <- function(df,id='user_id',date_c='date',date_f='%Y/%m/%d',lat='gps_lat',long='gps_long',nLunar=8) {
  
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
  lunar.df <- data.frame()
  for(record.id in 1:nrow(df.pp)){
    # isolate record
    cur.record <- df.pp[record.id,]
    cur.id <- cur.record %>% select(id)
    cur.date <- cur.record %>% select(DATE_SEARCH)
    cur.date2 <- cur.record %>% select(DATE)
    cur.year <- cur.record %>% select(YEAR) %>% as.numeric
    cur.gps.lat <- cur.record %>% select(lat) %>% as.numeric
    cur.gps.long <- cur.record %>% select(long) %>% as.numeric
    
    # extract all lunar phase results
    lunar.day <- data.frame(lunar_dist_earth_radii = lunar.distance.mean(cur.date2), #units of earth radii
                            lunar_phase = lunar.phase(cur.date2, name=nLunar))
    
    # bind weather and current record
    cur.record.final <- cbind(cur.record,lunar.day)
    
    # bind weather data for given record with full dataframe
    lunar.df <- bind_rows(lunar.df, cur.record.final)
  } # <END> for loop
  
  return(lunar.df)
} # <END> function