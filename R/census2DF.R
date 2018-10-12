#' A Function to Provide Environmental Context to Data
#' @name census2df
#' @param df class: data.frame description: must have columns labelled identically to: gps_lat, gps_long, user_id, date 
#' @param date_c class: string; variable name for column with date data
#' @param date_f class: string; date format
#' @param lat class: string; variable name for column with GPS Latitude data
#' @param long class: string; variable name for column with GPS Longitude data
#' @import tidyverse lubridate tidycensus
#' @examples
#' census2df(df,CENSUS_API='123abc',id='user_id',searchBy='zipcode',searchTarget='16803')

#' @export
# function to search and bind data with df given
census2df <- function(df,CENSUS_API='',id='user_id',searchBy='zipcode',searchTarget='16803') {
  if(CENSUS_API == '') {
    API_ERROR <- "ERROR: No Census API key provided"
    print(API_ERROR)
    census.df <- data.frame(RESULT = API_ERROR)
  } else {
    # display warning on large API draws
    if(searchBy == '' & searchTarget == '') {
      API_WARNING <- "WARNING: no zipcode or state provided. Returning records for entire United State (this may take a while)"
      warning(API_WARNING)
    }
    
    # get distinct rows (no need to duplicate work)
    df.clean <- df %>% distinct()
    
    # bring in API key
    census_api_key(CENSUS_API)
    
    # setup vector of data to extract
    chosen.ones <- c(INCOME_median = "B19013_001")
    
    # bind data #### 
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    census.df <- data.frame()
    for(record.id in 1:nrow(df.clean)){
      # isolate record
      cur.record <- df.clean[record.id,]
      cur.id <- cur.record %>% select(id)

      # transform searchby input
      searchBy = tolower(searchBy)
      
      # search by user-specified type
      if(searchBy == 'zip'){
        # get data by zip code
        search.result <- get_acs(geography = "zcta", 
                        variables = chosen.ones) %>% 
          filter(GEOID == as.character(searchTarget))
      } else {
        if(searchBy='county') {
          search.result <- get_acs(geography = "county", 
                        variables = chosen.ones, 
                        state = searchTarget)
        } else {
          if(searchBy='state') {
            search.result <- get_acs(geography = "state", 
                                     variables = chosen.ones, 
                                     state = searchTarget)
          } else {
            print("ERROR: no other geographies supported at this time")
          }
        }
      }
      
      # bind weather and current record
      cur.record.final <- cbind(cur.record,search.result)
      
      # bind weather data for given record with full dataframe
      census.df <- bind_rows(census.df, cur.record.final)
    } # <END> for loop
  }
  return(census.df)
} # <END> function