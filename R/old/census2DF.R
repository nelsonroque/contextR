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
    chosen.ones <- c(INCOME_median = "B19013_001",
                     COUNT.ASSISTANCE.TOTAL = 'B09010_001',
                     
                     INCOME.RETIRED.AGGREGATE = 'B19069_001',
                     INCOME.RETIRED.TOTAL = 'B19059_001',
                     INCOME.RETIRED.W.RETIREINCOME = 'B19059_002',
                     INCOME.RETIRED.WO.RETIREINCOME = 'B19059_003',
                     
                     COUNT.CITIZENSHIP.TOTAL = 'B05001_001',
                     COUNT.CITIZENSHIP.BornUS = 'B05001_002',
                     COUNT.CITIZENSHIP.BornPRIslands = 'B05001_003',
                     COUNT.CITIZENSHIP.BornAbroadAmericanParents = 'B05001_004',
                     COUNT.CITIZENSHIP.ByNaturalization = 'B05001_005',
                     COUNT.CITIZENSHIP.NotUSCitizen = 'B05001_006',
                     
                     COUNT.ALL = 'B01001_002',
                     COUNT.AGE.MALE.ALL = 'B01001_002',
                     COUNT.AGE.MALE.UNDER.5 = 'B01001_003',
                     COUNT.AGE.MALE.5.to.9 = 'B01001_004',
                     COUNT.AGE.MALE.10.to.14 = 'B01001_005',
                     COUNT.AGE.MALE.15.to.17 = 'B01001_006',
                     COUNT.AGE.MALE.18.to.19 = 'B01001_007',
                     COUNT.AGE.MALE.20 = 'B01001_008',
                     COUNT.AGE.MALE.21 = 'B01001_009',
                     COUNT.AGE.MALE.22.to.24 = 'B01001_010',
                     COUNT.AGE.MALE.25.to.29 = 'B01001_011',
                     COUNT.AGE.MALE.30.to.34 = 'B01001_012',
                     COUNT.AGE.MALE.35.to.39 = 'B01001_013',
                     COUNT.AGE.MALE.40.to.44 = 'B01001_014',
                     COUNT.AGE.MALE.45.to.49 = 'B01001_015',
                     COUNT.AGE.MALE.50.to.54 = 'B01001_016',
                     COUNT.AGE.MALE.55.to.59 = 'B01001_017',
                     COUNT.AGE.MALE.60.to.61 = 'B01001_018',
                     COUNT.AGE.MALE.62.to.64 = 'B01001_019',
                     COUNT.AGE.MALE.65.to.66 = "B01001_020",
                     COUNT.AGE.MALE.67.to.69 = 'B01001_021',
                     COUNT.AGE.MALE.70.to.74 = 'B01001_022',
                     COUNT.AGE.MALE.75.to.79 = 'B01001_023',
                     COUNT.AGE.MALE.80.to.84 = 'B01001_024',
                     COUNT.AGE.MALE.85.plus = 'B01001_025',
                     
                     COUNT.AGE.FEMALE.ALL = 'B01001_026',
                     COUNT.AGE.FEMALE.UNDER.5 = 'B01001_027',
                     COUNT.AGE.FEMALE.5.to.9 = 'B01001_028',
                     COUNT.AGE.FEMALE.10.to.14 = 'B01001_029',
                     COUNT.AGE.FEMALE.15.to.17 = 'B01001_030',
                     COUNT.AGE.FEMALE.18.to.19 = 'B01001_031',
                     COUNT.AGE.FEMALE.20 = 'B01001_032',
                     COUNT.AGE.FEMALE.21 = 'B01001_033',
                     COUNT.AGE.FEMALE.22.to.24 = 'B01001_034',
                     COUNT.AGE.FEMALE.25.to.29 = 'B01001_035',
                     COUNT.AGE.FEMALE.30.to.34 = 'B01001_036',
                     COUNT.AGE.FEMALE.35.to.39 = 'B01001_037',
                     COUNT.AGE.FEMALE.40.to.44 = 'B01001_038',
                     COUNT.AGE.FEMALE.45.to.49 = 'B01001_039',
                     COUNT.AGE.FEMALE.50.to.54 = 'B01001_040',
                     COUNT.AGE.FEMALE.55.to.59 = 'B01001_041',
                     COUNT.AGE.FEMALE.60.to.61 = 'B01001_042',
                     COUNT.AGE.FEMALE.62.to.64 = 'B01001_043',
                     COUNT.AGE.FEMALE.65.to.66 = "B01001_044",
                     COUNT.AGE.FEMALE.67.to.69 = 'B01001_045',
                     COUNT.AGE.FEMALE.70.to.74 = 'B01001_046',
                     COUNT.AGE.FEMALE.75.to.79 = 'B01001_047',
                     COUNT.AGE.FEMALE.80.to.84 = 'B01001_048',
                     COUNT.AGE.FEMALE.85.plus = 'B01001_049')
    
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
        if(searchBy == 'county') {
          search.result <- get_acs(geography = "county", 
                        variables = chosen.ones, 
                        state = searchTarget)
        } else {
          if(searchBy == 'state') {
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