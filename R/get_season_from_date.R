get_season_from_date <- function(date, hemi="northern", use_pkg = T){
  if(use_pkg){
    season <- tolower(lunar::terrestrial.season(as.Date(date)))
    } else {
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
    }
  
  return(season)
}

get_season_from_date_v <- Vectorize(get_season_from_date)