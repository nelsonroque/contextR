get_lunar_context_from_date <- function(date, ...){
  # if date provided is not a valid date, make it one [TODO: expose `format`]
  if(class(date) != "Date"){
    date <- as.Date(date, format="%Y-%m-%d")
  }
  
  # extract lunar data (no API -- this is cheap)
  lunar_phase_radians <- lunar::lunar.phase(date)
  lunar_phase_4cat <- lunar::lunar.phase(date,name=4)
  lunar_phase_8cat <- lunar::lunar.phase(date,name=8)
  lunar_distance_cat <- lunar::lunar.distance(date,name=T)
  lunar_distance_num <- lunar::lunar.distance(date,name=F)
  lunar_illumination <- lunar::lunar.illumination(date)
  
  # return as a tibble for niceties
  return(tibble::tibble(date,
                        lunar_phase_radians,
                        lunar_phase_4cat,
                        lunar_phase_8cat,
                        lunar_distance_cat,
                        lunar_distance_num,
                        lunar_illumination))
}

get_lunar_context_from_date_v <- Vectorize(get_lunar_context_from_date)