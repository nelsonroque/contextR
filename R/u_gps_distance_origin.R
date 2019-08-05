u_gps_distance_origin <- function(homepoint, lat2, lon2, return_type = "meters") {
  result <- as.numeric(geosphere::distm(c(homepoint[1], homepoint[2]), c(lon2, lat2)))
  
  if(return_type == "km"){
    result <- result/1000
  } else {
    if(return_type == "miles"){
      result <- u_meters_to_miles(result)
    } else {
      result <- result
    }
  }
  
  return(result) # default unit: meters
}
