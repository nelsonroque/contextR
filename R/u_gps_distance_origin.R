#' contextR
#' @name u_gps_distance_origin
#' @export
#' @param homepoint class: vector(lat, lng)
#' @param lat2 class: numeric
#' @param lng2 class: numeric
#' @param return_type class: string
#' @import geosphere
#' @examples
#' u_gps_distance_origin(homepoint, lat2, lon2, return_type = "meters")
u_gps_distance_origin <- function(homepoint, lat2, lng2, return_type = "meters") {
  result <- as.numeric(geosphere::distm(c(homepoint[2], homepoint[1]), c(lng2, lat2)))
  
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
