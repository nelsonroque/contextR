#' contextR
#' @name u_gps_pair_distance
#' @export
#' @param lat1 class: numeric
#' @param lng1 class: numeric
#' @param lat2 class: numeric
#' @param lng2 class: numeric
#' @param return_type class: string
#' @import geosphere
#' @examples
#' u_gps_pair_distance(lat1, lng1, lat2, lng2, return_type = "meters")
u_gps_pair_distance <- function(lat1, lon1, lat2, lon2, return_type = "meters") {
  result <- as.numeric(geosphere::distm(c(lng1, lat1), c(lng2, lat2)))
  
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
