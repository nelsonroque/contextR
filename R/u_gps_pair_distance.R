u_gps_pair_distance <- function(lat1, lon1, lat2, lon2) {
  result <- as.numeric(geosphere::distm(c(lon1, lat1), c(lon2, lat2)))
  return(result) # default unit: meters
}
