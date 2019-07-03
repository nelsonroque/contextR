create_photon_api_call <- function(lat, lng) {
  base_call = "http://photon.komoot.de/reverse?"
  var_call = paste0("lon=", lng, "&lat=", lat)
  api_call = paste0(base_call, var_call)
  return(api_call)
}

create_photon_api_call_v <- Vectorize(create_photon_api_call)