create_fcc_api_call <- function(lat, lng) {
  base_call = "https://geo.fcc.gov/api/census/area?"
  var_call = paste0("lat=", lat, "&lon=", lng, "&format=json")
  api_call = paste0(base_call, var_call)
  return(api_call)
}

create_fcc_api_call_v <- Vectorize(create_fcc_api_call)