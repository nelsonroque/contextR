create_darksky_api_call <- function(DARK_SKY_API_KEY, lat, lng, ts) {
  base_call = "https://api.darksky.net/forecast/"
  var_call = paste0("/", lat, ",", lng, ",", ts)
  api_call = paste0(base_call, DARK_SKY_API_KEY, var_call)
  return(api_call)
}

create_darksky_api_call_v <- Vectorize(create_darksky_api_call)