#' contextR

#' @name create_darksky_api_call
#' @param DARK_SKY_API_KEY class: string
#' @param lat class: numeric
#' @param lng  class: numeric
#' @examples
#' create_darksky_api_call(DARK_SKY_API_KEY, lat, lng, ts)
#' @export
create_darksky_api_call <- function(DARK_SKY_API_KEY, lat, lng, ts) {
  base_call = "https://api.darksky.net/forecast/"
  var_call = paste0("/", lat, ",", lng, ",", ts)
  api_call = paste0(base_call, DARK_SKY_API_KEY, var_call)
  return(api_call)
}