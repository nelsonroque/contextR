#' contextR
#' @name create_airnow_api_call
#' @export
#' @param API_KEY class: string
#' @param lat class: numeric
#' @param lng  class: numeric
#' @param ts  class: string (ISO format: 2019-10-23T16:00:00)
#' @param distance  class: numeric (miles)
#' @examples
#' create_airnow_api_call(API_KEY, lat, lng, ts, distance=10)
create_airnow_api_call <- function(API_KEY, lat, lng, ts, distance=10) {
  base_call = "http://www.airnowapi.org/aq/observation/latLong/historical/?format=application/json&"
  var_call = paste0("latitude=", lat, "&", "longitude=", lng, "&", "date=", ts, "&", "distance=", distance, "&", "API_KEY=", API_KEY)
  api_call = paste0(base_call, var_call)
  return(api_call)
}