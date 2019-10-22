#' contextR

#' @name create_breezometer_api_call
#' @param API_KEY class: string
#' @param lat class: numeric
#' @param lng  class: numeric
#' @param ts  class: string, ISO format: 2019-10-23T16:00:00
#' @param features  class: string
#' @param metadata class: string
#' @examples
#' create_breezometer_api_call(API_KEY, lat, lng, ts, features, metadata)
#' @export
create_breezometer_api_call <- function(API_KEY, lat, lng, ts, features="breezometer_aqi,local_aqi,sources_and_effects,dominant_pollutant_concentrations,pollutants_concentrations", metadata="true") {
  base_call = "https://api.breezometer.com/air-quality/v2/historical/hourly?"
  var_call = paste0("lat=", lat, "&", "lon=", lng, "&", "key=", API_KEY, "&", "datetime=", ts, "&", "features=", features, "&", "metadata=", metadata)
  api_call = paste0(base_call, var_call)
  return(api_call)
}