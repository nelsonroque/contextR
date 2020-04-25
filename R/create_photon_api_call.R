#' contextR

#' @name create_photon_api_call
#' @export
#' @param lat class: numeric
#' @param lng  class: numeric
#' @examples
#' create_photon_api_call(lat, lng)
create_photon_api_call <- function(lat, lng) {
  base_call = "http://photon.komoot.de/reverse?"
  var_call = paste0("lon=", lng, "&lat=", lat)
  api_call = paste0(base_call, var_call)
  return(api_call)
}