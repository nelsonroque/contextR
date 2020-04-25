#' contextR
#' @name create_darksky_api_call
#' @export
#' @param lat class: numeric
#' @param lng  class: numeric
#' @param delimiter  class: string
#' @examples
#' create_lat_lng_str(lat, lng, delimiter=",")
create_lat_lng_str <- function(lat,lng,delimiter=","){
  return(paste(lat,lng,sep=delimiter))
}