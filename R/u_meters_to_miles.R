#' contextR
#' @name u_meters_to_miles
#' @export
#' @param meters class: numeric
#' @examples
#' u_meters_to_miles(meters)
u_meters_to_miles <- function(meters) {
  miles <- meters * 0.00062137
  return(miles)
}