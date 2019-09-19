#' contextR

#' @name u_meters_to_miles
#' @param meters class: numeric
#' @examples
#' u_meters_to_miles(meters)
#' @export
u_meters_to_miles <- function(meters) {
  miles <- meters * 0.00062137
  return(miles)
}