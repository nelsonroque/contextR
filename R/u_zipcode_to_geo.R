#' contextR

#' @name u_zipcode_to_geo
#' @param zip class: numeric
#' @import zipcode
#' @import tidyverse
#' @examples
#' u_zipcode_to_geo(zip)
#' @export
u_zipcode_to_geo <- function(zip, ...) {
  # clean zipcodes
  #czip <- clean.zipcodes(zip)
  zip <- as.character(zip)
  
  # load package data
  data(zipcode)

  # merge data
  og <- tibble::tibble(zip) 
  result <- og %>% inner_join(zipcode)
  
  return(result)
}
