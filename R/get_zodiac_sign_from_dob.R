#' contextR

#' @name get_zodiac_sign_from_date
#' @param date class: POSIXct
#' @import DescTools
#' @examples
#' get_zodiac_sign_from_date(date)
#' @export
get_zodiac_sign_from_date <- function(date){
  return(tolower(DescTools::Zodiac(date)))
}
