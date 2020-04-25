#' contextR
#' @name get_zodiac_sign_from_date
#' @export
#' @param date class: POSIXct
#' @import DescTools
#' @examples
#' get_zodiac_sign_from_date(date)
get_zodiac_sign_from_date <- function(date){
  return(tolower(DescTools::Zodiac(date)))
}
