#' contextR
#' @name hash_api_call
#' @export
#' @param api_call class: string
#' @param hash class: string
#' @import digest
#' @examples
#' hash_api_call(api_call, hash="md5")
hash_api_call <- function(api_call, algo="md5") {
  return(digest::digest(api_call, algo=algo))
}