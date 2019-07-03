hash_api_call <- function(api_call, algo="md5") {
  return(digest::digest(api_call, algo=algo))
}

hash_api_call_v <- Vectorize(hash_api_call)