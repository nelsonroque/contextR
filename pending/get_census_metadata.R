get_census_metadata <- function(api_call, ...){
  raw.json <- jsonlite::fromJSON(api_call)

  # return as a tibble for niceties
  return(tibble::as_tibble(raw.json$results))
}