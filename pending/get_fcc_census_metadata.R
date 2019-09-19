get_fcc_census_metadata <- function(api_call, ...){
  # wrap in URL for long URL calls
  raw.json <- jsonlite::fromJSON(url(api_call))
  
  # for debug
  # print(raw.json)
  
  # save first result
  # need to read why multiples are returned?
  result = tibble::tibble(raw.json$results)
  result1 = result %>% filter(row_number() == 1)
  names(result1) <- paste0("fcc_",names(result1))

  # return as a tibble for niceties
  return(result=list(raw_json=raw.json, df=result1))
}