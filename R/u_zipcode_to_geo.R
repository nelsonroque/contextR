u_zipcode_to_geo <- function(zip, ...) {
  data(zipcode)
  
  # unsafe (think how to pad 0s)
  if(is.numeric(zip)){
    zip <- as.character(zip)
  }
  
  print(zip)
  
  result <- tibble(zip) %>% inner_join(zipcode)
  
  return(tibble::tibble(result))
}
