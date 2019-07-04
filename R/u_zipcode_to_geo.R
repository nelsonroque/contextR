u_zipcode_to_geo <- function(zip, ...) {
  # clean zipcodes
  #czip <- clean.zipcodes(zip)
  zip <- as.character(zip)
  
  # load package data
  data(zipcode)

  # merge data
  og <- tibble(zip) 
  result <- og %>% inner_join(zipcode)
  
  return(result)
}
