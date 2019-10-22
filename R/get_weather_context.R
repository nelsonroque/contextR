#' contextR

#' @name get_weather_context
#' @param api_call class: string
#' @param hash class: string
#' @import tidyverse
#' @import jsonlite
#' @examples
#' get_weather_context(api_call, hash="md5")
#' @export
get_weather_context <- function(api_call, hash="md5"){
  
  # POWERED BY DARKSKY: (https://darksky.net/poweredby)
  # FOR API INFORMATION, SEE (https://darksky.net/dev/docs#time-machine-request)
  
  # create blank dataframes
  daily.results <- data.frame()
  hourly.results <- data.frame()
  minutely.results <- data.frame()
  currently.results <- data.frame()
  alerts.results <- data.frame()
  
  for(i in 1:length(api_call)){
    raw.json <- jsonlite::fromJSON(api_call[i])
    
    # daily
    daily.df <- tibble::as_tibble(raw.json$daily$data) %>%
      mutate(darksky_api_call = api_call[i]) %>%
      mutate(darksky_api_call_id = digest::digest(darksky_api_call, algo=hash)) # save MD5 of call
    daily.results <- bind_rows(daily.results, daily.df)
    
    # hourly
    hourly.df <- tibble::as_tibble(raw.json$hourly$data) %>%
      mutate(darksky_api_call = api_call[i]) %>%
      mutate(darksky_api_call_id = digest::digest(darksky_api_call, algo=hash)) # save MD5 of call
    hourly.results <- bind_rows(hourly.results, hourly.df)
    
    # minutely
    minutely.df <- tibble::as_tibble(raw.json$minutely$data) %>%
      mutate(darksky_api_call = api_call[i]) %>%
      mutate(darksky_api_call_id = digest::digest(darksky_api_call, algo=hash)) # save MD5 of call
    minutely.results <- bind_rows(minutely.results, minutely.df)
    
    # current
    currently.df <- tibble::as_tibble(raw.json$currently$data) %>%
      mutate(darksky_api_call = api_call[i]) %>%
      mutate(darksky_api_call_id = digest::digest(darksky_api_call, algo=hash)) # save MD5 of call
    currently.results <- bind_rows(currently.results, currently.df)
    
    # alerts
    alerts.df <- tibble::as_tibble(raw.json$alerts$data) %>%
      mutate(darksky_api_call = api_call[i]) %>%
      mutate(darksky_api_call_id = digest::digest(darksky_api_call, algo=hash)) # save MD5 of call
    alerts.results <- bind_rows(alerts.results, alerts.df)
    
  }
  
  return(context=list(daily=daily.results,
                      minutely=minutely.results,
                      hourly=hourly.results,
                      currently=currently.results,
                      alerts=alerts.results))
}