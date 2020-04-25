#' contextR
#' @name get_airquality_context
#' @export
#' @param data class: data.frame
#' @param rate_limit class: numeric
#' @param save_results class: boolean
#' @param results_dir class: character
#' @import tidyverse
#' @import jsonlite
#' @examples
#' get_weather_context(api_call, hash="md5")
get_airquality_context <- function(data, rate_limit=8.6, save_results=T, results_dir = "") {
  
  # adjust rate limit if user set too low
  if(rate_limit < 8.6) {
    rate_limit = 8.6
    warning("`rate_limit` must be 8.5 or greater.")
  }

  # present message with total execution time expected
  print("Please be patient, execution will take ", 8.6 * nrow(data), "seconds.")
  
  # init list for records
  aqi_records = list()
  for(i in 1:nrow(data)) {
    cur_row <- data[i,]
    cur_callid =  hash_api_call_v(cur_row$airnow_api_call, algo="md5")
    cur_call = cur_row$airnow_api_call
  
    # pull records from API
    cur_df = jsonlite::fromJSON(cur_call)
    
    if(save_results) {
      write.csv(cur_df, paste0(results_dir, "EPA_AIRNOW_",cur_callid, ".csv"), row.names = F)
    }
  
    # save record to list
    aqi_records[[cur_callid]] = cur_df
    
    # sleep for rate limiting purposes
    Sys.sleep(rate_limit)
  }
  return(api_records)
}
