test.df <- data.frame(survey_response = c(90,90),
                     dob = c("1991-05-16","1991-04-04"),
                     lat = c(40.7788,39.7788),
                     lng = c(-77.84137,-77.84137),
                     ts = c(1273687200,1273687200)) %>%
  mutate(og_date = lubridate::as_datetime(ts)) %>%
  mutate(api_call = create_api_call(DARK_SKY_API_KEY, lat, lng, ts)) %>%
  rowwise() %>%
  mutate(api_call_id = digest::digest(api_call, algo="md5")) %>%
  mutate(lat_lng = create_lat_lng_str(lat,lng)) %>%
  mutate(season = sapply(og_date, get_season_from_date)) %>%
  mutate(season2 = sapply(og_date, get_season_from_date_pkg)) %>%
  mutate(zodiac = sapply(dob, get_zodiac_sign_from_dob))