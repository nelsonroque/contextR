# load library
devtools::install_github('nelsonroque/contextR')
library(contextR)

# create data structure required for function
test.df <- data.frame(user_id = c("1000","2000","3000"),
                      dates = c("2018/2/1","2017/5/16","2018/2/1"),
                      gps_lat = c(40.825651,39.825651,25.790654),
                      gps_long = c(-77.887898,-76.887898,-80.1300455),
                      RT = c(1200,1100,3000),
                      Accuracy = c(.9,.99,.5))

# get weather data
a <- weather2df(test.df,date_c="dates", date_f="%Y/%m/%d", id='user_id', lat='gps_lat', long='gps_long', radius=5, clean=F)




# get lunar data
b <- lunar2df(test.df,date_c="date", date_f="%Y/%m/%d", id='user_id', lat='gps_lat', long='gps_long')

# bring it together
c <- merge(a,b,by=c('user_id','DATE_SEARCH'))
