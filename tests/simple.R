# load library
library(contextR)

# create data structure required for function
test.df <- data.frame(user_id = c("1000","2000","3000"),
                      date = c("2018/2/1","2018/2/1","2018/2/1"),
                      gps_lat = c(40.825651,39.825651,25.790654),
                      gps_long = c(-77.887898,-76.887898,-80.1300455),
                      RT = c(1200,1100,3000),
                      Accuracy = c(.9,.99,.5))

final.df <- weather2df(test.df)
View(final.df)
