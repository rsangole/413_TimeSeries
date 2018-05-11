library(magrittr)
library(forecast)
library(dplyr)
library(lubridate)
library(forecast)
library(ggplot2)
library(purrr)
library(lattice)
library(sweep)
library(astsa)
library(janitor)
library(xts)

# Data Source: https://finance.yahoo.com/quote/CMI/history?period1=1368158400&period2=1525924800&interval=1d&filter=history&frequency=1d

cmi <- readr::read_csv('data/CMI.csv') %>% clean_names()
cat <- readr::read_csv('data/CAT.csv') %>% clean_names()
mro <- readr::read_csv('data/MRO.csv') %>% clean_names()

cmi
cat
mro

diesel <- readxl::read_xls('data/diesel.xls', sheet = 2, skip = 2) %>%
    clean_names() %>%
    mutate(date = as.Date(date)) %>%
    select(date, diesel = new_york_harbor_ultra_low_sulfur_no_2_diesel_spot_price_dollars_per_gallon) %>% filter(date >= ymd('2013-05-10'))

df <- tibble(date    = cmi$date,
             cmi_vol = cmi$volume,
             cmi     = cmi$adj_close,
             cat     = cat$adj_close,
             mro     = mro$adj_close) %>%
    left_join(diesel) %>%
    tidyr::fill(diesel, .direction = 'down')

df_ts <- xts(df[,-1], order.by = df$date, frequency = 365)

plot(df_ts[,2:3], legend.loc = 'topleft', main = '5 year Adjusted Close USD', grid.col='lightgray')
lines(df_ts$diesel, on = NA, col = 'blue', main = 'Diesel Prices')
lines(df_ts$cmi_vol, on = NA, col = 'red', main = 'CMI Volume')

xyplot(cat~cmi, df, asp=1, panel = function(x, y, ...) {
        panel.xyplot(x,y, type = "p", cex=.5, alpha=0.5)
        panel.xyplot(x, y, type = "smooth", col = "red", lwd = 2)})
xyplot(diesel~cmi, df, asp=1, panel = function(x, y, ...) {
    panel.xyplot(x,y, type = "p", cex=.5, alpha=0.5)
    panel.xyplot(x, y, type = "smooth", col = "red", lwd = 2)})
xyplot(log(cmi_vol)~cmi, df, asp=1, panel = function(x, y, ...) {
    panel.xyplot(x, y, type = "p", cex=.5, alpha=0.5)
    panel.xyplot(x, y, type = "smooth", col = "red", lwd = 2)})

df_ts$vol_ind <- as.numeric(log(df_ts$cmi_vol)<14.2)

train <- window(df_ts, end = '2017-12-31')
test <- window(df_ts, start = '2018-01-01')

plot(train$cmi)
plot(test$cmi)
acf2(train$cmi)
plot(diff(train$cmi))
acf2(diff(train$cmi))
sarima(train$cmi,0,1,0)
sarima.for(train[,2],0,1,0,n.ahead = 30*1)
lines(ts(test$cmi,start = 1171,frequency = 1),col='blue')

xreg = matrix(c(lag(train$cat), lag(train$diesel), lag(train$cmi_vol), lag(train$vol_ind)),ncol = 4,byrow = T)[-1,]
xreg_scaled <- scale(xreg)
sarima(train$cmi[-1],0,1,0,xreg = xreg_scaled[,3:4])
sarima.for(train[,2],0,1,0,n.ahead = 88,
           xreg = matrix(c(lag(train$cat),
                           lag(train$diesel),
                           lag(train$cmi_vol)),ncol = 3,byrow = T),
           newxreg = matrix(c(lag(test$cat),
                              lag(test$diesel),
                              lag(test$cmi_vol)),ncol=3,byrow=T)[-1,])
lines(ts(test$cmi,start = 1171,frequency = 1),col='blue')
