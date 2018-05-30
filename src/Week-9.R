library(magrittr)
library(forecast)
library(dplyr)
library(lubridate)
require(forecast)
library(ggplot2)

# Data Source: https://datamarket.com/data/set/22t4/monthly-sunspot-number-zurich-1749-1983#!ds=22t4&display=line

df <- readr::read_csv(
    'data/monthly-sunspot-number-zurich-17.csv',
    col_names = c('mnth', 'no_of_sunspots'),
    col_types = 'cn',
    skip = 1
)
df <- df %>%
    na.omit() %>%
    tidyr::separate(mnth, c('year', 'mnth'), convert = T) %>%
    filter(year >= 1900)

df

spots <- ts(
    data = df$no_of_sunspots,
    # start = c(df$year[1], df$mnth[1]),
    frequency = 12*11
)
head(spots, 24)
forecast::ggtsdisplay(spots, smooth = T, lag.max = 12*12)
fit <- stl(spots, s.window = 11*12, robust = F,l.degree = 1)
plot(fit)

spots.agg = df %>% group_by(year) %>% summarise(avg_spots = mean(no_of_sunspots))
spots.agg = ts(spots.agg$avg_spots, start = 1900, end = 1983)

training <- window(spots.agg, end=1970)
testing <- window(spots.agg, start=1971)

tsdisplay(training)
tsdisplay(log(training))
tsdisplay(diff(training))
tsdisplay(diff(log(training)))
tsdisplay(diff(diff(training),10))

sarima(training,3,1,1,1,1,0,10)
sarima.for(training,3,1,1,1,1,0,10, n.ahead = 15)
lines(testing, col = 'blue', lty = 2, lwd = 2)

fit.arima = auto.arima(ts(training, frequency = 10), trace = T, stepwise = F, d = 1, D = 1,start.P = 1, start.Q = 1)
fit.arima %>%
    forecast(h=15) %>%
    autoplot(showgap=F) +
    forecast::autolayer(ts(testing, start = 8.10, frequency = 10), series = 'Test data', lwd = 1)
fit.arima %>% summary()

forecast::nnetar(training, P = 1, repeats = 20) %>%
    forecast(h=15) %>%
    autoplot(showgap=F) +
    forecast::autolayer(testing, series = 'Test data', lwd = 1)
forecast::nnetar(training, P = 1, repeats = 20) %>%
    forecast(h=15) %>% summary()

RMSE_arima = fit.arima %>% forecast(h=13) %>% as_tibble() %>%
    transmute(rmse = `Point Forecast` - testing) %>%
    mutate(rmse = rmse^2) %>% pull() %>% sum() %>% sqrt()
RMSE_nnet = forecast::nnetar(training, P = 1, repeats = 20) %>% forecast(h=13) %>% as_tibble() %>%
    transmute(rmse = `Point Forecast` - testing) %>%
    mutate(rmse = rmse^2) %>% pull() %>% sum() %>% sqrt()
RMSE_nnet/RMSE_arima


# Trying a log transform
fit.arima = Arima(log(training), order = c(0,1,3),seasonal = list(order=c(0,1,0),period=10))
exponentiate_forecast_obj <- function(A){
    A$mean = exp(A$mean)
    A$upper = exp(A$upper)
    A$lower = exp(A$lower)
    A$x = exp(A$x)
    A
}
fit.arima %>% summary()
fit.arima %>%
    forecast(h=15) %>%
    exponentiate_forecast_obj %>%
    autoplot(showgap=F) +
    forecast::autolayer(testing, series = 'Test data', lwd = 1)
