library(magrittr)
library(forecast)
library(dplyr)
library(lubridate)
library(forecast)
library(ggplot2)
library(purrr)
library(lattice)
library(sweep)

# Data Source: https://datamarket.com/data/set/22qp/quarterly-production-of-gas-in-australia-million-megajoules-includes-natural-gas-from-july-1989-mar-1956-sep-1994#!ds=22qp&display=line

df <- readr::read_csv(
    'data/quarterly-production-of-gas-in-a.csv',
    col_names = c('YQ', 'gas'), col_types = 'cn',skip = 1,n_max = 155) %>%
    na.omit() %>%
    tidyr::separate(YQ, c('year', 'qtr'), sep = 'Q', convert = T, remove = F) %>%
    mutate(YQ = yq(YQ),
           type=ifelse(YQ>'1989-06-01', 'NatGasAlso','GasOnly')) %>%
    filter(year >=1970)
df

ft.ts <- ts(data = df$gas, start = c(df$year[1], df$qtr[1]), frequency = 4)
head(ft.ts, 24)
ggtsdisplay(ft.ts,
            smooth = T,
            lag.max = 20,
            main = 'Quarterly Production of gas in Aus')

df %>%
    ggplot(aes(x=YQ, y=gas))+
    geom_point(aes(color=type))+
    geom_line(aes(color=type))+
    theme_bw()+
    labs(title = 'Quarterly Production of gas in Aus')


fit_ets <- function(ts, model_name = 'ZZZ', ...) {
    ts %>% ets(model = model_name, ...)
}
fit_autoarima <- function(ts, ...){
    ts %>% auto.arima(trace = T,...)
}

etsfit <- fit_ets(ft.ts)
arimafit <- fit_autoarima(ft.ts)

par(mfrow=c(1,2))
forecast(etsfit, h = 8) %>% plot()
forecast(arimafit, h = 8) %>% plot()
par(mfrow=c(1,1))

summary(etsfit)
summary(arimafit)

sw_glance(etsfit) %>% bind_rows(sw_glance(arimafit))

checkresiduals(etsfit)
checkresiduals(arimafit)


# Test Train Split
train.ts <- window(ft.ts, end = c(1993,1))
test.ts <- window(ft.ts, start = c(1993,2))

etsfit_train <- fit_ets(train.ts)
arimafit_train <- fit_autoarima(train.ts)
sw_glance(etsfit_train) %>% bind_rows(sw_glance(arimafit_train,test))
checkresiduals(etsfit_train)
checkresiduals(arimafit_train)

forecast(etsfit_train, h=6) %>% autoplot() + forecast::autolayer(test.ts, series = 'Test data', lwd = 1) -> p1
forecast(arimafit_train, h=6) %>% autoplot() + forecast::autolayer(test.ts, series = 'Test data', lwd = 1) -> p2
gridExtra::grid.arrange(p1,p2)

summary(etsfit_train)
summary(arimafit_train)

results <- accuracy(forecast(etsfit_train, h=6), test.ts) %>% rbind(accuracy(forecast(arimafit_train, h=6), test.ts))
rownames(results) <- c('ETS Training','ETS Test','ARIMA Training','ARIMA Test')
results





# Fur Sales -------------------------------------------------------------------------

# Data: https://datamarket.com/data/set/22y2/fur-sales-skunk-h-b-co-1850-1911#!ds=22y2&display=line

df <- readr::read_csv( 'data/fur-sales-skunk-h-b-co-18501911.csv',
    col_names = c('year', 'fur'), col_types = 'nn',skip = 1, n_max = 62) %>%
    na.omit()

fur.ts <- ts(df$fur, start = 1851)
ggtsdisplay(fur.ts)

train <- ts(df$fur[1:52], frequency = 10)
test <-ts(df$fur[53:61], frequency = 10, start = c(6,3))
stlfit <- stl(train, s.window = 9, s.degree = 1)
forecast(stlfit, h = 8) %>% autoplot() + autolayer(test, series = 'Test data', lwd = 1)

etsfit_train <- fit_ets(train, )
arimafit_train <- fit_autoarima(train)
sw_glance(etsfit_train) %>% bind_rows(sw_glance(arimafit_train,test))
checkresiduals(etsfit_train)
checkresiduals(arimafit_train)


forecast(etsfit_train, h=8) %>% autoplot() + autolayer(test, series = 'Test data', lwd = 1) -> p1
forecast(arimafit_train, h=6) %>% autoplot() + autolayer(test, series = 'Test data', lwd = 1) -> p2
gridExtra::grid.arrange(p1,p2)

summary(etsfit_train)
summary(arimafit_train)

results <- accuracy(forecast(etsfit_train, h=8), test) %>% rbind(accuracy(forecast(arimafit_train, h=8), test))
rownames(results) <- c('ETS Training','ETS Test','ARIMA Training','ARIMA Test')
results
