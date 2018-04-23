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


fit_ets <- function(ts, model_name = 'ZZZ') {
    ts %>% ets(model = model_name)
}
fit_autoarima <- function(ts, ...){
    ts %>% auto.arima(trace = T,...)
}

etsfit <- fit_ets(ft.ts)
arimafit <- fit_autoarima(ft.ts)
plot(etsfit)
plot(arimafit)

forecast(etsfit, h = 8) %>% plot
forecast(arimafit, h = 8) %>% plot


post_process_ets <- function(ets, xlim = c(1970, 1976)) {
    autoplot(ets, xlim = xlim) %>% ggsave(
        filename = paste0('img/', ets$method, '.png'),
        device = 'png',
        width = 8,
        height = 3
    )
    checkresiduals(ets)
    summary(ets)
}