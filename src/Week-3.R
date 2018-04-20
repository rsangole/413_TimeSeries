library(magrittr)
library(forecast)
library(dplyr)
library(lubridate)
library(forecast)
library(ggplot2)
library(purrr)

# Data Source: https://datamarket.com/data/set/22ty/monthly-traffic-fatalities-in-ontario-1960-1974#!ds=22ty&display=line

df <- readr::read_csv(
    'data/monthly-traffic-fatalities-in-on.csv',
    col_names = c('mnth', 'fatalities'),
    col_types = 'cn',
    skip = 1
)
df <- df %>%
    na.omit() %>%
    tidyr::separate(mnth, c('year', 'mnth'), convert = T)

df

ft.ts <- ts(
    data = df$fatalities,
    start = c(df$year[1], df$mnth[1]),
    frequency = 12
)
head(ft.ts, 24)
ggtsdisplay(ft.ts,
            smooth = T,
            lag.max = 20,
            main = 'Monthly Traffic Fatalities in Ontario')

fit_ets <- function(ts, model_name, h = 12) {
    ts %>% ets(model = model_name) %>% forecast(h = h)
}
post_process_ets <- function(ets, xlim = c(1970, 1976)) {
    autoplot(ets, xlim = xlim) %>% print
    checkresiduals(ets)
    summary(ets)
}
model_table <- tibble(
    model_name = c('ZZZ', 'AAA', 'AAN', 'ANN'),
    model_descrip = c('Auto Select', 'AAA', 'AAN', 'ANN')) %>%
    mutate(result = map(.x = model_name, ~ fit_ets(ft.ts, model_name = .x)),
           RMSE = map_dbl(.x = result, ~accuracy(.x)[2]),
           MAE  = map_dbl(.x = result, ~accuracy(.x)[3]),
           MPE  = map_dbl(.x = result, ~accuracy(.x)[4]),
           MAPE = map_dbl(.x = result, ~accuracy(.x)[5]),
           MASE = map_dbl(.x = result, ~accuracy(.x)[6]))

model_table

walk(model_table$result,
     ~post_process_ets(.x))
