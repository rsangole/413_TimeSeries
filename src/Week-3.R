library(magrittr)
library(forecast)
library(dplyr)
library(lubridate)
library(forecast)
library(ggplot2)
library(purrr)
library(lattice)

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

fit_ets <- function(ts, model_name) {
    ts %>% ets(model = model_name)
}
post_process_ets <- function(ets, xlim = c(1970, 1976)) {
    autoplot(ets, xlim = xlim) %>% ggsave(filename = paste0('img/',ets$method,'.png'),device = 'png', width = 8, height = 3)
    checkresiduals(ets)
    summary(ets)
}
model_table <- tibble(
    model_name = c('ZZZ', 'AAA', 'ANN'),
    model_descrip = c('Auto Select', 'Holt-Winters', 'Naive')) %>%
    mutate(model = map(.x = model_name, ~ fit_ets(ft.ts, model_name = .x))) %>%
    bind_cols(
        map_df(.$model, ~sw_glance(.x))
    ) %>%
    mutate(forecast = map(.x = model, .f = ~forecast(.x, h=12)))

model_table

walk(model_table$forecast, ~post_process_ets(.x))

model_table %>%
    select(model_descrip, RMSE, MASE, MAPE) %>%
    reshape2::melt(id.vars = 'model_descrip', variable.name  = 'performance_param') %>%
    ggplot(aes(x=model_descrip, y= value, fill = model_descrip))+
    geom_bar(stat='identity',position=position_dodge())+
    theme_bw()+
    facet_grid(performance_param~.,scales = 'free_y')

model_table %>%
    select(model_descrip, RMSE, MASE, MAPE) %>%
    reshape2::melt(id.vars = 'model_descrip', variable.name  = 'performance_param') %>%
    xyplot(value~performance_param, groups = model_descrip, auto.key = T, type='b', .)
