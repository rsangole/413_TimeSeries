library(tidyquant)
library(magrittr)
sprices <-
    tq_get(x = 'S',
           get = 'stock.prices',
           from = "2016-03-30",
           to = "2018-03-30") %>%
    select(date, price = adjusted)
sprices

sprices %>%
    ggplot(aes(x = date, y = price)) +
    geom_line() +
    geom_smooth(method = 'lm', se = F) +
    theme_bw() +
    scale_x_date(date_breaks = '2 month') +
    labs(x = 'date', y = 'stock price (USD)', title = 'Daily stock price for Sprint (S)')

linFit <- lm(formula = price ~ date, data = sprices)

nextMonth <- tibble(date=seq.Date(from = as.Date('2018-04-01'), to = as.Date('2018-05-01'),by = '1 day'))
nextMonth$price = predict(linFit,nextMonth)
nextMonth$type = 'Prediction'

sprices %<>%
    mutate(type='Data') %>%
    bind_rows(nextMonth)

sprices %>%
    ggplot(aes(x = date, y = price)) +
    geom_smooth(method = 'lm', se = F,size=.3) +
    geom_line(aes(col=type),size=1) +
    theme_bw() +
    scale_x_date(date_breaks = '2 month') +
    labs(x = 'date', y = 'stock price (USD)', title = 'Daily stock price for Sprint (S)')
