library(tidyquant)
library(magrittr)
library(forecast)
library(dplyr)
sprices <-

    tq_get(x = 'S',
           get = 'stock.prices',
           from = "2016-03-30",
           to = "2018-03-30") %>%
    select(date, price = adjusted)
sprices

sprices %<>% mutate(date_numeric = as.numeric(date))

linFit <- lm(formula = price ~ date, data = sprices)
linFit

sprices %<>% rownames_to_column('id') %>% mutate(id = as.numeric(id))
linFit2 <- lm(formula = price ~ id, data = sprices)
linFit2

linFit3 <- lm(price ~ date_numeric, sprices)
linFit3

nextMonth <-
    tibble(date = seq.Date(
        from = as.Date('2018-04-01'),
        to = as.Date('2018-05-01'),
        by = '1 day'
    ))
nextMonth %<>% rownames_to_column('id') %>% mutate(id = as.numeric(id) +
                                                       536)
nextMonth$linFit_datemodel = predict(linFit, nextMonth)
nextMonth$linFit_rowIDmodel = predict(linFit2, nextMonth)
nextMonth <-
    reshape2::melt(nextMonth, id = c('id', 'date')) %>% rename(type = variable, price =
                                                                   value)

sprices %<>%
    mutate(type = 'Data') %>%
    bind_rows(nextMonth)

sprices %>%
    ggplot(aes(x = date, y = price)) +
    geom_smooth(
        method = 'lm',
        se = F,
        size = .3,
        lty = 2
    ) +
    # geom_line(aes(y=price_linFit2), col='black', size=1)+
    geom_line(aes(col = type), size = 1) +
    theme_bw() +
    scale_x_date(date_breaks = '2 month') +
    labs(
        x = 'date',
        y = 'stock price (USD)',
        title = 'Daily stock price for Sprint (S)',
        caption = 'Blue dotted line - lm-fit using ggplot2::geom_smooth'
    )

# plot(forecast(object = ts(sprices$price)),xlim=c(400,520))



cdata <- feather::read_feather('to_rahul.feather')
cdata %<>% add_count(esn)
cdata <-
    cdata %>% filter(n > 150) %>% dplyr::select(-n) %>% nest(-esn)
cdata
# df1 <- cdata$data[[30]]
# df1
# plot_ts(df1)

plot_ts <- function(df) {
    # plot.new()
    inds <-
        seq(head(df$devicetimestamp, 1),
            length.out = nrow(df),
            by = 'day')
    df.ts <-
        ts(
            data = df$avg_imp,
            start = c(year(inds[1]), as.numeric(format(inds[1], "%j"))),
            frequency = 365
        )
    plot(df.ts)
}

walk(.x = cdata$data,  ~ plot_ts(.x))

df1.decomposed <- decompose(df1.ts)
plot(df1.decomposed)
plot(df1.ts - df1.decomposed$seasonal)
fit <- HoltWinters(df1.ts)
fit
plot(fit)
r <- forecast(fit, h = 20)
plot(r)
acf(x = r$residuals[2:288])
