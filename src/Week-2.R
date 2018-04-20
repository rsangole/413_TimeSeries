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
    start = c(df$year[1], df$mnth[1]),
    frequency = 12
)
head(spots, 24)
forecast::ggtsdisplay(spots, smooth = T, lag.max = 20)

dcomp.a <- decompose(spots, type = 'additive')
plot(dcomp.a)

dcomp.m <- decompose(spots, type = 'multiplicative')
plot(dcomp.m)


# Yearly ------------------------------------------------------------------
df.yearly <- df %>%
    group_by(year) %>%
    summarise(avg_sunspots = mean(no_of_sunspots))

df.yearly

spots.yr <- ts(data = df.yearly$avg_sunspots, frequency = 11, start=1900)
plot(spots.yr, main = 'Average count of sunspots', type='b')
forecast::ggtsdisplay(spots.yr, smooth = T, lag.max = 20)

dcomp.yr.a <- decompose(spots.yr, type = 'additive')
plot(dcomp.yr.a, type='b')

dcomp.yr.m <- decompose(spots.yr, type = 'multiplicative')
plot(dcomp.yr.m)

# Compare the models

resids <- tibble(resid = as.vector(scale(dcomp.yr.a$random,center = T,scale = T)), type = 'additive') %>% bind_rows(tibble(resid = as.vector(scale(dcomp.yr.m$random,center = T,scale = T)), type = 'multiplicative')) %>% na.omit

lattice::densityplot(~resid, groups=type, resids, auto.key=T)
lattice::qqmath(~resid|type, resids, type=c('p','r'))

# Seasonally adjusted plots
spots.yr.SA = spots.yr - dcomp.yr.a$seasonal
ts.plot(spots.yr, spots.yr.SA,col=c('black','red'),main='Black = original, Red = seasonally adjusted')

stl(spots.yr, s.window = 'periodic') %>% plot

#Use stl
fit <- stl(spots.yr, s.window = 11, robust = T)
plot(fit)
plot(forecast(fit, h = 12))

#
plot(spots.yr)
lines(ma(spots.yr,order = 11),col='blue')
lines(ma(spots.yr,order = 5),col='red')
lines(ma(spots.yr,order = 17),col='red',lty=2)


# Section 6.3
# https://www.otexts.org/fpp/6/3

spots.yr

## Using decompose()
dcomp.yr.a <- decompose(spots.yr, type = 'additive')
plot(dcomp.yr.a, type='b')

## Doing it manually
trend <- ma(spots.yr, order = 11)
detrend <- spots.yr - trend

# Making sure decomp() and manual way give the same results...
plot(as.numeric(trend),type='l',lwd=4)
lines(as.numeric(dcomp.yr.a$trend), col = 'red',lty=3,lwd=4)
# Yes, world makes sense.

tibble(detrend = detrend) %>%
    mutate(grps= rep(x = 1:11,length.out=84)) %>%
    group_by(grps) %>%
    summarise(avg_detrend = mean(detrend, na.rm = T)) %>%
    mutate(from_decomp = dcomp.yr.a$seasonal[1:11]) -> seasonal_df
seasonal_df

leftovers <- detrend - rep(seasonal_df$avg_detrend, length.out=84)

# Making sure decomp() and manual way give the same results...
plot(as.numeric(leftovers),type='l',lwd=4)
lines(as.numeric(dcomp.yr.a$random), col = 'red',lty=3,lwd=4)
# Yes, world makes sense.
