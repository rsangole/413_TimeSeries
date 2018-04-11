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

spots.yr <- ts(data = df.yearly$avg_sunspots, frequency = 11)
plot(spots.yr)
forecast::ggtsdisplay(spots.yr, smooth = T, lag.max = 20)

dcomp.yr.a <- decompose(spots.yr, type = 'additive')
plot(dcomp.yr.a)

dcomp.yr.m <- decompose(spots.yr, type = 'multiplicative')
plot(dcomp.yr.m)

# Compare the models

resids <- tibble(resid = as.vector(scale(dcomp.yr.a$random,center = T,scale = T)), type = 'additive') %>% bind_rows(tibble(resid = as.vector(scale(dcomp.yr.m$random,center = T,scale = T)), type = 'multiplicative')) %>% na.omit

lattice::densityplot(~resid, groups=type, resids, auto.key=T)
lattice::qqmath(~resid|type, resids, type=c('p','r'))
