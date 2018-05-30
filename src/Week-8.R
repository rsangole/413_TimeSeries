library(magrittr)
library(forecast)
library(dplyr)
library(lubridate)
library(forecast)
library(ggplot2)
library(purrr)
library(lattice)
library(sweep)
library(quantmod)
library(vars)

getSymbols('KHC', src = 'yahoo')
getSymbols('K', src = 'yahoo')

K <- K['2015-07::']
KHC <- KHC['2015-07::']

kkhc <- tail(xts::merge.xts(KHC$KHC.Adjusted,K$K.Adjusted),-2)

plot(kkhc, multi.panel = T, main = 'Kraft Heinz Co (KHC) and Kellog Company (K)')

lattice::xyplot(KHC.Adjusted~K.Adjusted, data.frame(kkhc), type=c('smooth','p'), col.l ='red', lwd=2, asp = 1, main = paste0('Pearsons Corr Coeff: ', round(cor(kkhc)[1,2],3)))

old.par = par()
par('mfrow'=c(1,1))
astsa::acf2(kkhc$KHC.Adjusted, max.lag = 120)
astsa::acf2(kkhc$K.Adjusted, max.lag = 120)

ndiffs(kkhc$KHC.Adjusted)
ndiffs(kkhc$K.Adjusted)

astsa::acf2(diff(kkhc$KHC.Adjusted), max.lag = 120)
astsa::acf2(diff(kkhc$KHC.Adjusted,2), max.lag = 120)
astsa::acf2(diff(kkhc$K.Adjusted), max.lag = 120)
astsa::acf2(diff(kkhc$K.Adjusted,2), max.lag = 120)

plot(diff(kkhc$KHC.Adjusted,1), main = 'KHC (Black) and K (Red) - Differenced by 1')
lines(diff(kkhc$K.Adjusted,1), col = 'red', lwd = 2, on = NA)

kkhc.diff <- diff(kkhc,na.pad = F)

vars::VARselect(kkhc, lag.max = 10)

varfit1 <- vars::VAR(kkhc, p = 1)
summary(varfit1)
coef(varfit1)

preds <- predict(varfit1, n.ahead = 30)
par('mar'=c(1.2,1.2,1.7,1.2))
plot(preds)

lm(kkhc$KHC.Adjusted~kkhc$K.Adjusted) %>% resid %>% as.numeric() %>% tseries::adf.test()

tseries::po.test(kkhc)

