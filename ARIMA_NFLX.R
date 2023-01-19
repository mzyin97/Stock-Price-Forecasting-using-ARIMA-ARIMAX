library(quantmod)
library(forecast)
library(fpp2)
library(binhf)
library(dplyr)
library(urca)
library(tseries)
library(ggplot2)

data.frame(getSymbols("NFLX", from = '2010-09-30', to = '2022-10-01', auto_assign = TRUE, src="yahoo"))
NFLX
NFLX_M <- to.monthly(NFLX)[, 6] # convert to monthly price
NFLX_M
length(NFLX_M)
nflxts <- ts(NFLX_M, frequency = 12)
nflxts
# lag(nflxts, -1)
# nflxreturn <- nflxts / lag(nflxts, -1)
# nflxreturn
autoplot(nflxts)

library(TSstudio)
outofsamplemonths = 24
nflx_split <- ts_split(nflxts, sample.out = outofsamplemonths)
autoplot(nflx_split$train)
autoplot(nflx_split$test)

autoplot(mstl(nflx_split$train))
seasonalsplit <- seasonal(mstl(nflx_split$train))
seasadjsplit <- seasadj(mstl(nflx_split$train))

##### seasonal 
autoplot(seasonalsplit)
seasonalsplit_bc <- BoxCox(seasonalsplit+30, BoxCox.lambda(seasonalsplit+30))
tsdisplay(seasonalsplit_bc)
nsdiffs(seasonalsplit_bc)
ndiffs(diff(seasonalsplit_bc,lag = 12))
tsdisplay(diff(seasonalsplit_bc,lag = 12))
acf(diff(seasonalsplit_bc, 12), lag.max = 40)
pacf(diff(seasonalsplit_bc, 12), lag.max = 40)

# fit1
seasonalfit1 <- Arima(seasonalsplit+30, order = c(0,0,0), seasonal = c(1,1,0), lambda = 'auto', method = 'ML')
seasonalfit1
checkresiduals(seasonalfit1)
# (0,0,0)(1,1,0)
# AIC=-747.89   AICc=-747.78   BIC=-742.51

# fit2
seasonalfit2 <- Arima(seasonalsplit+30, order = c(0,0,0), seasonal = c(2,1,0), lambda = 'auto', method = 'ML')
seasonalfit2
checkresiduals(seasonalfit2)
# (0,0,0)(2,1,0)
# AIC=-749.26   AICc=-749.03   BIC=-741.19

# fit3
seasonalfit3 <- Arima(seasonalsplit+30, order = c(1,0,0), seasonal = c(1,1,0), lambda = 'auto', method = 'ML')
seasonalfit3
checkresiduals(seasonalfit3)
# (1,0,0)(1,1,0)
# AIC=-752.15   AICc=-751.92   BIC=-744.07

# auto
seasonalauto <- auto.arima(seasonalsplit+30, lambda = 'auto', method = 'ML')
seasonalauto
checkresiduals(seasonalauto)
# (1,0,0)(0,1,0)
# AIC=-612.98   AICc=-612.75   BIC=-604.9


#### seasadj split
autoplot(seasadjsplit)
tsdisplay(BoxCox(seasadjsplit, BoxCox.lambda(seasadjsplit)))
ndiffs(BoxCox(seasadjsplit, BoxCox.lambda(seasadjsplit)))
nsdiffs(BoxCox(seasadjsplit, BoxCox.lambda(seasadjsplit)))

tsdisplay(diff(BoxCox(seasadjsplit, BoxCox.lambda(seasadjsplit))))
acf(diff(BoxCox(seasadjsplit, BoxCox.lambda(seasadjsplit))), lag.max = 40)
pacf((diff(BoxCox(seasadjsplit, BoxCox.lambda(seasadjsplit)))), lag.max = 40)
# ma(3)

# fit1
seasadjfit1 <- Arima(seasadjsplit, order = c(0,1,3), seasonal = c(0,0,2), lambda = 'auto')
seasadjfit1
checkresiduals(seasadjfit1)
# (0,1,3)(0,0,2)
# AIC=122.85   AICc=123.85   BIC=142.36

# fit2
seasadjfit2 <- Arima(seasadjsplit, order = c(0,1,3), seasonal = c(2,0,0), lambda = 'auto', include.drift = TRUE)
seasadjfit2
checkresiduals(seasadjfit2)
# (0,1,3)(2,0,0)
# AIC=128.46   AICc=129.46   BIC=147.98

# fit3
seasadjfit3 <- Arima(seasadjsplit, order = c(1,1,3), seasonal = c(0,0,2), lambda = 'auto', include.drift = TRUE)
seasadjfit3
checkresiduals(seasadjfit3)
# (1,1,3)(0,0,2)
# AIC=124.2   AICc=125.49  BIC=146.5

# auto
seasadjauto = auto.arima(seasadjsplit, lambda = 'auto')
seasadjauto
checkresiduals(seasadjauto)
# (0,1,0)(0,0,0)
# AIC=134.33   AICc=134.43   BIC=139.9


### combine

seasonalforecast <- forecast(seasonalfit1, n = outofsamplemonths)

seasadjforecast <- forecast(seasadjfit1, n = outofsamplemonths)

addforecast <- seasonalforecast$mean - 30 + seasadjforecast$mean

accuracy(addforecast, x = nflx_split$test)

# S1, SA1, RMSE = 132.04, MAPE = 32.25
# S1, SA2, RMSE = 155.48, MAPE = 42.51
# S1, SA3, RMSE = 132.73, MAPE = 32.61
# S1, SA_AUTO, RMSE = 282.99, MAPE = 77.51
# S2, SA1, RMSE = 132.04, MAPE = 32.25
# S2, SA2, RMSE = 155.48, MAPE = 42.51
# S2, SA3, RMSE = 132.73, MAPE = 32.61
# S2, SA_AUTO, RMSE = 282.99, MAPE = 77.51
# S3, SA1, RMSE = 282.99, MAPE = 77.51

# Conclusion: SA1 gives lowest RMSE and MAPE
# choose S1 for simplicity -- (0,0,0)(1,1,0)

# seasonal: ARIMA(0,0,0)(1,1,0)
# seasadj: ARIMA(0,1,3)(0,0,2)

# write.csv(addforecast,"/Users/yinmingze/Desktop/qf603/group project/nflxtest.csv")


fitted <- seasadjfit1$fitted + seasonalfit1$fitted - 30
nflx_split$train

# write.csv(fitted,"/Users/yinmingze/Desktop/qf603/group project/nflxfitted.csv")


# netflix price on 30-9-2023
a <- forecast(seasonalfit1, h = 36)

b <- forecast(seasadjfit1, h = 36)

oneyearforward <- a$mean - 30 + b$mean
oneyearforward

accuracy(fitted, x = nflx_split$train)

