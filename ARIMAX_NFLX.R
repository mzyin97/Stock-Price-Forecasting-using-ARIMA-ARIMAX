library(quantmod)
library(forecast)
library(fpp2)
library(binhf)
library(dplyr)
library(urca)
library(tseries)

nflxfactors <- read.csv('nflx_factor_v2.csv')
nflxfactors_ts <- ts(nflxfactors, frequency = 12, start = c(2012,9))
nflxfactors_ts

library(TSstudio)
outofsamplemonths = 24
nflxfactors_split <- ts_split(nflxfactors_ts, sample.out = outofsamplemonths)
nflxfactors_split$train
nflxfactors_split$test
nflxfactors_split$train[, 'Log.Return']
nflxfactors_split$train[, c(3,4,5,6)]


autoplot(nflxfactors_split$train[, 'Log.Return'])
returns <- nflxfactors_split$train[, 'Log.Return']
ev_to_sales <- nflxfactors_split$train[, 'EV.to.Sales']
sales_growth <- (nflxfactors_split$train[, 'sales.growth'])
ep_ratio <- (nflxfactors_split$train[, 'E.P.Ratio'])
roce_log <- (nflxfactors_split$train[, 'ROCE'])

autoplot(diff(ev_to_sales))
autoplot(diff(sales_growth))
autoplot(ep_ratio)
autoplot(diff(roce_log))
ndiffs(nflxfactors_split$train[, 'Log.Return'])
ndiffs(ev_to_sales)
ndiffs(sales_growth)
ndiffs(ep_ratio)
ndiffs(roce_log)

return_d1 <- diff(nflxfactors_split$train[, 'Log.Return'])
evtosales_d1 <- diff(ev_to_sales)
salesgrowth_d1 <- diff(sales_growth)
epratio_d1 <- diff(ep_ratio)
rocelog_d1 <- diff(roce_log)

autoplot(return_d1)

########## manual ARIMAX fitting ###########
ols <- lm(return_d1 ~ evtosales_d1 + salesgrowth_d1 + epratio_d1 + rocelog_d1)
ols

res <- ols$residuals
res_ts <- ts(res, frequency = 12)

autoplot(res_ts)
res_bc <- BoxCox(res_ts + 0.5, 
                 BoxCox.lambda(res_ts + 0.5)
                 )

nsdiffs(res_bc)
ndiffs(res_bc)

acf(res_bc)
pacf(res_bc)
# MA(3)

fit <- Arima(returns + 0.5, 
              order = c(2,1,3),
              seasonal = c(0,0,0), 
              xreg = nflxfactors_split$train[, c(3,4,5,6)],
              lambda = 'auto'
              )
fit
checkresiduals(fit)
forecast <- forecast(fit, 
                     xreg = nflxfactors_split$test[, c(3,4,5,6)]
                     )$mean - 0.5

# accuracy via RMSE & MAPE
accuracy(exp(forecast), x = exp(nflxfactors_split$test[, 'Log.Return']
                                )
         )

accuracy(exp(fit$fitted - 0.5), x = exp(nflxfactors_split$train[, 'Log.Return']
                                        )
         )



test <- nflxfactors_split$test[, c(3,4,5,6)]

test23 <- ts(test[c(12,13,14,15,16,17,18,19,20,21,22,23,24),], 
             frequency = 12,
             start = c(2022,10)
)


trainingforecast <- fit$fitted - 0.5
testingforecast <- forecast

extendforecast <- forecast(fit,  
                           xreg = rbind.xts(test, test23)
)$mean - 0.5
extendforecast































############################################


seasonalres <- seasonal(mstl(res_ts))
seasadjres <- seasadj(mstl(res_ts))

##### seasonal
autoplot(seasonalres)
seasonalbc <- BoxCox(seasonalres + 0.15, BoxCox.lambda(seasonalres + 0.15))
tsdisplay(seasonalbc)
nsdiffs(seasonalbc)
ndiffs(diff(seasonalbc, lag = 12))

tsdisplay(diff(diff(seasonalbc, lag = 12)))
acf(diff(diff(seasonalbc, lag = 12)), lag.max = 40)
pacf(diff(diff(seasonalbc, lag = 12)), lag.max = 40)

seasonalauto = auto.arima(seasonalres + 0.15, lambda = 'auto')
seasonalauto
checkresiduals(seasonalauto)

seasonalfit1 = Arima(seasonalres + 0.15, order = c(0,1,1), seasonal = c(1,1,2), lambda = 'auto', include.drift = TRUE)
seasonalfit1
checkresiduals(seasonalfit1)

seasonalfit2 = Arima(seasonalres + 0.15, order = c(0,1,1), seasonal = c(3,1,2), lambda = 'auto', include.drift = TRUE)
seasonalfit2
checkresiduals(seasonalfit2)
# fail

# ARIMA(0,1,1)(1,1,2) without drift

#### seasadj
autoplot(seasadjres)
seasadjbc <- BoxCox(seasadjres+0.4, BoxCox.lambda(seasadjres+0.4))
tsdisplay(seasadjbc)
nsdiffs(seasadjbc)
ndiffs(seasadjbc)
acf(seasadjbc, lag.max = 40)
pacf(seasadjbc, lag.max = 40)

seasadjauto = auto.arima(seasadjres+0.4, lambda = 'auto')
seasadjauto
checkresiduals(seasadjauto)
# (1,0,2)(0,0,2)
# AICC = 352.57

seasadjfit1 = Arima(seasadjres+0.4, order = c(2,0,3), seasonal = c(0,0,2), lambda = 'auto', include.constant = TRUE)
seasadjfit1
checkresiduals(seasadjfit1)
# AIC=346.92   AICc=349.01   BIC=370

seasadjfit2 = Arima(seasadjres+0.4, order = c(2,0,3), seasonal = c(0,0,1), lambda = 'auto', include.constant = TRUE)
seasadjfit2
checkresiduals(seasadjfit2)
# AIC=354.88   AICc=356.53   BIC=375.39

seasadjfit3 = Arima(seasadjres+0.4, order = c(2,0,3), seasonal = c(0,0,3), lambda = 'auto', include.constant = TRUE)
seasadjfit3
checkresiduals(seasadjfit3)
# AIC=348.25   AICc=350.84   BIC=373.9

# (2,0,3)(0,0,2)

##### combine

seasonalforecast <- forecast(seasonalfit1, n = outofsamplemonths)
seasadjforecast <- forecast(seasadjfit1, n = outofsamplemonths)
addforecast <- seasonalforecast$mean - 0.15 + seasadjforecast$mean - 0.4


seasonalforecast$mean
seasadjforecast$mean
addforecast_ts <- ts(addforecast, frequency = 12, start = c(2020,10), end = c(2022,9))
addforecast_ts
nflxfactors_split$test[, 'Log.Return']


#### testing set accuracy test
accuracy(exp(addforecast_ts), x = exp(nflxfactors_split$test[, 'Log.Return']))


#### training set accuracy test
fitted <- seasonalfit1$fitted - 0.15 + seasadjfit1$fitted - 0.4
fitted_ts <- ts(fitted, frequency = 12, start = c(2012,9), end = c(2020,9))
fitted_ts
accuracy(exp(fitted_ts), x = exp(nflxfactors_split$train[, 'Log.Return']))

# write.csv(exp(fitted_ts),"/Users/yinmingze/Desktop/Singapore Management University/QF603 Project Group - Arima_Main/arimaxtrain_v1.csv")
# write.csv(exp(addforecast_ts),"/Users/yinmingze/Desktop/Singapore Management University/QF603 Project Group - Arima_Main/arimaxtest_v1.csv")

seasonal36 <- forecast(seasonalfit1, h = outofsamplemonths+12)
seasadj36 <- forecast(seasadjfit1, h = outofsamplemonths+12)
seasonal36$mean
seasadj36$mean
forecast36 <- seasonal36$mean - 0.15 + seasadj36$mean - 0.4
forecast36

write.csv(forecast36,"/Users/yinmingze/Desktop/Singapore Management University/QF603 Project Group - Arima_Main/arimax36_v1.csv")


