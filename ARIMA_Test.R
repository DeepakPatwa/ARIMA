library('ggplot2')
library('forecast')
library('tseries')

getwd()
daily_data <- read.csv('day.csv', header=TRUE, stringsAsFactors=FALSE)

daily_data$Date = as.Date(daily_data$dteday)

ggplot(daily_data, aes(Date, cnt)) + geom_line() + scale_x_date('month')  + ylab("Daily Bike Checkouts") +
  xlab("")

count_ts = ts(daily_data[, c('cnt')])

daily_data$clean_cnt = tsclean(count_ts)

ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt)) + ylab('Cleaned Bicycle Count')

daily_data$cnt_ma = ma(daily_data$clean_cnt, order=7) # using the clean count with no outliers
daily_data$cnt_ma30 = ma(daily_data$clean_cnt, order=30)


ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt, colour = "Counts")) +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma30, colour = "Monthly Moving Average"))  +
  ylab('Bicycle Count')

count_ma = ts(na.omit(daily_data$cnt_ma), frequency=30)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)# decomposed into trend, seasonal and reminder data
plot(deseasonal_cnt)# after seasonality removal 

adf.test(count_ma, alternative = "stationary")

Acf(count_ma, main='')

Pacf(count_ma, main='')

count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')

auto.arima(deseasonal_cnt, seasonal=FALSE)

fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')

library(caret)
fit <- nnetar(deseasonal_cnt)
plot(forecast(fit,h=60))
points(1:length(x),fitted(fit),type="l",col="green")

hold <- window(ts(deseasonal_cnt), start=500)

fit_no_holdout = nnetar(deseasonal_cnt[-c(500:725)])

fcast_no_holdout <- forecast(fit_no_holdout,h=225)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt))

fit2 = arima(deseasonal_cnt, order=c(1,1,7)) #we can use (7,1,1) or (7,1,7) but best is (1,1,7) cuz it gives smallest AIC value

fit2

tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')


fcast <- forecast(fit2, h=30)
plot(fcast)

hold <- window(ts(deseasonal_cnt), start=500)

fit_no_holdout = arima(ts(deseasonal_cnt[-c(500:725)]), order=c(1,1,7))

fcast_no_holdout <- forecast(fit_no_holdout,h=225)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt))

fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
fit_w_seasonality

seas_fcast <- forecast(fit_w_seasonality, h=30)
plot(seas_fcast)