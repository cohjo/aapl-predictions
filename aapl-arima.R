library(forecast)

aapl <- read.csv("aapl.csv", header = TRUE)
aapl$Date <- as.Date(aapl$Date, format = '%Y-%m-%d')
aaplimp <- data.frame(aapl[,c(1,5)])
lastdate <- nrow(aaplimp)
#View(aapl)
#View(aaplimp)

plot(aaplimp$Date, aaplimp$Close,
     main =  'Apple Stock Prices',
     xlab = 'Date',
     ylab = 'Closing Price($)',
     type = 'l',
     col = 'blue')

# Find last date for training data
for(i in 1:lastdate)
{
  if((aaplimp$Date[i] <= as.Date("2019-11-01")) &
     (aaplimp$Date[i + 1] > as.Date("2019-11-01")))
  {
    trlast = i
    break
  }
}

# Number of trading days in a year is 252
# training data is 5 times the test data
ntrain <- which(aaplimp$Date == '2019-11-01') - which(aaplimp$Date == '2019-06-12')
trfirst <- which(aaplimp$Date == '2019-06-12')

# ACF and PACF to check stationarity
acf(aaplimp$Close[trfirst:trlast])
pacf(aaplimp$Close[trfirst:trlast])

aaplarima <- auto.arima(aaplimp$Close,
                       stepwise = FALSE,
                       approximation = FALSE)
aaplarima

aaplforecast <- forecast(aaplarima, h = ntest)
plot(aaplforecast, 
     ylim = c(100, 300)
)

View(aaplforecast)

aaplPrediction <- aaplforecast[["mean"]]
aaplPrediction

#write.csv(aaplPrediction, file = "aaplPrediction.csv")


retvec <- c()
for (i in 1:trlast-1)
{
  
  retvec[i] <- (aaplimp$Close[i+1] - aaplimp$Close[i])/aaplimp$Close[i]
  
}

# Defining necessary variables and formulas along with calculating
# the simulation of GBM
mu <- mean(retvec)
sigma <- sd(retvec)
aaplgbm <- c()

startdate <- trlast + 1
ndays <- lastdate - trlast
aaplgbm[1] <- aaplimp$Close[trlast]*exp(mu-.5*sigma^2 + sigma*rnorm(1))
for (i in 2:ndays)
{
  aaplgbm <- c(aaplgbm, aaplgbm[i-1]*exp(mu-.5*sigma^2 + sigma*rnorm(1)))
}

for (i in 2:ndays)
{
  aaplgbm[i] <- aaplgbm[i-1]*exp(mu-.5*sigma^2 + sigma*rnorm(1))
}

# Plot of real data vs GBM forecast
plot(aaplimp$Date[startdate:lastdate], aaplgbm,
     type = 'l',
     main = 'Monte Carlo GBM Forecast of aapl for April 2019',
     xlab = 'Date',
     ylab = 'Closing Price',
     ylim = c(100, 300),
     col = 'blue')
lines(aaplimp$Date[startdate:lastdate], aaplimp$Close[startdate:lastdate],
      type = 'l',
      col = 'red')