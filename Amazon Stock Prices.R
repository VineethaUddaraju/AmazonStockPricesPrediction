install.packages('readxl')
install.packages('urca')
install.packages('fpp2')
library(readxl)
library(urca)
library(fpp2)
library(forecast)
install.packages('lubridate')
library(lubridate)
library(xts)
library(tseries)

data <- read_excel("ECON Jagruthi/Project/amzn_data.xlsx")
View(data)

data$date = as.Date(data$date, format="%m/%d/%Y")

open = data$open[c(6452:6681)]
close = data$close[c(6452:6681)]
close
high = data$high[c(6452:6681)]
low = data$low[c(6452:6681)]
adj = data$adjusted_close[c(6452:6681)]
volume = data$volume[c(6452:6681)]

dataTS = ts(data = data[c(6452:6681),c(2:7)],start=2023,frequency = 252)
dataTS_f = ts(data = data[c(5193:6681),c(2:7)],start=2018,frequency = 252)


autoplot(dataTS_f[,'close'])+
  xlab('year')+
  ylab('amazon closing stock price')+
  ggtitle('Autoplot of Amazon Closing Stock Price from 2018 to 2023')
autoplot(dataTS[,'close'])+
  xlab('year')+
  ylab('amazon closing stock price')+
  ggtitle('Autoplot of Amazon Closing Stock Price in Year 2023')

closereg1 = tslm(close~open+high+low+adj+volume+trend+season,dataTS)
CV(closereg1)
closereg2 = tslm(close~open+high+low+adj+trend+season,dataTS)
CV(closereg2)
closereg3 = tslm(close~open+high+low+trend+season,dataTS)
CV(closereg3)
closereg4 = tslm(close~open+high+low+trend,dataTS)
CV(closereg4)
closereg5 = tslm(close~open+high+trend,dataTS)
CV(closereg5)
closereg6 = tslm(log(close)~open+high+low,dataTS)
CV(closereg6)
closereg7 = tslm(close~open+low,dataTS)
CV(closereg7)

clreg_final = tslm(close~open+high+low,dataTS)
summary(clreg_final)
checkresiduals(clreg_final)

clreg_final = tslm(close~open+high+low,dataTS_f)
summary(clreg_final)
checkresiduals(clreg_final)

openets = ets(dataTS[,"open"])
summary(openets)
fopen = forecast(openets,h=12)
summary(fopen)

highets = ets(dataTS[,"high"])
summary(highets)
fhigh = forecast(highets,h=12)
summary(fhigh)

lowets = ets(dataTS[,"low"])
summary(lowets)
flow = forecast(lowets,h=12)
summary(flow)

foreCastedData = data.frame(
  open = fopen$mean,
  low = flow$mean,
  high = fhigh$mean
)

fcast.exante = forecast(clreg_final,newdata = foreCastedData)
summary(fcast.exante)

autoplot(dataTS[,"close"])+
  autolayer(fcast.exante)

openets = stlf(dataTS_f[,"open"])
summary(openets)
fopen = forecast(openets,h=12)
summary(fopen)

highets = stlf(dataTS_f[,"high"])
summary(highets)
fhigh = forecast(highets,h=12)
summary(fhigh)

lowets = stlf(dataTS_f[,"low"])
summary(lowets)
flow = forecast(lowets,h=12)
summary(flow)

foreCastedData = data.frame(
  open = fopen$mean,
  low = flow$mean,
  high = fhigh$mean
)

fcast.exante = forecast(clreg_final,newdata = foreCastedData)
summary(fcast.exante)

autoplot(dataTS_f[,"close"])+
  autolayer(fcast.exante)+
  xlab("Year")+
  ylab("Closing Stock Price")+
  ggtitle("Forecast of Closing Price Using Stls and Regression Model")

autoplot(dataTS_f[,'close'])+
  ggtitle("Autoplot of Amazon Closing Stock Price after BoxCox Transformation")

cllam = BoxCox.lambda(dataTS_f[,'close'])
autoplot(BoxCox(dataTS_f[,'close'],lambda = cllam))+
  xlab("Year")+
  ylab("Closing Stock Price")+
  ggtitle("Autoplot of Amazon Closing Stock Price after BoxCox Transformation")

autoplot(log(dataTS_f[,'close']))+
  xlab("Year")+
  ylab("Closing Stock Price")+
  ggtitle("Autoplot of Amazon Closing Stock Price after log Transformation")

ndiffs(dataTS_f[,'close'])
adf.test(log(dataTS_f[,'close']))
close_d1=diff(BoxCox(dataTS_f[,'close'],lambda = cllam))
adf.test(close_d1)
autoplot(close_d1)+
  xlab("Year")+
  ylab("Closing Stock Price")+
  ggtitle("Amazon Close Stock Price Differenced to Order 1 after BoxCox Transformation")
acf(close_d1)
pacf(close_d1)
checkresiduals(close_d1)

model1 = Arima(dataTS_f[,'close'],order=c(1,1,1),lambda = cllam)
summary(model1)
model2 = Arima(dataTS_f[,'close'],order=c(1,1,0),lambda = cllam)
summary(model2)
model3 = Arima(dataTS_f[,'close'],order=c(0,1,1),lambda = cllam)
summary(model3)
model4 = Arima(dataTS_f[,'close'],order=c(2,1,2),lambda = cllam)
summary(model4)

close_arima1 = auto.arima(dataTS_f[,'close'],lambda = cllam)
summary(close_arima1)
fcast_arima1 = forecast(close_arima1,h=12)
summary(fcast_arima1)

autoplot(dataTS_f[,'close'])+
  autolayer(fcast_arima1,PI = F)

