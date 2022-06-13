library(data.table)
library(forecast)
library(ggplot2)
library(padr)
library(xts)
library(tseries)
library(urca)

data_production <- read.csv("C:/Users/lenovo/Desktop/2022-05-25_production.csv",colClasses=c("Date",rep("numeric",2)))
tail(data_production)
data_production$dateandtime <- paste(data_production$date,data_production$hour)
data_production$dateandtime <- strptime(data_production$dateandtime,format = "%Y-%m-%d %H")

dataxts <- xts(data_production[,"production"], order.by=data_production[,"dateandtime"])
acf(dataxts) #BU KISIM GEREKSÝZ!!!!!!

data_production$dateandtime <- paste(data_production$date,data_production$hour)
data_production[['dateandtime']] <- as.POSIXct(data_production[['dateandtime']],
                                               format = "%Y-%m-%d %H")
data_production <- data_production[,2:4]
data_production <- pad(data_production)


data_production_0 <- data_production %>% filter(hour == 0)
data_production_1 <- data_production %>% filter(hour == 1)
data_production_2 <- data_production %>% filter(hour == 2)
data_production_3 <- data_production %>% filter(hour == 3)
data_production_4 <- data_production %>% filter(hour == 4)
data_production_5 <- data_production %>% filter(hour == 5)
data_production_6 <- data_production %>% filter(hour == 6)
data_production_7 <- data_production %>% filter(hour == 7)
data_production_8 <- data_production %>% filter(hour == 8)
data_production_9 <- data_production %>% filter(hour == 9)
data_production_10 <- data_production %>% filter(hour == 10)
data_production_11 <- data_production %>% filter(hour == 11)
data_production_12 <- data_production %>% filter(hour == 12)
data_production_13 <- data_production %>% filter(hour == 13)
data_production_14 <- data_production %>% filter(hour == 14)
data_production_15 <- data_production %>% filter(hour == 15)
data_production_16 <- data_production %>% filter(hour == 16)
data_production_17 <- data_production %>% filter(hour == 17)
data_production_18 <- data_production %>% filter(hour == 18)
data_production_19 <- data_production %>% filter(hour == 19)
data_production_20 <- data_production %>% filter(hour == 20)
data_production_21 <- data_production %>% filter(hour == 21)
data_production_22 <- data_production %>% filter(hour == 22)
data_production_23 <- data_production %>% filter(hour == 23)


ggplot(data_production_0, aes(x = dateandtime, y=production)) + geom_line(color="pink") + xlab("Date") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")

ggplot(data_production_1, aes(x = dateandtime, y=production)) + geom_line(color="purple") + xlab("Date") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")

ggplot(data_production_2, aes(x = dateandtime, y=production)) + geom_line(color="pink") + xlab("Date") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")

ggplot(data_production_3, aes(x = dateandtime, y=production)) + geom_line(color="pink") + xlab("Date") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")

ggplot(data_production_4, aes(x = dateandtime, y=production)) + geom_line(color="pink") + xlab("Date") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")

ggplot(data_production_5, aes(x = dateandtime, y=production)) + geom_line(color="pink") + xlab("Date") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")

ggplot(data_production_6, aes(x = dateandtime, y=production)) + geom_line(color="purple") + xlab("Date") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")

ggplot(data_production_7, aes(x = dateandtime, y=production)) + geom_line(color="pink") + xlab("Date") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")

ggplot(data_production_8, aes(x = dateandtime, y=production)) + geom_line(color="pink") + xlab("Date") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")

ggplot(data_production_9, aes(x = dateandtime, y=production)) + geom_line(color="pink") + xlab("Date") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")

ggplot(data_production_10, aes(x = dateandtime, y=production)) + geom_line(color="pink") + xlab("Date") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")

ggplot(data_production_11, aes(x = dateandtime, y=production)) + geom_line(color="pink") + xlab("Date") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")

ggplot(data_production_12, aes(x = dateandtime, y=production)) + geom_line(color="pink") + xlab("Date") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")

ggplot(data_production_13, aes(x = dateandtime, y=production)) + geom_line(color="pink") + xlab("Date") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")

ggplot(data_production_14, aes(x = dateandtime, y=production)) + geom_line(color="pink") + xlab("Date") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")

ggplot(data_production_15, aes(x = date, y=production)) + geom_line(color="pink") + xlab("Date") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")

ggplot(data_production_16, aes(x = dateandtime, y=production)) + geom_line(color="pink") + xlab("Date") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")

ggplot(data_production_17, aes(x = date, y=production)) + geom_line(color="pink") + xlab("Date") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")

ggplot(data_production_18, aes(x = date, y=production)) + geom_line(color="pink") + xlab("Date") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")

ggplot(data_production_19, aes(x = dateandtime, y=production)) + geom_line(color="pink") + xlab("Date") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")

ggplot(data_production_20, aes(x = dateandtime, y=production)) + geom_line(color="pink") + xlab("Date") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")

ggplot(data_production_21, aes(x = dateandtime, y=production)) + geom_line(color="pink") + xlab("Date") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")

ggplot(data_production_22, aes(x = dateandtime, y=production)) + geom_line(color="pink") + xlab("Date") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")

ggplot(data_production_23, aes(x = dateandtime, y=production)) + geom_line(color="pink") + xlab("Date") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")


#dataxts_0 <- xts(data_production_0[,"production"], order.by=data_production_0[,"dateandtime"])
#acf(dataxts_0)
#pacf(dataxts_0)

#dataxts_1 <- xts(data_production_1[,"production"], order.by=data_production_1[,"dateandtime"])
#acf(dataxts_1)
#pacf(dataxts_1)

#dataxts_2 <- xts(data_production_2[,"production"], order.by=data_production_2[,"dateandtime"])
#acf(dataxts_2)
#pacf(dataxts_2)

#dataxts_3 <- xts(data_production_3[,"production"], order.by=data_production_3[,"dateandtime"])
#acf(dataxts_3)
#pacf(dataxts_3)

#dataxts_4 <- xts(data_production_4[,"production"], order.by=data_production_4[,"dateandtime"])
#acf(dataxts_4)
#pacf(dataxts_4)

dataxts_5 <- xts(data_production_5[,"production"], order.by=data_production_5[,"dateandtime"])
acf(dataxts_5)
pacf(dataxts_5)


dataxts_6 <- xts(data_production_6[,"production"], order.by=data_production_6[,"dateandtime"])
acf(dataxts_6)
pacf(dataxts_6)

dataxts_7 <- xts(data_production_7[,"production"], order.by=data_production_7[,"dateandtime"])
acf(dataxts_7)
pacf(dataxts_7)

dataxts_8 <- xts(data_production_8[,"production"], order.by=data_production_8[,"dateandtime"])
acf(dataxts_8)
pacf(dataxts_8)

a <- ur.kpss(dataxts_8)
summary(a)

dataxts_8_decomposed <- decompose(x = dataxts_8,type = "multiplicative")
plot(dataxts_8_decomposed)

dataxts_9 <- xts(data_production_9[,"production"], order.by=data_production_9[,"dateandtime"])
acf(dataxts_9)
pacf(dataxts_9)

dataxts_10 <- xts(data_production_10[,"production"], order.by=data_production_10[,"dateandtime"])
acf(dataxts_10)
pacf(dataxts_10)

dataxts_11 <- xts(data_production_11[,"production"], order.by=data_production_11[,"dateandtime"])
acf(dataxts_11)
pacf(dataxts_11)

dataxts_12 <- xts(data_production_12[,"production"], order.by=data_production_12[,"dateandtime"])
acf(dataxts_12)
pacf(dataxts_12)

dataxts_13 <- xts(data_production_13[,"production"], order.by=data_production_13[,"dateandtime"])
acf(dataxts_13)
pacf(dataxts_13)

dataxts_14 <- xts(data_production_14[,"production"], order.by=data_production_14[,"dateandtime"])
acf(dataxts_14)
pacf(dataxts_14)
auto.arima(dataxts_14) #BURAYA ARIMA DENEMESÝ KOYDUM

dataxts_15 <- xts(data_production_15[,"production"], order.by=data_production_15[,"dateandtime"])
acf(dataxts_15)
pacf(dataxts_15)

dataxts_16 <- xts(data_production_16[,"production"], order.by=data_production_16[,"dateandtime"])
acf(dataxts_16)
pacf(dataxts_16)

dataxts_17 <- xts(data_production_17[,"production"], order.by=data_production_17[,"dateandtime"])
acf(dataxts_17)
pacf(dataxts_17)

dataxts_18 <- xts(data_production_18[,"production"], order.by=data_production_18[,"dateandtime"])
acf(dataxts_18)
pacf(dataxts_18)

dataxts_19 <- xts(data_production_19[,"production"], order.by=data_production_19[,"dateandtime"])
acf(dataxts_19)
pacf(dataxts_19)

dataxts_20 <- xts(data_production_20[,"production"], order.by=data_production_20[,"dateandtime"])
acf(dataxts_20)
pacf(dataxts_20)

#dataxts_21 <- xts(data_production_21[,"production"], order.by=data_production_21[,"dateandtime"])
#acf(dataxts_21)
#pacf(dataxts_21)

#dataxts_22 <- xts(data_production_22[,"production"], order.by=data_production_22[,"dateandtime"])
#acf(dataxts_22)
#pacf(dataxts_22)

#dataxts_23 <- xts(data_production_23[,"production"], order.by=data_production_23[,"dateandtime"])
#acf(dataxts_23)
#pacf(dataxts_23)


#SARIMA WITH FREQUENCY 24
data_production_sarima <- read.csv("C:/Users/lenovo/Desktop/2022-05-25_production.csv",colClasses=c("Date",rep("numeric",2)))
data_production_sarima <- pad(data_production_sarima)
data_production_sarima[,date:=as.Date(date)]

n_days <- 2
forecasted_production=tail(data_production_sarima,n_days*24)
forecasted_production[,date:=date+n_days]
forecasted_production[,data_production_sarima:=NA]

# actual production data with forecasted dates
production_with_forecast=rbind(data_production_sarima,forecasted_production)

# create a template for forecast date
forecast_table=data.table(date="2022-05-25",hour=0:23,production=NA)

# ALTERNATIVE 2
# simple sarima model with frequency 24


# order data just in case
production_with_forecast=production_with_forecast[order(date,hour)]
production_series=ts(production_with_forecast[!is.na(production)]$production,frequency=24)

# this can take too long, you may want to run it for every hour and train arima models, 
# it is run with approximation and stepwise to TRUE
sarima_model=auto.arima(production_series,seasonal=T,stepwise=T,approximation=T,trace=T)

# how many days ahead forecast is needed?
forecast_ahead=nrow(forecast_table)
sarima_forecast=forecast(sarima_model,h=forecast_ahead)
# update forecast table - get the latest predictions
forecast_table[,sarima:=tail(sarima_forecast$mean,24)]





