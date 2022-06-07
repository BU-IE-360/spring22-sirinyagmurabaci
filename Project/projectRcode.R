library(data.table)
library(forecast)
library(ggplot2)
library(padr)

data_production <- read.csv("C:/Users/lenovo/Desktop/2022-05-31_production.csv",colClasses=c("Date",rep("numeric",2)))
tail(data_production)
data_weather <- read.csv("C:/Users/lenovo/Desktop/2022-05-31_weather.csv")
data_weather <- data.table(data_weather)

data_production$dateandtime <- paste(data_production$date,data_production$hour)
data_production$dateandtime <- strptime(data_production$dateandtime,format = "%Y-%m-%d %H")
dataxts <- xts(data_production[,"production"], order.by=data_production[,"dateandtime"])

plot(data_production$production[11113:11280],
     xlab = "Hour",
     ylab = "Production (MW)", 
     main = "Hourly Solar Power Production Data between 16/05/22 and 22/05/22",
     type = 'l')

ggplot(data_production, aes(x = date, y=production)) + geom_line(color="blue") + xlab("Date") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")

data_production <- read.csv("C:/Users/lenovo/Desktop/2022-06-02_production.csv",colClasses=c("Date",rep("numeric",2)))
tail(data_production)
data_weather <- read.csv("C:/Users/lenovo/Desktop/2022-06-02_weather.csv")
data_weather <- data.table(data_weather)

data_production$dateandtime <- paste(data_production$date,data_production$hour)
data_production[['dateandtime']] <- as.POSIXct(data_production[['dateandtime']],
                                               format = "%Y-%m-%d %H")
data_production <- data_production[,3:4]

data_production <- pad(data_production)
data_weather$extvar <- paste(data_weather$variable, data_weather$lat, data_weather$lon, sep = "_")
data_weather <- dcast(data_weather, date + hour ~ extvar, value.var = "value")
data_weather$production <- NA
data_weather$production[1:11640] <- data_production$production

data_weather$lag24<- rep(NA)
data_weather$lag24[25:11664]<-data_weather$production[1:11640]
data_weather$lag48<- rep(NA)
data_weather$lag48[49:11688]<-data_weather$production[1:11640]
data_weather$lag1<- rep(NA)
data_weather$lag1[2:11641]<-data_weather$production[1:11640]
data_weather$lag2<- rep(NA)
data_weather$lag2[3:11642]<-data_weather$production[1:11640]
data_weather$lag3 <- rep(NA)
data_weather$lag3[4:11643]<-data_weather$production[1:11640]
data_weather$lag4 <- rep(NA)
data_weather$lag4[5:11644]<-data_weather$production[1:11640]
data_weather$lag30<- rep(NA)
data_weather$lag30[31:11670]<-data_weather$production[1:11640]

data_weather$cloud_mean <- (data_weather$CLOUD_LOW_LAYER_36.25_33 +data_weather$CLOUD_LOW_LAYER_36.25_33.25+data_weather$CLOUD_LOW_LAYER_36.25_33.5+data_weather$CLOUD_LOW_LAYER_36.5_33
                            + data_weather$CLOUD_LOW_LAYER_36.5_33.25 + data_weather$CLOUD_LOW_LAYER_36.5_33.5 + data_weather$CLOUD_LOW_LAYER_36.75_33 + data_weather$CLOUD_LOW_LAYER_36.75_33.25 
                            +data_weather$CLOUD_LOW_LAYER_36.75_33.5)/9
data_weather$DSWRF_mean <- (data_weather$DSWRF_36.25_33 +data_weather$DSWRF_36.25_33.25+data_weather$DSWRF_36.25_33.5+data_weather$DSWRF_36.5_33
                            + data_weather$DSWRF_36.5_33.25 + data_weather$DSWRF_36.5_33.5 + data_weather$DSWRF_36.75_33 + data_weather$DSWRF_36.75_33.25 
                            +data_weather$DSWRF_36.75_33.5)/9
data_weather$REL_HUMIDITY_mean <- (data_weather$REL_HUMIDITY_36.25_33 +data_weather$REL_HUMIDITY_36.25_33.25+data_weather$REL_HUMIDITY_36.25_33.5+data_weather$REL_HUMIDITY_36.5_33
                                   + data_weather$REL_HUMIDITY_36.5_33.25 + data_weather$REL_HUMIDITY_36.5_33.5 + data_weather$REL_HUMIDITY_36.75_33 + data_weather$REL_HUMIDITY_36.75_33.25 
                                   +data_weather$REL_HUMIDITY_36.75_33.5)/9
data_weather$TEMP_mean <- (data_weather$TEMP_36.25_33 +data_weather$TEMP_36.25_33.25+data_weather$TEMP_36.25_33.5+data_weather$TEMP_36.5_33
                           + data_weather$TEMP_36.5_33.25 + data_weather$TEMP_36.5_33.5 + data_weather$TEMP_36.75_33 + data_weather$TEMP_36.75_33.25 
                           +data_weather$TEMP_36.75_33.5)/9

pro <-data_weather[!(data_weather$hour==0 | data_weather$hour==1 | data_weather$hour==2 | data_weather$hour==3 | data_weather$hour==4),]
pro <-pro[!(pro$hour==20 | pro$hour==21 | pro$hour==22 | pro$hour==23),]

ggplot(pro, aes(x = date, y=production)) + geom_line(color="purple")

model2 <-  lm( formula = production ~ hour +lag1 +lag24+lag48+ lag2 +lag3+ cloud_mean + TEMP_mean +DSWRF_mean, pro)
summary(model2)
checkresiduals(model2)

five <- data_weather[data_weather$hour==5,]
six <-data_weather[data_weather$hour==6,]
seven <-data_weather[data_weather$hour==7,]
eight <-data_weather[data_weather$hour==8,]
nine <-data_weather[data_weather$hour==9,]
ten <-data_weather[data_weather$hour==10,]
eleven <-data_weather[data_weather$hour==11,]
twelve <-data_weather[data_weather$hour==12,]
thirteen <-data_weather[data_weather$hour==13,]
fourteen <-data_weather[data_weather$hour==14,]
fifteen <-data_weather[data_weather$hour==15,]
sixteen <-data_weather[data_weather$hour==16,]
seventeen <-data_weather[data_weather$hour==17,]
eighteen <-data_weather[data_weather$hour==18,]
nineteen <-data_weather[data_weather$hour==19,]

model3 <-  lm( formula = production ~  lag1 +lag24+lag48+ cloud_mean + TEMP_mean +DSWRF_mean, six)
summary(model3)
checkresiduals(model3)
model4 <- lm( formula = production ~  lag1 +lag24+lag48+ cloud_mean + TEMP_mean +DSWRF_mean, fifteen)
summary(model4)
checkresiduals(model4)

model1 <-  lm( formula = production ~ hour +lag48  +lag24+lag1+lag2+lag3+lag30+cloud_mean + TEMP_mean  , data_weather)
summary(model1)
checkresiduals(model1)

#ARIMA YORUMU????
data_production <- read.csv("C:/Users/lenovo/Desktop/2022-05-31_production.csv",colClasses=c("Date",rep("numeric",2)))
tail(data_production)
data_weather <- read.csv("C:/Users/lenovo/Desktop/2022-05-31_weather.csv")
data_weather <- data.table(data_weather)

data_production$dateandtime <- paste(data_production$date,data_production$hour)
data_production$dateandtime <- strptime(data_production$dateandtime,format = "%Y-%m-%d %H")
dataxts <- xts(data_production[,"production"], order.by=data_production[,"dateandtime"])

auto.arima(dataxts)

data_production_final <- read.csv("C:/Users/lenovo/Desktop/actual production data.csv",colClasses=c("Date",rep("numeric",2)))
data_production_final$time <- 0:215

final_forecast <- read.csv("C:/Users/lenovo/Desktop/final forecast data.csv",colClasses=c("Date",rep("numeric",2)))
final_forecast$time <- 0:215

summary(model1)
ggplot() + 
  geom_line(data = data_production_final, aes(x = time, y = data_production_final$production,color = "Actual")) + 
  geom_line(data = final_forecast, aes(x = time, y = final_forecast$production,color = "Predicted")) + xlab("Hour") +
  ylab("Production (MW)") +
  ggtitle("Hourly Solar Power Production Data between 01/02/21 and 21/05/22")

library(knitr)

statistics <- function(actual, forecasted){
  n=length(actual)
  error = actual - forecasted
  mean = mean(actual)
  sd = sd(actual)
  bias = sum(error) / sum(actual)
  mad = sum(abs(error)) / n
  wmape = mad / mean
  l = data.frame(n, mean, sd, bias, mad, wmape)
  colnames(l) <- c("N", "Mean", "Standard Deviation", "Bias", "MAD", "WMAPE")
  return(l)
}

kable(statistics(data_production_final$production, final_forecast$production),
      caption = "Statistics for Fitted Hourly Solar Power Production According to Linear Regression Model", align = 'c')










