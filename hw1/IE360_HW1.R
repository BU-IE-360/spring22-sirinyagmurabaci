library(readxl)
library(ggplot2)    
library(ggfortify)  
library(forecast)   
library(xts)        
library(ggcorrplot) 
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(zoo)
library(lubridate)

Sys.setlocale("LC_TIME", "English")

hw1data <- read.csv("C:/Users/lenovo/Desktop/IE360-hw1data.csv")


colnames(hw1data) <- c("Date","Unemployement Rate","USD Exchange Rate(Buying)","Residential Property Price Index")


hw1data$Date <- as.yearmon(x = hw1data$Date)


data_xts <- xts(x = hw1data[-1],order.by = hw1data$Date,frequency = 12)


head(data_xts)

#Graph plots
  ggplot(hw1data,aes(x=Date))+
    geom_line(size=1,color="green",aes(y=`Unemployement Rate`))+
    ggtitle("Time Series of Unemployement Rate",
            subtitle="Between the Years 2014-2022")
  
  ggplot(hw1data,aes(x=Date))+
    geom_line(size=1,color="pink",aes(y=`USD Exchange Rate(Buying)`))+
    ggtitle("Time Series of USD Exchange Rate(Buying)",
            subtitle="Between the Years 2014-2022")
  
  ggplot(hw1data,aes(x=Date))+
    geom_line(size=1,color="purple",aes(y=`Residential Property Price Index`))+
    ggtitle("Time Series of Residential Property Price Index",
            subtitle="Between the Years 2014-2022")
  
  #==============================================
  
  ggplot(hw1data, aes(`Unemployement Rate`))+ggtitle("Density of Producer Price Index (2014 to 2022)")+
    ylab("Density")+xlab("Unemployement Rate")+geom_density(fill="lightyellow")
  
  ggplot(hw1data, aes(`USD Exchange Rate(Buying)`))+ggtitle("Density of USD Exchange Rate(Buying) (2014 to 2022)")+
    ylab("Density")+xlab("USD Exchange Rate(Buying)")+geom_density(fill="lightyellow")
  
  ggplot(hw1data, aes(`Residential Property Price Index`))+ggtitle("Density of Residential Property Price Index (2014 to 2022)")+
    ylab("Density")+xlab("Residential Property Price Index")+geom_density(fill="lightyellow")
  
  #==================================================
  ggcorrplot(corr = cor(data_xts),
             hc.order = TRUE,
             outline.col = "white",
             type = "upper", lab = TRUE,
             title = "Correlation Matrix",
             colors = c("darkred","white","darkgreen"),
             legend.title = "Correlation",
             ggtheme = theme_void)




