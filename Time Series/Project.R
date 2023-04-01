rm(list = ls())

##required r packages
library(IRdisplay)
library(magrittr)
library(tidyverse)
library(scales)
library(gridExtra)
library(forecast)
library(tseries)
library(ggthemes)
theme_set(theme_economist())

##Load helper R functions
getwd()
setwd("/Users/giorgiocavallo/Desktop/GIORGIO")
setwd("./COURSERA/Time Series/Project/")
source("R functions/compare_models_function.R")
source("R functions/sim_random_walk_function.R")
source("R functions/sim_stationary_example_function.R")

print("Loading is completed")

compare.models(n=100)


##simulate random walk
data <- sim.random.walk()
##plot random walk
data %>%
        ggplot(aes(t,X)) + geom_line() + xlab("T") + ylab("X") + ggtitle("Time Series Plot")


#ACF plot
ggAcf(data$X, type = "correlation") + ggtitle("Autocorrelation ACF Plot")

#PACF plot
ggAcf(data$X, type = "partial") + ggtitle("Partical Autocorrelation PACF Plot")


##create 3 time series for example
df <-sim.stationary.example(n=1000)
head(df)
dim(df)
        
g1 <- ggplot(df,aes(x=t,y=X1)) + geom_line() + xlab("T") + ylab("X1") + ggtitle("Nonstationary")
g2 <- ggplot(df,aes(x=t,y=X3)) + geom_line() + xlab("T") + ylab("X3") + ggtitle("Stationary")
grid.arrange(g1,g2)



#ACF plot (look at the ACF for non-stationarity and stationarity time series)
g1 <- ggAcf(df$X1, type = "correlation") + xlab("T") + ylab("X1") + ggtitle("Nonstationary")
g2 <- ggAcf(df$X3, type = "correlation") + xlab("T") + ylab("X3") + ggtitle("Stationary")
grid.arrange(g1,g2)

##perform unit test; non stationarity example has large, non signigicant p-value
adf.test(df$X1)
adf.test(df$X3)

#difference time series to make stationary
diff <- df$X1-lag(df$X1,1)

#plot original and differenced time series
g1 <- ggAcf(df$X1, type="correlation")
g2 <- ggAcf(diff, type="correlation")   ##autocorrelation dies off 
grid.arrange(g1,g2)

##detrend time series to make stationary
detrended <- resid(lm(X2~t,data=df))

##plot original and detrended time series
g1<-ggAcf(df$X2, type="correlation")
g2<-ggAcf(detrended, type="correlation")
grid.arrange(g1,g2)


##Load the data 
getwd()
df_mur <- read.csv("Data/Mass Monthly Unemployment Rate.csv")

head(df_mur)
str(df_mur)
dim(df_mur)

df_mur$DATE <- as.Date(df_mur$DATE)
class(df_mur$DATE)

##checking for stationarity
##check time series plot
ggplot(df_mur,aes(x=DATE,y=MAURN)) + geom_line()


##check ACF plot
ggAcf(df_mur$MAURN, type="correlation")

##check ADF plot
adf.test(df_mur$MAURN)  ##we failto reject the null hp


##fit AR model
ar.model <- auto.arima(df_mur$MAURN, max.d=0,max.q=0,allowdrift=T)
ar.model

##fit MA model
ma.model <- auto.arima(df_mur$MAURN, max.d=0, max.p=0, allowdrift=T)
ma.model

##fit ARMA model
arma.model <- auto.arima(df_mur$MAURN, max.d=0,allowdrift=T)
arma.model

##fit ARIMA model
arima.model <- auto.arima(df_mur$MAURN, allowdrift = T)
arima.model

##Checking the residuals of the Model fit
ar.residual <- resid(ar.model)
ma.residual <- resid(ma.model)
arma.residual <- resid(arma.model)
arima.residual <- resid(arima.model)

##plot PACF plot of each models residuals
ggAcf(ar.residual, type = "partial")
ggAcf(ma.residual, type = "partial")
ggAcf(arma.residual, type = "partial")
ggAcf(arima.residual, type = "partial")

##run the Ljung Box test on the residuals
Box.test(ar.residual,type="Ljung-Box",lag=1)
Box.test(ma.residual,type="Ljung-Box",lag=1)
Box.test(arma.residual,type="Ljung-Box",lag=1)
Box.test(arima.residual,type="Ljung-Box",lag=1)

##Making a forecast for each model



