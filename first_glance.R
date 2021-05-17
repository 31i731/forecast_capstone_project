library(ggplot2)
library(data.table)
library(forecast)
library(tidyverse)
library(lubridate)
library(dplyr) 
library(tseries)
library(nortest)


# 'sun' - sensor data, otherwise the energy produced

building_2_sun <- readRDS("~/capstone/Building 2 sun.rds")
building_2 <- readRDS("~/capstone/Building 2.rds")

building_5_sun <- readRDS("~/capstone/Building 5 sun.rds")
building_5 <- readRDS("~/capstone/Building 5.rds")

building_8_sun <- readRDS("~/capstone/Building 8 sun.rds")
building_8 <- readRDS("~/capstone/Building 8.rds")

setDT(building_2_sun)
setDT(building_2)
setDT(building_5_sun)
setDT(building_5)
setDT(building_8_sun)
setDT(building_8)

setnames(building_2_sun, "1302611", "sun")
setnames(building_2, "1490017", "energy_produced")
setnames(building_5_sun, "1328370", "sun")
setnames(building_5, "1328347", "energy_produced")
setnames(building_8_sun, "1302169", "sun")
setnames(building_8, "1498763", "energy_produced")

#ggplot(data = building_2_sun, aes(x = timestamp, y = sun)) + 
#  geom_line(color = "#00AFBB", size = 1) +
#  theme_minimal()

# ----------------------------------- Removing outliers

building_2 <- building_2[energy_produced < 100 & energy_produced >= 0]

# ----------------------------------- Aggregating

############# Daily data aggregated
building_2_sun_daily <- building_2_sun[, .(daily_sum=sum(sun)),
                                         by=.(Day=floor_date(timestamp, "days"))]
building_2_daily <- building_2[, .(daily_sum=sum(energy_produced)),
                                 by=.(Day=floor_date(timestamp, "days"))]

building_5_sun_daily <- building_5_sun[, .(daily_sum=sum(sun)),
                                         by=.(Day=floor_date(timestamp, "days"))]
building_5_daily <- building_5[, .(daily_sum=sum(energy_produced)),
                                 by=.(Day=floor_date(timestamp, "days"))]

building_8_sun_daily <- building_8_sun[, .(daily_sum=sum(sun)),
                                         by=.(Day=floor_date(timestamp, "days"))]
building_8_daily <- building_8[, .(daily_sum=sum(energy_produced)),
                                by=.(Day=floor_date(timestamp, "days"))]

############# Weekly data aggregated
building_2_sun_weekly <- building_2_sun[, .(weekly_sum=sum(sun)),
                                       by=.(Week=floor_date(timestamp, "weeks"))]
building_2_weekly <- building_2[, .(weekly_sum=sum(energy_produced)),
                               by=.(Week=floor_date(timestamp, "weeks"))]

building_5_sun_weekly <- building_5_sun[, .(weekly_sum=sum(sun)),
                                       by=.(Week=floor_date(timestamp, "weeks"))]
building_5_weekly <- building_5[, .(weekly_sum=sum(energy_produced)),
                               by=.(Week=floor_date(timestamp, "weeks"))]

building_8_sun_weekly <- building_8_sun[, .(weekly_sum=sum(sun)),
                                       by=.(Week=floor_date(timestamp, "weeks"))]
building_8_weekly <- building_8[, .(weekly_sum=sum(energy_produced)),
                               by=.(Week=floor_date(timestamp, "weeks"))]

############# Monthly data aggregated
building_2_sun_monthly <- building_2_sun[, .(monthly_sum=sum(sun)),
               by=.(Month=floor_date(timestamp, "months"))]
building_2_monthly <- building_2[, .(monthly_sum=sum(energy_produced)),
               by=.(Month=floor_date(timestamp, "months"))]

building_5_sun_monthly <- building_5_sun[, .(monthly_sum=sum(sun)),
               by=.(Month=floor_date(timestamp, "months"))]
building_5_monthly <- building_5[, .(monthly_sum=sum(energy_produced)),
               by=.(Month=floor_date(timestamp, "months"))]

building_8_sun_monthly <- building_8_sun[, .(monthly_sum=sum(sun)),
              by=.(Month=floor_date(timestamp, "months"))]
building_8_monthly <- building_8[, .(monthly_sum=sum(energy_produced)),
              by=.(Month=floor_date(timestamp, "months"))]


#------------------------ Time series and decomposing

####### Monthly

building_2_monthly_ts = ts(building_2_monthly[, "monthly_sum"], frequency = 12, start = c(2016,8))
autoplot(building_2_monthly_ts)
autoplot(decompose(building_2_monthly_ts, type="additive"))
#autoplot(decompose(energy_produced - decompose(energy_produced)$seasonal))

building_5_monthly_ts = ts(building_5_monthly[, "monthly_sum"], frequency = 12, start = c(2016,8))
autoplot(building_5_monthly_ts)
autoplot(decompose(building_5_monthly_ts, type="additive"))
#autoplot(decompose(energy_produced - decompose(energy_produced)$seasonal))

building_8_monthly_ts = ts(building_8_monthly[, "monthly_sum"], frequency = 12, start = c(2016,8))
autoplot(building_8_monthly_ts)
autoplot(decompose(building_8_monthly_ts, type="additive"))
#autoplot(decompose(energy_produced - decompose(energy_produced)$seasonal))


####### Weekly

building_2_weekly_ts = ts(building_2_weekly[, "weekly_sum"], frequency = 365.25/7, start = c(2016,8))
autoplot(building_2_weekly_ts)
autoplot(decompose(building_2_weekly_ts, type="additive"))
#autoplot(decompose(energy_produced - decompose(energy_produced)$seasonal))

building_5_weekly_ts = ts(building_5_weekly[, "weekly_sum"], frequency = 365.25/7, start = c(2016,8))
autoplot(building_5_weekly_ts)
autoplot(decompose(building_5_weekly_ts, type="additive"))
#autoplot(decompose(energy_produced - decompose(energy_produced)$seasonal))

building_8_weekly_ts = ts(building_8_weekly[, "weekly_sum"], frequency = 365.25/7, start = c(2016,8))
autoplot(building_8_weekly_ts)
autoplot(decompose(building_8_weekly_ts, type="additive"))

#------------------------ Pre-processing
str(building_2_sun_monthly)
summary(building_2_sun_monthly)
boxplot(building_2_sun_monthly$monthly_avg)

str(building_2_monthly)
summary(building_2_monthly)
boxplot(building_2_monthly$monthly_avg)

str(building_5_sun_monthly)
summary(building_5_sun_monthly)
boxplot(building_5_sun_monthly$monthly_avg)

str(building_5_monthly)
summary(building_5_monthly)
boxplot(building_5_monthly$monthly_avg)

str(building_8_sun_monthly)
summary(building_8_sun_monthly)
boxplot(building_8_sun_monthly$monthly_avg)

str(building_8_monthly)
summary(building_8_monthly)
boxplot(building_8_monthly$monthly_avg)

adf.test(building_2_monthly_ts)
adf.test(building_5_monthly_ts)
adf.test(building_8_monthly_ts)

## Which data are we working on (what is it? How is it structured? What does it contain?)

# Time series data, which was aggregated into daily/weekly/monthly data. 
# All the datasets here have the same structure (first column - datetime, second column - number,
# units of sun detected/produced)

## What problem are we trying to address?
# predict amount of sun units needed to produce enough energy

## What are our next steps?
# Pre-processing steps specific to time series data, like stationarity, variance, and
# time series' outliers

############# Modelling

#### Monthly

# building 2
autoplot(building_2_monthly_ts)

building_2_monthly_ts.fc <- HoltWinters(building_2_monthly_ts, beta = F, gamma = F)
building_2_monthly_ts.fc$fitted

plot(building_2_monthly_ts.fc)
plot(forecast(building_2_monthly_ts.fc, h=12))

# Buiding 5
autoplot(building_5_monthly_ts)

building_5_monthly_ts.fc <- HoltWinters(building_5_monthly_ts, beta = F, gamma = F)
building_5_monthly_ts.fc$fitted

plot(building_5_monthly_ts.fc)
plot(forecast(building_5_monthly_ts.fc, h=12))

# Building 8
autoplot(building_8_monthly_ts)

building_8_monthly_ts.fc <- HoltWinters(building_8_monthly_ts, beta = F, gamma = F)
building_8_monthly_ts.fc$fitted

plot(building_8_monthly_ts.fc)
plot(forecast(building_8_monthly_ts.fc, h=12))


#### Weekly

autoplot(building_2_weekly_ts)

building_2_weekly_ts.fc <- HoltWinters(building_2_weekly_ts, beta = F, gamma = F)
building_2_weekly_ts.fc$fitted

plot(building_2_weekly_ts.fc)
plot(forecast(building_2_weekly_ts.fc, h=365.25/7))

# Buiding 5
autoplot(building_5_weekly_ts)

building_5_weekly_ts.fc <- HoltWinters(building_5_weekly_ts, beta = F, gamma = F)
building_5_weekly_ts.fc$fitted

plot(building_5_weekly_ts.fc)
plot(forecast(building_5_weekly_ts.fc, h=365.25/7))

# Building 8
autoplot(building_8_weekly_ts)

building_8_weekly_ts.fc <- HoltWinters(building_8_weekly_ts, beta = F, gamma = F)
building_8_weekly_ts.fc$fitted

plot(building_8_weekly_ts.fc)
plot(forecast(building_8_weekly_ts.fc, h=365.25/7))

## AR forecasting

(building_8_weekly_ts.fc <- ar(building_8_weekly_ts))
plot(forecast(building_8_weekly_ts.fc, h=365.25/7))

## Holt forecasting

(building_8_weekly_ts.fc <- holt(building_8_weekly_ts))
building_8_weekly_ts.fc$fitted

plot(building_8_weekly_ts.fc)
plot(forecast(building_8_weekly_ts.fc))

## arima / sarima

ggAcf(building_8_weekly_ts)

training <- window(building_8_weekly_ts, start=c(2016,8),end= c(2018,11))
testing <- window(building_8_weekly_ts, start=c(2018,12) )

arima_optimal <- auto.arima(training)
arima_optimal

plot(forecast(arima_optimal, h=365.25/7))

library(astsa)

sarima_forecast <- sarima.for(training, n.ahead = length(testing),
                              p=0,d=1,q=1, P=1, D=1, Q=0)



