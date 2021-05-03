
if (rstudioapi::isAvailable())
  setwd(dirname(rstudioapi::getActiveDocumentContext()[["path"]]))

library(data.table)
library(lubridate)
library(dplyr) 


building_2_sun <- readRDS("./solar_panels/Building 2 sun.rds")
building_2 <- readRDS("./solar_panels/Building 2.rds")

building_5_sun <- readRDS("./solar_panels/Building 5 sun.rds")
building_5 <- readRDS("./solar_panels/Building 5.rds")

building_8_sun <- readRDS("./solar_panels/Building 8 sun.rds")
building_8 <- readRDS("./solar_panels/Building 8.rds")

weather <- fread("./vienna.csv", na.strings = c("No moonrise", "No moonset"))
colSums(is.na(weather))
weather[,location := NULL]
weather[,date_time := as.Date(date_time)]
# impute missing values
weather<-VIM::kNN(weather, variable=c("moonrise", "moonset"), imp_var = F)

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


#------------------------Merged datasets
# --------------- building 2
b2_joined <- left_join(building_2, building_2_sun, 
                       by = c("timestamp" = "timestamp"))
summary(b2_joined)
# aggregate daily

building2 <- b2_joined[, .(daily_total_sun=sum(sun), daily_total_energy=sum(energy_produced)),
                       by=.(Day=floor_date(timestamp, "days"))]
summary(building2)

building2_weather <- left_join(building2, weather, 
                               by = c("Day" = "date_time"))


# --------------- building 5
b5_joined <- left_join(building_5, building_5_sun, 
                       by = c("timestamp" = "timestamp"))
b5_joined <- na.omit(b5_joined) # remove the last day --> bc it has NAs
summary(b5_joined)
# aggregate daily
building5 <- b5_joined[, .(daily_total_sun=sum(sun), daily_total_energy=sum(energy_produced)),
                       by=.(Day=floor_date(timestamp, "days"))]
summary(building5)

building5_weather <- left_join(building5, weather, 
                               by = c("Day" = "date_time"))

# --------------- building 8
b8_joined <- left_join(building_8, building_8_sun, 
                       by = c("timestamp" = "timestamp"))
summary(b8_joined)
# aggregate daily
building8 <- b8_joined[, .(daily_total_sun=sum(sun), daily_total_energy=sum(energy_produced)),
                       by=.(Day=floor_date(timestamp, "days"))]
summary(building8)

building8_weather <- left_join(building8, weather, 
                               by = c("Day" = "date_time"))

