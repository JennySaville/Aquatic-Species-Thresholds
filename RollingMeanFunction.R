# Functions to calculate rolling mean, max, and range for serial data of daily
# temperature statistics from data loggers. # Calculations are done over 3, 5, 7,
# 14, 21, and 30 day windows

# For these functions data need to be in long format and contain columns labeled:
### SITEYR, DATE, DAILY_MEAN, DAILY_MAX, DALIY_MIN
library(plyr)
library(reshape2)
library(dplyr)
library(xts)
library(zoo)
library(lubridate)

rollingmeanfunction <- function(year.subset){  
  site.list <- as.character(unique(year.subset$SITEYR))
  holder <- data.frame(SITEYR = character(), N = numeric(), MaxMove_3D_Avg = numeric(), MaxMove_5D_Avg = numeric(), MaxMove_7D_Avg = numeric()
                       , MaxMove_14D_Avg = numeric(), MaxMove_21D_Avg = numeric(), MaxMove_30D_Avg = numeric(), MaxMove_3D_Max = numeric()
                       , MaxMove_5D_Max = numeric(), MaxMove_7D_Max = numeric(), MaxMove_14D_Max = numeric(), MaxMove_21D_Max = numeric()
                       , MaxMove_30D_Max = numeric(), MaxMove_3D_DRange = numeric(), MaxMove_5D_DRange = numeric(), MaxMove_7D_DRange = numeric()
                       , MaxMove_14D_DRange = numeric(), MaxMove_21D_DRange = numeric(), MaxMove_30D_DRange = numeric())
  
  for (i in 1:length(site.list)) {
    site.temp <- site.list[i]
    site.temp.sub <- subset(year.subset, year.subset$SITEYR == site.temp)
    daily.range <- site.temp.sub$DAILY_MAX - site.temp.sub$DAILY_MIN #Daily range (max-min)
    x <- ndays(site.temp.sub$DATE)
    zoo.mean <- zoo(site.temp.sub$DAILY_MEAN, as.Date(site.temp.sub$DATE)) #zoo converts dataframe to timeseries object
    zoo.max <- zoo(site.temp.sub$DAILY_MAX, as.Date(site.temp.sub$DATE))    
    zoo.daily.range <- zoo(daily.range, as.Date(site.temp.sub$DATE))
    
    # Max daily mean
    moving.mean.3 <- rollmean(na.omit(zoo.mean), 3, align = "center") #Rolling average with time frame of 3 days, calculation is center based
    max.moving.mean.3 <- max(moving.mean.3)
    
    moving.mean.5 <- rollmean(na.omit(zoo.mean), 5, align = "center")
    max.moving.mean.5 <- max(moving.mean.5)
    
    moving.mean.7 <- rollmean(na.omit(zoo.mean), 7, align = "center")
    max.moving.mean.7 <- max(moving.mean.7)
    
    moving.mean.14 <- rollmean(na.omit(zoo.mean), 14, align = "center")
    max.moving.mean.14 <- max(moving.mean.14)
    
    moving.mean.21 <- rollmean(na.omit(zoo.mean), 21, align = "center")
    max.moving.mean.21 <- max(moving.mean.21)
    
    moving.mean.30 <- rollmean(na.omit(zoo.mean), 30, align = "center")
    max.moving.mean.30 <- max(moving.mean.30)
    
    # Max daily max
    moving.max.3 <- rollmax(na.omit(zoo.max), 3, align = "center")
    max.moving.max.3 <- max(moving.max.3)
    
    moving.max.5 <- rollmax(na.omit(zoo.max), 5, align = "center")
    max.moving.max.5 <- max(moving.max.5)
    
    moving.max.7 <- rollmax(na.omit(zoo.max), 7, align = "center")
    max.moving.max.7 <- max(moving.max.7)
    
    moving.max.14 <- rollmax(na.omit(zoo.max), 14, align = "center")
    max.moving.max.14 <- max(moving.max.14)
    
    moving.max.21 <- rollmax(na.omit(zoo.max), 21, align = "center")
    max.moving.max.21 <- max(moving.max.21)
    
    moving.max.30 <- rollmax(na.omit(zoo.max), 30, align = "center")
    max.moving.max.30 <- max(moving.max.30)
    
    # Max daily range
    moving.daily.range.3 <- rollmax(na.omit(zoo.daily.range), 3, align = "center")
    max.moving.daily.range.3 <- max(moving.daily.range.3) 
    
    moving.daily.range.5 <- rollmax(na.omit(zoo.daily.range), 5, align = "center")
    max.moving.daily.range.5 <- max(moving.daily.range.5)
    
    moving.daily.range.7 <- rollmax(na.omit(zoo.daily.range), 7, align = "center")
    max.moving.daily.range.7 <- max(moving.daily.range.7)
    
    moving.daily.range.14 <- rollmax(na.omit(zoo.daily.range), 14, align = "center")
    max.moving.daily.range.14 <- max(moving.daily.range.14)
    
    moving.daily.range.21 <- rollmax(na.omit(zoo.daily.range), 21, align = "center")
    max.moving.daily.range.21 <- max(moving.daily.range.21)
    
    moving.daily.range.30 <- rollmax(na.omit(zoo.daily.range), 30, align = "center")
    max.moving.daily.range.30 <- max(moving.daily.range.30)
    
    results <- data.frame(SITEYR = site.temp, N = x 
                          , MaxMove_3D_Avg = max.moving.mean.3, MaxMove_5D_Avg = max.moving.mean.5, MaxMove_7D_Avg = max.moving.mean.7
                          , MaxMove_14D_Avg = max.moving.mean.14, MaxMove_21D_Avg = max.moving.mean.21, MaxMove_30D_Avg = max.moving.mean.30
                          , MaxMove_3D_Max = max.moving.max.3, MaxMove_5D_Max = max.moving.max.5, MaxMove_7D_Max = max.moving.max.7
                          , MaxMove_14D_Max = max.moving.max.14, MaxMove_21D_Max = max.moving.max.21, MaxMove_30D_Max = max.moving.max.30
                          , MaxMove_3D_DRange = max.moving.daily.range.3, MaxMove_5D_DRange = max.moving.daily.range.5
                          , MaxMove_7D_DRange = max.moving.daily.range.7, MaxMove_14D_DRange = max.moving.daily.range.14
                          , MaxMove_21D_DRange = max.moving.daily.range.21, MaxMove_30D_DRange = max.moving.daily.range.30)
    
    holder <- rbind(holder, results) 
  }
  return(holder)
}