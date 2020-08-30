library(ggplot2)
library(dplyr)
#library(tidyr)
library(lubridate)



desktop<- "C:/Users/Aditya Gupta/Desktop"
setwd(desktop)
fileUrl<- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

if(!file.exists("./CourseData")){
        dir.create("./CourseData")
        download.file(fileUrl,destfile="./CourseData/Dataset.zip")
        unzip("./CourseData/Dataset.zip",exdir="./CourseData")
  
}

Data<- read.csv("./CourseData/activity.csv")



StepsPerDay <- tapply(Data$steps,Data$date,sum,na.rm=TRUE)
#hist(StepsPerDay,col = "steelblue1",breaks = 15)
#hist(StepsPerDay,col = "steelblue1",breaks = 20)
hist(StepsPerDay,col = "steelblue1",breaks = 12)
MeanSteps <- mean(StepsPerDay)
MedianSteps <- median(StepsPerDay)
AvgSteps <- tapply(Data$steps,Data$interval,mean,na.rm=TRUE)
TimeInterval <- rownames(AvgSteps)
plot(y = AvgSteps, x= TimeInterval,type="l",xlab= "5-min interval")
title(main="Time series plot of the average number of steps taken")
AvgSteps[(which.max(AvgSteps))]
sum(is.na(Data))
mean(is.na(Data))
df = data.frame(Data)

for (i in 1:nrow(df)){
       
       if(is.na(df[i,"steps"])){
             
          df[i,"steps"] <- as.integer(AvgSteps[as.character(df[i,"interval"])]) 
       }
     
}
StepsPerDay2 <- tapply(df$steps,df$date,sum)
hist(StepsPerDay2,col = "steelblue1",breaks = 12,
     main="StepsPerDay after imputing missing data")
MeanSteps2 <- mean(StepsPerDay2)
MedianSteps2 <- median(StepsPerDay2)
df <- transform(df,date = ymd(date))
df["Day"] <- wday(df$date)
for (i in 1:nrow(df)){
  
  if((df[i,"Day"] == 1 | df[i,"Day"]==7)){
    
    df[i,"Day"] <- "weekend" 
  }
  else
    
    df[i,"Day"] <- "weekday"
  
}

AvgSteps2 <- with(df,tapply(steps,list(Day,interval),mean))
TimeInterval <- colnames(AvgSteps2)
par(mfrow=c(1,2),mar = c(4,2,1,2))
plot(y = AvgSteps2["weekday",], x= TimeInterval,type="l",xlab= "5-min interval",
     ,main="AvgSteps on Weekdays",col="steelblue1")
plot(y = AvgSteps2["weekend",], x= TimeInterval,type="l",xlab= "5-min interval",
     ,main="AvgSteps on Weekend",col = "steelblue1")
