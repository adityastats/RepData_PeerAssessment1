#loading and preprocessing the data:

if(!file.exists("activity.zip")){
  download.file(url="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
                destfile="./activity.zip")
}
if(!file.exists("activity.csv")){
  unzip(zipfile = "./activity.zip",exdir = ".")
}  
activity <- read.csv(file = "activity.csv", header = T)
#Preprocessing
activity$date <- as.Date(activity$date,format="%Y-%m-%d")
str(activity)
activityComplete <- activity[complete.cases(activity),]
str(activityComplete)

# Histogram of the total number of steps taken each day
steps_each_day <- with(activityComplete, aggregate(steps~date,FUN=sum))
summary(steps_each_day)
library(ggplot2)
plt <- ggplot(steps_each_day,aes(x=steps))
plt+geom_histogram(binwidth = 1000,fill="cyan",col="black")+
  geom_point(x=mean(steps_each_day$steps),y=0,size=3,col="red",shape=15)+
  geom_point(x=median(steps_each_day$steps),y=0,size=3,col="mistyrose",shape=16)+
  labs(title="Histogram of total steps taken each day",x="Number of Steps",y="Frequency")

# Mean and Median number of steps taken each day
print(paste("Mean : ", round(mean(steps_each_day$steps),3)))
print(paste("Median : ", round(median(steps_each_day$steps),3)))

# average time series plot of the 5-minute interval (x-axis) and the average number of steps taken, 
# averaged across all days (y-axis)
avg_all_days <- with(activityComplete,aggregate(steps~interval,FUN=mean))
avg_all_days$interval <- as.integer(avg_all_days$interval)
ggplot(avg_all_days, aes(interval,steps))+
  geom_line(color='blue')+
  ggtitle("Average Daily Activity Pattern")+
  labs(x="5 Minute Intervals",y="Average Number of Steps")+
  geom_point(x=avg_all_days[which.max(avg_all_days$steps),1],y=0,color="red",shape=17,size=3)
avg_all_days[which.max(avg_all_days$steps),1]

sum(is.na(activity))
head(activity)
head(avg_all_days)

# Imputing the missing steps with avg steps in that interval across all days

activityImputed <- activity  
for (i in 1:length(activityImputed$steps)){
  if(is.na(activityImputed[i,"steps"])){
    missingInterval <- activityImputed[i,"interval"]
    for (j in 1:length(avg_all_days$interval)){
      if(missingInterval==avg_all_days[j,"interval"]){
        activityImputed[i,"steps"] <- avg_all_days[j,"steps"]
        break
      }
    }
  }
}
sum(is.na(activityImputed$steps))

steps_each_day1 <- with(activityImputed, aggregate(steps~date,FUN=sum))
summary(steps_each_day1)
plt <- ggplot(steps_each_day1,aes(x=steps))
plt+geom_histogram(binwidth = 1000,fill="cyan",col="black")+
  geom_point(x=mean(steps_each_day1$steps),y=0,size=3,col="red",shape=15)+
  geom_point(x=median(steps_each_day1$steps),y=0,size=3,col="mistyrose",shape=16)+
  labs(title="Histogram of total steps taken each day",x="Number of Steps",y="Frequency")

# Mean and Median number of steps taken each day
print(paste("Mean : ", round(mean(steps_each_day1$steps),3)))
print(paste("Median : ", round(median(steps_each_day1$steps),3)))

# Detecting weekday and weekend pattern
activityImputed$day.type <- ifelse(weekdays(activityImputed$date) %in% c("Saturday","Sunday"),"Weekend","Weekday")
activityImputed$day.type <- as.factor(activityImputed$day.type)
avg_steps <- with(activityImputed,aggregate(steps~interval+day.type,FUN = mean))
ggplot(avg_steps,aes(x=interval,y=steps,color=day.type))+
  geom_line()+facet_grid(day.type~.)+
  ggtitle("Time Series plot for Activity Patterns between weekdays and weekends")+
  labs(x="5 Minute Interval",y="Average Number of Steps")
