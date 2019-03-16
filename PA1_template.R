
##title: "Reproducible Research: Peer Assessment 1"
##output: 
  ##html_document:
    ##keep_md: true

### Loading and preprocessing the data
### Make sure to load the proper libraries 
### Make sure that you are in the right directory

library(ggplot2)

###Warning: package 'ggplot2' was built under R version 3.1.3

library(plyr)

###Warning: package 'plyr' was built under R version 3.1.3

activity <- read.csv("activity.csv", header = TRUE)

##Processing the Data

StepsPerDay <- tapply(activity$steps, activity$date, sum)
StepsPerDay

##Make a histogram of the total number of steps taken each day

hist(StepsPerDay, xlab = "Number of Steps", main = "Histogram: Steps per Day")

##Calculate and report the mean and median of the total number of steps taken per day

MeanPerDay <- mean(StepsPerDay, na.rm = TRUE)
MedianPerDay <- median(StepsPerDay, na.rm = TRUE)

### What is the mean steps per day = 10766.19 and median steps per day = 10765.

# (Q2) What is the average daily acitivity pattern?

StepsPerInterval <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
plot(as.numeric(names(StepsPerInterval)),
     StepsPerInterval,
     xlab = "Interval",
     ylab = "Steps",
     main = "Average Daily Activity Pattern", 
     type = "l")

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

maxInterval <- names(sort(StepsPerInterval, decreasing = TRUE) [1])
maxSteps <- sort(StepsPerInterval, decreasing = TRUE)[1]

###The interval with maximum activity is 83, at 206.169 steps ## Q3) Impute missing values

NA.vals <- sum(is.na(activity$steps))
NA.vals

### [1] 2304

##Devise a stratergy for filling in all of the missing values in the dataset

StepPerInterval <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
# split activity data by interval
activity.split <- split(activity, activity$interval)
# fill in missing data for each interval
for (i in 1:length(activity.split)){
        activity.split[[i]]$steps[is.na(activity.split[[i]]$steps)] <- StepsPerInterval[i]
}

activity.imputed <- do.call("rbind", activity.split)
activity.imputed <- activity.imputed[order(activity.imputed$date) ,]

##Make a histogram of the total number of steps taken 
##and calcualte and report the mean and median total number of steps taken per day       

StepsPerDay.imputed <- tapply(activity.imputed$steps, activity.imputed$date, sum)
hist(StepsPerDay.imputed, xlab = "Number of Steps", main = "Histogram: Steps per Day (Imputed data)")

MeanPerDay.imputed <- mean(StepsPerDay.imputed, na.rm = TRUE)
MedianPerDay.imputed <- median(StepsPerDay.imputed, na.rm = TRUE)

MeanPerDay.imputed
### [1] 10766.19

MedianPerDay.imputed
### [1] 10766.19

## (Q4) Are there differences in acitivity patterns between weekdays and weekends?

###Create a new factor variable in the dataset with two levels - “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

activity.imputed$day <- ifelse(weekdays(as.Date(activity.imputed$date)) == "Saturday" | 
                                       weekdays(as.Date(activity.imputed$date)) == "Sunday", "weekend", "weekday")

##Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all 
##weekday days or weekend days.

## Calculate average steps per interval for weekends
StepsPerInterval.weekend <- tapply(activity.imputed[activity.imputed$day == "weekend" ,]$steps, 
                                   activity.imputed[activity.imputed$day == "weekend" ,]$interval, mean, na.rm = TRUE)

## Calculate average steps per interval for weekdays
StepsPerInterval.weekday <- tapply(activity.imputed[activity.imputed$day == "weekday" ,]$steps, 
                                   activity.imputed[activity.imputed$day == "weekday" ,]$interval, mean, na.rm = TRUE)

### Set a 2 panel plot
par(mfrow=c(1,2))

###Plot weekday activity
plot(as.numeric(names(StepsPerInterval.weekday)),
     StepsPerInterval.weekday,
     xlab = "Interval",
     ylab = "Steps",
     main = "Activity Pattern (Weekdays)",
     type = "l")


###Plot weekday activity
plot(as.numeric(names(StepsPerInterval.weekend)),
     StepsPerInterval.weekend,
     xlab = "Interval",
     ylab = "Steps",
     main = "Activity Pattern (Weekends)",
     type = "l")

## Are there differences in activity patterns between weekdays and weekends?
### Yes, there are differences in the activity patterns as more steps are being taken during the weekends. 
















