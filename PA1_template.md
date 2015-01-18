---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data



```r
rawFileDirectory ="C:/Users/Jelly/Documents/GitHub/RepData_PeerAssessment1"
setwd(rawFileDirectory)
getwd()
```

```
## [1] "C:/Users/Jelly/Documents/GitHub/RepData_PeerAssessment1"
```
### 1. Load the data (i.e. read.csv())

```r
activityDataSet <- read.csv("activity.csv")
activityDataSet$date <-as.Date(activityDataSet$date)
colnames(activityDataSet)<-c("steps","date", "Interval")
str(activityDataSet)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ Interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?


```r
intervalMeanActivityDataSet<-aggregate(step~interval,data=activityDataSet, mean,na.rm = TRUE)
```

```
## Error in eval(expr, envir, enclos): object 'interval' not found
```

```r
colnames(intervalMeanActivityDataSet)<-c("Interval", "Mean")
```
### 1. Make a histogram of the total number of steps taken each day

```r
hist(dailyActivityDataSet$steps)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 
### 2. Calculate and report the mean and median total number of steps taken per day

```r
mean(dailyActivityDataSet$steps)
```

```
## [1] 10766.19
```

```r
median(dailyActivityDataSet$steps)
```

```
## [1] 10765
```
## What is the average daily activity pattern?
### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
plot(intervalMeanActivityDataSet$Interval,
     intervalMeanActivityDataSet$Mean,
     type="n",
     main="Time Series Plot of every 5-minute interval",
     xlab = "5-minute intervals",
     ylab = "Average number of steps taken")

lines(intervalMeanActivityDataSet$Interval,
      intervalMeanActivityDataSet$Mean,
      type="l")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 
### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxAverageStepByInterval<- intervalMeanActivityDataSet[which.max(intervalMeanActivityDataSet$Mean),1]

maxAverageStepByInterval
```

```
## [1] 835
```


## Imputing missing values

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
totalMissingValue <-sum(is.na(activityDataSet$steps))
totalMissingValue
```

```
## [1] 2304
```

### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
missingActivityDataSet <- merge(activityDataSet,
                                intervalMeanActivityDataSet,
                                by = "Interval",
                                sort= FALSE)

missingActivityDataSet$steps[is.na(missingActivityDataSet$steps)] <- missingActivityDataSet$Mean[is.na(missingActivityDataSet$steps)]
```
### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newActivityDataSet <-missingActivityDataSet[,-4]
str(newActivityDataSet)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ Interval: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ steps   : num  1.72 0 0 0 0 ...
##  $ date    : Date, format: "2012-10-01" "2012-11-23" ...
```
### 4. Make a histogram of the total number of steps taken each day 


```r
newDailyActivityDataSet <- aggregate(steps~date,data=newActivityDataSet, sum)
hist(newDailyActivityDataSet$steps)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

```r
str(newDailyActivityDataSet)
```

```
## 'data.frame':	61 obs. of  2 variables:
##  $ date : Date, format: "2012-10-01" "2012-10-02" ...
##  $ steps: num  10766 126 11352 12116 13294 ...
```
### 5.  Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
mean(newDailyActivityDataSet$steps)
```

```
## [1] 10766.19
```

```r
mean(dailyActivityDataSet$step, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(newDailyActivityDataSet$steps)
```

```
## [1] 10766.19
```

```r
median(dailyActivityDataSet$step, na.rm=TRUE)
```

```
## [1] 10765
```

## Are there differences in activity patterns between weekdays and weekends?
### 1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
newActivityDataSet$day <- weekdays(as.Date(newActivityDataSet$date))
newActivityDataSet$day=factor(newActivityDataSet$day)
levels(newActivityDataSet$day) <- list(weekday = c("Monday", "Tuesday","Wednesday","Thursday", "Friday"),
                                       weekend = c("Saturday", "Sunday"))
newActivityDataSet$day=factor(newActivityDataSet$day,levels=c("weekday","weekend"))
```
### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:

```r
str(newActivityDataSet)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ Interval: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ steps   : num  1.72 0 0 0 0 ...
##  $ date    : Date, format: "2012-10-01" "2012-11-23" ...
##  $ day     : Factor w/ 2 levels "weekday","weekend": 1 1 2 1 2 1 2 1 1 2 ...
```

```r
newIntervalMeanActivityDataSet=aggregate(steps~Interval+day,newActivityDataSet,mean)
library(lattice)
xyplot(steps~Interval|factor(day),data=newIntervalMeanActivityDataSet,aspect=1/2,type="l")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 
