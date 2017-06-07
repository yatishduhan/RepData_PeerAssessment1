# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data


```r
activityData <- read.csv("./repdata%2Fdata%2Factivity/activity.csv",stringsAsFactors = FALSE)
activityData$date <- as.Date(activityData$date)
```


## What is mean total number of steps taken per day?
  1. Calculate the total number of steps taken per day
  2. Make a histogram of the total number of steps taken each day
  3. Calculate and report the mean and median of the total number of steps taken per day


```r
totalSteps <- aggregate(steps ~ date, activityData, sum) 
hist(totalSteps$steps,xlab="Total Steps taken each day",main="Total Steps taken each day")
```

![](PA1_Template_files/figure-html/analysis-1.png)<!-- -->

```r
mean(totalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(totalSteps$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
  1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
  2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
steps.interval <- aggregate(steps ~ interval, data=activityData, FUN=mean)
plot(steps.interval, type="l",xlab="interval", ylab="Average daily activity pattern of steps",  main="average number of steps")
```

![](PA1_Template_files/figure-html/avg. analysis-1.png)<!-- -->

```r
steps.interval[which.max(steps.interval$steps),]$interval
```

```
## [1] 835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
sum(is.na(activityData))
```

```
## [1] 2304
```

```r
activity.merge <- merge(activityData,steps.interval,by="interval")
activity.merge$steps.x[is.na(activity.merge$steps.x)] <- activity.merge$steps.y[is.na(activity.merge$steps.x)]

totalUpdatedSteps <- aggregate(steps.x ~ date, activity.merge, sum)
hist(totalUpdatedSteps$steps.x,xlab="Total Steps taken each day",main="Total Steps taken each day")
```

![](PA1_Template_files/figure-html/imputing-1.png)<!-- -->

```r
mean(totalUpdatedSteps$steps.x)
```

```
## [1] 10766.19
```

```r
median(totalUpdatedSteps$steps.x)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
typeofDay <- function(inputDate) {
    if (weekdays(inputDate) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
activityData$typeofDay <- as.factor(sapply(activityData$date, typeofDay))
activityWeekDayData <- activityData[activityData$typeofDay == 'weekday', ]
activityWeekendData <- activityData[activityData$typeofDay == 'weekend', ]

weekday.interval <- aggregate(steps ~ interval, data=activityWeekDayData, FUN=mean)

weekend.interval <- aggregate(steps ~ interval, data=activityWeekendData, FUN=mean)

par(mfrow=c(2,1))
plot(weekday.interval,type="l",main = "Weekdays")
plot(weekend.interval,type="l",main = "Weekend")
```

![](PA1_Template_files/figure-html/patterns-1.png)<!-- -->
