---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

1. Unzip the folder and load the data into a data frame 'activity'.


```r
if (!file.exists('activity.csv')) {
  unzip("activity.zip")
}

activity <- read.csv("activity.csv", header = TRUE)
```

2. Transform the date variable into a format suitable for the analysis


```r
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

1. Calculate the total steps taken per day


```r
totalStepsByDate <- aggregate(steps ~ date, activity, FUN=sum)
```

2. Make a histogram of the total number of steps taken per day


```r
hist(totalStepsByDate$steps,
     main = "Steps per Day",
     xlab = "Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
meanStepsByDate <- round(mean(totalStepsByDate$steps, na.rm = TRUE), digits = 2)
medianStepsByDate <- round(median(totalStepsByDate$steps, na.rm = TRUE), digits = 2)
```

Mean of the number of steps taken per day: 10766.19

Median of the number of steps taken per day: 10765.00

## What is the average daily activity pattern?

1. Make a time-series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
meanStepsByInterval <- aggregate(steps ~ interval, activity, mean)

plot(meanStepsByInterval$interval, 
     meanStepsByInterval$steps, 
     type="l", 
     main = "Average Daily Activity Pattern", 
     xlab = "5-min interval", 
     ylab = "Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. 5-minute interval, on average across all the days in the dataset, which contains the maximum number of steps


```r
maxInterval <- meanStepsByInterval[which.max(meanStepsByInterval$steps),]
```

The interval 835 contains the maximum number of steps with 206.17

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset


```r
missingValues <- sum(!complete.cases(activity))
```

The total number of missing values in the dataset is: 2304 

2. Devise a strategy for filling in all of the missing values in the dataset. 

Strategy: Replace any NA with the mean of the corresponding 5-minute interval

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newActivity <- transform(activity, 
            steps = ifelse(is.na(activity$steps), 
            meanStepsByInterval$steps[match(activity$interval, 
            meanStepsByInterval$interval)],
            steps))
```

4. Make a histogram of the total number of steps taken each day with the new dataset 


```r
newTotalStepsByDate <- aggregate(steps ~ date, newActivity, FUN=sum)

hist(newTotalStepsByDate$steps,
     main = "Steps per Day with Imputing Missing Values",
     xlab = "Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

5. Calculate and report the new mean and median total number of steps taken per day. 


```r
newMeanStepsByDate <- round(mean(newTotalStepsByDate$steps, na.rm = TRUE), digits = 2)
newMedianStepsByDate <- round(median(newTotalStepsByDate$steps, na.rm = TRUE), digits = 2)
```

Mean of the number of steps taken per day: 10766.19

Median of the number of steps taken per day: 10766.19

6. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
diffMean = newMeanStepsByDate - meanStepsByDate
diffMedian = newMedianStepsByDate - medianStepsByDate
```

The difference between the two datasets in the mean steps is 0. On the other hand, the difference in the median steps is 1.19.

There was not a real impact of imputing missing data. Both the mean and median have similar values in the original and imputing datasets.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
newActivity$dayType <- factor(
    ifelse(tolower(weekdays(newActivity$date)) == "saturday" | 
            tolower(weekdays(newActivity$date)) == "sunday", 
           "weekend", 
           "weekday"))
```

2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(lattice)

meanStepsByDayType <- aggregate(steps ~ interval + dayType, newActivity, mean)

xyplot(steps ~ interval | dayType, 
       data = meanStepsByDayType, 
       layout = c(1, 2), 
       type = "l", 
       ylab = "Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
