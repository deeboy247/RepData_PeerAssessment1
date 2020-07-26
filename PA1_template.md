---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and processing the data


```r
activity<-read.csv("activity.csv")
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
TotalSteps<-aggregate(steps~date, activity, sum)
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day. 


```r
hist(TotalSteps$steps, xlab=" Steps per day", ylab="Number of Days", main="Total Number of Steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
Meansteps <- mean(TotalSteps$steps)
Meansteps
```

```
## [1] 10766.19
```

```r
Mediansteps <- median(TotalSteps$steps)
Mediansteps
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
AverageSteps <- aggregate(steps~interval, activity, mean)
with(AverageSteps, plot(interval, steps, type = "l", ylab="Number of Steps", xlab="5-min interval", main="Average Number of Steps by 5-minutes Interval"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
AverageSteps[which.max(AverageSteps[,2]),1]
```

```
## [1] 835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)


```r
missingval <- is.na(activity[,1])
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

 Assigning the mean of the 5-minute interval

```r
m <- mean(AverageSteps$steps)
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activ<-activity
activ[missingval,1] <- m
head(activ)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```

4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# total number of steps taken per day
TotalSteps <- aggregate(steps~date, activ, sum)
hist(TotalSteps$steps, xlab=" Steps per day", ylab="Number of Days", main="Total daily number of steps taken each day afterimputing missing values")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
activ$date<-as.Date(activ$date)
activity2<-activ%>%
        mutate(dayType=ifelse(weekdays(activ$date)=="Saturday" | weekdays(activ$date)=="Sunday", "Weekend", "Weekday"))
head(activity2)
```

```
##     steps       date interval dayType
## 1 37.3826 2012-10-01        0 Weekday
## 2 37.3826 2012-10-01        5 Weekday
## 3 37.3826 2012-10-01       10 Weekday
## 4 37.3826 2012-10-01       15 Weekday
## 5 37.3826 2012-10-01       20 Weekday
## 6 37.3826 2012-10-01       25 Weekday
```

2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
averageStepByDayTypeAndInterval<-activity2 %>%
group_by(dayType, interval) %>%
summarize(averageStepByDay=sum(steps))
```

```
## `summarise()` regrouping output by 'dayType' (override with `.groups` argument)
```

```r
head(averageStepByDayTypeAndInterval)
```

```
## # A tibble: 6 x 3
## # Groups:   dayType [1]
##   dayType interval averageStepByDay
##   <chr>      <int>            <dbl>
## 1 Weekday        0             315.
## 2 Weekday        5             242.
## 3 Weekday       10             231.
## 4 Weekday       15             232.
## 5 Weekday       20             228.
## 6 Weekday       25             283.
```

```r
library(lattice)
with(averageStepByDayTypeAndInterval, 
xyplot(averageStepByDay ~ interval | dayType, type = "l",      
main = "Total Number of Steps within Interval by dayType",
xlab = "Daily Interval",ylab = "Average Number of Steps"))
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

