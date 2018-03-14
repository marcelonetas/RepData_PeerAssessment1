---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
###*_Marcelo Silva_*  
###_14 março, 2018_


## Loading and preprocessing the data

1. Load the data (i.e. read.csv())


```r
library(psych)
```

```
## Warning: package 'psych' was built under R version 3.4.3
```

```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.4.3
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
unzip(zipfile="activity.zip")
activity <- read.csv("activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis  
_It isn't necessary process or transform the data_


## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
total_of_steps_per_day <- sum(activity$steps, na.rm = TRUE)
total_of_steps_per_day
```

```
## [1] 570608
```

2. Make a histogram of the total number of steps taken each day


```r
total_steps_each_day <- aggregate(steps~date, data=activity, FUN=sum, na.rm=TRUE)
hist(total_steps_each_day$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
total_steps_each_day_mean <- mean(total_steps_each_day$steps)
total_steps_each_day_mean
```

```
## [1] 10766.19
```

```r
total_steps_each_day_median <- median(total_steps_each_day$steps)
total_steps_each_day_median
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
five_minutes_average <- aggregate(steps~interval, data=activity, FUN=mean, na.rm=TRUE)
plot(x = five_minutes_average$interval, y = five_minutes_average$steps, type = "l") 
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_steps <- max(five_minutes_average$steps)
for (i in 1:288) {
    if (five_minutes_average$steps[i] == max_steps)
        five_minute_interval_at_max_steps <- five_minutes_average$interval[i]
}
five_minute_interval_at_max_steps 
```

```
## [1] 835
```


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
total_na <- 0
for (i in 1:17568) {
    if(is.na(activity$steps[i])) 
        total_na <- total_na+1 
}
total_na
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  
_The strategy could be to fill in the dataset the mean of that 5 minute interval_

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity_filled_in <- activity
for (i in 1:17568) {
    if(is.na(activity_filled_in$steps[i])) { 
        five_minute_pointer <- activity_filled_in$interval[i] 
        for (j in 1:288)  {
            if (five_minutes_average$interval[j] == five_minute_pointer) {
                activity_filled_in$steps[i] <- five_minutes_average$steps[j]
            }
        }
    }
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
total_steps_each_day_filled_in <- aggregate(steps~date, data=activity_filled_in, FUN=sum, na.rm=TRUE)

hist(total_steps_each_day_filled_in$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


```r
total_steps_each_day_mean_filled_in <- mean(total_steps_each_day_filled_in$steps)
total_steps_each_day_mean_filled_in
```

```
## [1] 10766.19
```

```r
total_steps_each_day_median_filled_in <- median(total_steps_each_day_filled_in$steps)
total_steps_each_day_median_filled_in
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
week <- wday(activity_filled_in$date)
week_day <- week
for (i in 1:17568) {
    if(week[i] == 1)
        week_day[i] <- 'weekend'
    if(week[i] == 2)
        week_day[i] <- 'weekday'
    if(week[i] == 3)
        week_day[i] <- 'weekday'
    if(week[i] == 4)
        week_day[i] <- 'weekday'
    if(week[i] == 5)
        week_day[i] <- 'weekday'
    if(week[i] == 6)
        week_day[i] <- 'weekday'
    if(week[i] == 7)
        week_day[i] <- 'weekend'
}

activity_filled_in$weekday <- week_day
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
weekday <- grep("weekday",activity_filled_in$weekday)
weekday_frame <- activity_filled_in[weekday,]
weekend_frame <- activity_filled_in[-weekday,]

five_minutes_average_weekday <- aggregate(steps~interval, data=weekday_frame, FUN=mean, na.rm=TRUE)
five_minutes_average_weekend <- aggregate(steps~interval, data=weekend_frame, FUN=mean, na.rm=TRUE)

plot(x = five_minutes_average_weekday$interval, y = five_minutes_average_weekday$steps, type = "l") 
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


```r
plot(x = five_minutes_average_weekend$interval, y = five_minutes_average_weekend$steps, type = "l") 
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
