# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
if (!file.exists('./activity.csv')) {
  file.url<-'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
  download.file(
    file.url,
    destfile='./rep_data_activity.zip',
    method="curl"
  )
  unzip('./rep_data_activity.zip',overwrite=TRUE)
}
d <- read.csv('./activity.csv')
d$date <- as.Date(d$date, "%Y-%m-%d")

# Create copy to create imputed dataset later
d_copy <- d
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day


```r
total_steps = summarise(
  group_by(d,date),
  steps = sum(steps)
)
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
with ( 
  total_steps
  ,{
    hist(
      steps,
      breaks=20,
      col = "green", 
      main = "Histogram of the total number of steps per day", 
      xlab = "Total number of steps per day"
    )
  }
)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day

mean:


```r
mean(total_steps$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

median:


```r
median(total_steps$steps, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
interval_steps <- summarise(
  group_by(d,interval), 
  steps=mean(steps,na.rm = TRUE)
)

with(
  interval_steps,
  {
    plot(
      steps~interval,
      type="l",
      main = "Average number of steps taken per 5 minute interval across all days", 
      xlab = "5-minute interval",
      ylab = "Number of Steps"
    )
  }
)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
interval_steps[which.max(interval_steps$steps),]
```

```
## Source: local data frame [1 x 2]
## 
##   interval    steps
## 1      835 206.1698
```

So interval 835 contains the maximum number of steps

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(d$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Strategy: The NA values will be replaced by the mean of the 5-minute intervals

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# Replace the value of NA steps with the mean of the intervals
d_copy$steps <- ifelse(
  is.na(d$steps),
  # Replicate the mean intervals for each of the 61 days to allow for easy replacement
  rep(interval_steps$steps,61),
  d$steps
)
```
  
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
total_steps_imputed = summarise(
  group_by(d_copy,date),
  steps = sum(steps)
)

with ( 
  total_steps_imputed
  ,{
    hist(
      steps,
      breaks=20,
      col = "green", 
      main = "Histogram of the total number of steps per day (Imputed)", 
      xlab = "Total number of steps per day"
    )
  }
)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

mean:


```r
mean(total_steps_imputed$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

median:


```r
median(total_steps_imputed$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

The median increased by imputing the data, the mean stayed the same. The total number of steps measured has increased.

In the end the imputed dataset has almost the same median and mean, but as can be seen from the histograms the data changed and this might create bias depending on the analysis applied on the datasets.

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels ??? ???weekday??? and ???weekend??? indicating whether a given date is a weekday or weekend day.


```r
isWeekend <- function(date, ...){
  weekend_days <- c('Sat','Sun')
  ifelse(weekdays(date, abbreviate = TRUE) %in% weekend_days, 'weekend', 'week')
}

#Create summarised data set that is grouped by interval, weekend vs week
week_vs_weekend <- summarise(
  group_by(d_copy,interval,isWeekend=isWeekend(date)),
  steps=mean(steps)
)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
ggplot(week_vs_weekend, aes(interval, steps))  + facet_grid(isWeekend ~ .) + 
      xlab("5-minute interval") + ylab("Number of steps") + geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png) 
