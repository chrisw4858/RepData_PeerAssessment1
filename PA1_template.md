---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

Reproducible Research (Johns Hopkins/Coursera)  
==============================================
Peer Assignment 1
===================

## Loading and preprocessing the data

First load the necessary packages:

```r
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
```

Unzip the data file and load into a data table.

```r
if(!file.exists("activity.csv")){
    unzip("activity.zip")
}
activity = read.csv("activity.csv", stringsAsFactors = FALSE, 
                    colClasses = c("integer","Date","character"))
```

## What is mean total number of steps taken per day?

Start off by calculating the mean number of steps taken each day, while ignoring any NA values.  


```r
activityPerDay <- activity %>%
  group_by(date) %>%
  summarize(steps = sum(steps, na.rm = TRUE))
head(activityPerDay)
```

```
## Source: local data frame [6 x 2]
## 
##         date steps
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

Note that with this particular approach 10/1/2012 which had only NA entries is included in the calculation as having zero steps.

We then look at a histogram of the number of steps per day.  Visually, the 'typical' value appears to be around 10,000 steps in day, while there are 10 days with no steps recorded at all.


```r
ggplot( activityPerDay, aes(steps) ) + 
  geom_histogram(binwidth = 500) + 
  xlab( "Total Number of Steps Per Day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 


We then calculate the mean and median steps per day, when NA values are not accounted for.

```r
activityPerDaySummary <- activityPerDay %>%
  summarize(meanStepsPerDay = mean(steps), 
            medianStepsPerDay = median(steps))
sprintf("Mean steps per day is %.0f",
        activityPerDaySummary$meanStepsPerDay)
```

```
## [1] "Mean steps per day is 9354"
```

```r
sprintf("Median steps per day is  %.0f", 
        activityPerDaySummary$medianStepsPerDay)
```

```
## [1] "Median steps per day is  10395"
```
## What is the average daily activity pattern?

To explore the daily activity pattern, we examine at the average number of steps taken in each 5 minute time period, averaged across all the days in the sample.  

[Note a transformation is applied to the interval supplied in the original data set to allow the activity to be charted across hours of the day.  The timezone is arbitrarily set to GMT.]

```r
# Calculate average number of steps per interval
activityDiurnal <- activity %>%
  group_by(interval) %>%
  summarize(steps = mean(steps, na.rm = TRUE))

# Calculate time of day from interval
activityDiurnal$time <- as.POSIXct(str_pad(activityDiurnal$interval, 4, "left", "0"),
                                   format="%H%M", origin = "1970-01-01", "GMT")

# Display timeseries chart
ggplot( activityDiurnal, aes(time, steps) ) + geom_line() +
  scale_x_datetime(labels = date_format("%H:%M"), breaks = "2 hour") +
  xlab("Time of Day") +
  ylab("Number of Steps per 5 Minute Interval") +
  ggtitle("Average Daily Activity")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

A peak of activity is observed after 8am, while minimal activity occurs between the hours of midnight to 5:30am (approximately).

Finally we examine the data to extract the 5 minute interval which has the most activity on average.


```r
activityDiurnal$time[which.max(activityDiurnal$step)]
```

```
## [1] "2015-08-15 08:35:00 GMT"
```

```r
activityDiurnal$interval[which.max(activityDiurnal$step)]
```

```
## [1] "835"
```

## Imputing missing values

The next question is: how big an effect do the missing values have?  We'll count how many NAs are present in count of steps, impute the missing values and then recalculate the mean and median steps per day.

Firstly, find out how many missing values are present:

```r
sum( is.na(activity$steps) )
```

```
## [1] 2304
```

Next, impute the missing values.  The approach here is to replace the missing value for a given 5 minute interval with the average of that 5 minute interval over all the days for which a value was present.


```r
activityImputed <- activity
for( i in 1:nrow(activity)) {
  activityImputed$steps[i] <- ifelse(is.na(activityImputed$steps[i]), 
                                     activityDiurnal$steps[ activityDiurnal$interval == activityImputed$interval[i]],
                                     activityImputed$steps[i]) 
}

activityPerDayImputed <- activityImputed %>%
  group_by(date) %>%
  summarize(steps = sum(steps, na.rm = TRUE))
head(activityPerDayImputed)
```

```
## Source: local data frame [6 x 2]
## 
##         date    steps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```
We see that 10/1/2010 now has a non-zero value for the number of steps  

Examining the histogram of the number of steps per day shows that there are far fewer days with zero steps (but some do remain). Visually, it appears that there is a greater number of days with more than 10,000 steps.


```r
ggplot( activityPerDayImputed, aes(steps) ) + 
  geom_histogram(binwidth = 500) + 
  xlab( "Total Number of Steps Per Day (after imputing missing data)")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

Recalculating the mean and median steps per day after imputing missing values shows that both measures have been impacted.


```r
imputedActivityPerDaySummary <- activityPerDayImputed %>%
  summarize(meanStepsPerDay = mean(steps), medianStepsPerDay = median(steps))

sprintf("Mean steps per day is %.0f, (difference from non-imputed %.0f)",
        imputedActivityPerDaySummary$meanStepsPerDay, 
        imputedActivityPerDaySummary[1]-activityPerDaySummary[1])
```

```
## [1] "Mean steps per day is 10766, (difference from non-imputed 1412)"
```

```r
sprintf("Median steps per day is %.0f, (difference from non-imputed %.0f)", 
        imputedActivityPerDaySummary$medianStepsPerDay,
        imputedActivityPerDaySummary[2]-activityPerDaySummary[2])
```

```
## [1] "Median steps per day is 10766, (difference from non-imputed 371)"
```


There is a larger impact on the mean value than on the median value of steps per day, possibly related to the days for which only NAs were present in the data.  In the previous calculations where NAs were ignored the mean value would definitely have been reduced.


## Are there differences in activity patterns between weekdays and weekends?

The average steps per interval were calculated separately for weekdays (Mon-Fri) and weekends (Sat-Sun).  We then compare the average daily activity for weekdays vs weekends.


```r
activityImputed$dayofweek <- factor( weekdays(activityImputed$date) %in% c( "Sunday", "Saturday"), labels=c("weekend", "weekday"))

activityDiurnalImputed <- activityImputed %>%
  group_by(dayofweek, interval) %>%
  summarize(steps = mean(steps, na.rm = TRUE))

activityDiurnalImputed$time <- as.POSIXct(str_pad(activityDiurnalImputed$interval, 4, "left", "0"),
                                   format="%H%M", origin = "1970-01-01", "GMT")

ggplot( activityDiurnalImputed, aes(time, steps) ) + geom_line() + facet_grid( dayofweek ~ .) +
  scale_x_datetime(labels = date_format("%H:%M"), breaks = "2 hour") +
  xlab("Time of Day") +
  ylab("Number of Steps per 5 Minute Interval") +
  ggtitle("Comparison of Average Daily Activity between Weekend and Weekdays")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

The charts do indeed show a difference in profile between weekdays and weekends with a notable change at the weekend being a reduction in the peak of activity seen between 8am-9am on weekdays.

