---
title: "Reproducible Research: Peer Assessment 1"
author: "Sacha Liguori"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
Download and unzip data to obtain a csv file.

```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = paste0(getwd(), '/repdata%2Fdata%2Factivity.zip'), method = "curl")
unzip("repdata%2Fdata%2Factivity.zip",exdir = "data")
```

### Process and store data

```r
activityTable <- data.table::fread(input = "data/activity.csv")
```

## What is mean total number of steps taken per day?

### Total daily steps

```r
Tot_Steps <- activityTable[, c(lapply(.SD, sum, na.rm = FALSE)), .SDcols = c("steps"), by = .(date)] 

head(Tot_Steps, 10)
```

```
##           date steps
##  1: 2012-10-01    NA
##  2: 2012-10-02   126
##  3: 2012-10-03 11352
##  4: 2012-10-04 12116
##  5: 2012-10-05 13294
##  6: 2012-10-06 15420
##  7: 2012-10-07 11015
##  8: 2012-10-08    NA
##  9: 2012-10-09 12811
## 10: 2012-10-10  9900
```

### Histogram

```r
library(ggplot2)
ggplot(Tot_Steps, aes(x = steps)) +
    geom_histogram(fill = "red", binwidth = 1000) +
    labs(title = "Daily Steps", x = "Steps", y = "Frequency")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### Mean and median

```r
Tot_Steps[, .(Mean_steps = mean(steps, na.rm = TRUE), Median_steps = median(steps, na.rm = TRUE))]
```

```
##    Mean_steps Median_steps
## 1:   10766.19        10765
```

## What is the average daily activity pattern?
Following plot displays the individual's average number of steps with a 5-minute interval basis over the last two months.

```r
IntervalTable <- activityTable[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)] 

ggplot(IntervalTable, aes(x = interval , y = steps)) + geom_line(color="red", size=1) + labs(title = "Avg. daily steps", x = "Interval", y = "Avg. steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
Interval with the highest average number of steps:

```r
IntervalTable[steps == max(steps), .(max_interval = interval)]
```

```
##    max_interval
## 1:          835
```

## Imputing missing values

Total number of missing values in the dataset:

```r
activityTable[is.na(steps), .N ]
```

```
## [1] 2304
```

To get a complete set of data, "NA" entries will be replaced with the median steps taken value across the entire *clean* data set. 

```r
activityTable[is.na(steps), "steps"] <- activityTable[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
```

### Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
data.table::fwrite(x = activityTable, file="data/tidyData.csv", quote = FALSE)
```

### Histogram

```r
TotalSteps <- activityTable[, c(lapply(.SD, sum)), .SDcols = c("steps"), by = .(date)] 
TotalSteps[, .(Mean_Steps = mean(steps), Median_Steps = median(steps))]
```

```
##    Mean_Steps Median_Steps
## 1:    9354.23        10395
```

```r
ggplot(TotalSteps, aes(x = steps)) + geom_histogram(fill = "red", binwidth = 1000) + labs(title = "Daily Steps", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
Type of Estimate | Mean_Steps | Median_Steps
--- | --- | ---
First Part (with na) | 10766.19	 | 10765
Second Part (fillin in na with median) | 9354.23 | 10395

## Are there differences in activity patterns between weekdays and weekends?

###  New factor variable
Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day

```r
activityTable[, date := as.POSIXct(date, format = "%Y-%m-%d")]
activityTable[, `Day of Week`:= weekdays(x = date)]
activityTable[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = `Day of Week`), "weekday or weekend"] <- "weekday"
activityTable[grepl(pattern = "Saturday|Sunday", x = `Day of Week`), "weekday or weekend"] <- "weekend"
activityTable[, `weekday or weekend` := as.factor(`weekday or weekend`)]
head(activityTable, 10)
```

```
##     steps       date interval Day of Week weekday or weekend
##  1:     0 2012-10-01        0      Monday            weekday
##  2:     0 2012-10-01        5      Monday            weekday
##  3:     0 2012-10-01       10      Monday            weekday
##  4:     0 2012-10-01       15      Monday            weekday
##  5:     0 2012-10-01       20      Monday            weekday
##  6:     0 2012-10-01       25      Monday            weekday
##  7:     0 2012-10-01       30      Monday            weekday
##  8:     0 2012-10-01       35      Monday            weekday
##  9:     0 2012-10-01       40      Monday            weekday
## 10:     0 2012-10-01       45      Monday            weekday
```

### Panel plot
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
intervalTable <- activityTable[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `weekday or weekend`)] 

ggplot(intervalTable , aes(x = interval , y = steps, color=`weekday or weekend`)) + geom_line() + labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps") + facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
