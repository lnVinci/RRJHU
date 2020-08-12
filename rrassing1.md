---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
Unzip the included zip file, which extracts a csv file. Then read the csv file

```r
# load data
filenames <- unzip("activity.zip")
act <- read.csv(filenames)
```

## What is the mean total number of steps taken per day?

```r
library(ggplot2)
# find step total for each day
dailyTot <- tapply(act$steps, act$date, sum, na.rm=TRUE)
qplot(dailyTot, bins=24, main="Daily Total Number of Steps", xlab="Steps", ylab="Frequency")
```

![](rrassing1_template_files/figure-html/2-1.png)<!-- -->

```r
summary(dailyTot)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```
The mean total number of steps per day is 9354 steps, and the median is 10395.

## What is the average daily activity pattern?

```r
# find average steps for each interval
dailyAvg <- tapply(act$steps, act$interval, mean, na.rm=TRUE)
# maximum steps in an interval
sprintf("Maximum: %.2f at interval %s", max(dailyAvg), names(which.max(dailyAvg)))
```

```
## [1] "Maximum: 206.17 at interval 835"
```

```r
qplot(act$interval[1:288], dailyAvg, geom="line", main="Daily Mean Number of Steps", xlab="Interval", ylab="Mean # of Steps")
```

![](rrassing1_template_files/figure-html/3-1.png)<!-- -->

The most active 5-minute interval was at interval 835, with about 206 steps.


## Imputing missing values

```r
sum(is.na(act$steps))
```

```
## [1] 2304
```

```r
sum(is.na(act$date))
```

```
## [1] 0
```

```r
sum(is.na(act$interval))
```

```
## [1] 0
```

```r
actM <- act
intervals <- as.numeric(names(dailyAvg))
dailyAvg <- c(as.matrix(dailyAvg))

for (i in 1:dim(actM)[1]) {
  if (is.na(actM$steps[i])) {
    # find matching interval and set average steps for that interval
    actM$steps[i] <- dailyAvg[which(sapply(intervals, function(y) actM$interval[i] %in% y))]; # possibly i % 288 would work just as well
  }
  if (is.na(actM$steps[i])) {
    actM$steps[i] <- 0;
  }
}

# new total steps per day
dailyTotNew <- tapply(actM$steps, actM$date, sum)
summary(dailyTotNew)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

```r
qplot(dailyTotNew, bins=24, main="Daily Total Number of Steps", xlab="Steps", ylab="Frequency")
```

![](rrassing1_template_files/figure-html/4-1.png)<!-- -->

```act$steps``` had 2304 missing values. These values were approximated as the average number of steps at that time of day across all days. 
The mean and median number of steps daily are both now 10766 steps. The mean increased because the imputted step numbers were larger.


## Are there differences in activity patterns between weekdays and weekends?

```r
# use the dataset with the filled-in missing values for this part
actM$dayOfWeek <- sapply(lapply(actM$date, as.Date), weekdays)
actM$isWeekday <- factor(actM$dayOfWeek %in% c("Saturday", "Sunday"), labels = c("weekday", "weekend"))

par(mfrow=c(2,1))
plot(intervals, tapply(subset(actM, isWeekday=="weekday")$steps, subset(actM, isWeekday=="weekday")$interval, mean, na.rm=TRUE), "l", main="Weekday Step Counts", 
  xlab="Interval", ylab="Number of Steps", ylim=c(0, 230))
plot(intervals, tapply(subset(actM, isWeekday=="weekend")$steps, subset(actM, isWeekday=="weekend")$interval, mean, na.rm=TRUE), "l", main="Weekend Step Counts", 
  xlab="Interval", ylab="Number of Steps", ylim=c(0, 230))
```

![](rrassing1_template_files/figure-html/5-1.png)<!-- -->
