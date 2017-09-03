# Reproducible Research: Peer Assessment 1


## Loading and preproccessing the data

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
library(ggplot2)
file <- read.csv("activity.csv")
file <- tbl_df(file)
```

Exclude missing values from data:

```r
file1 <- subset(file, !is.na(steps))
```
## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.

3. Calculate and report the mean and median of the total number of steps taken per day.

```r
by_day <- group_by(file1, date)
steps_per_day <- summarize(by_day, steps.day = sum(steps))
hist(steps_per_day$steps.day, xlab = "Total number of steps per day", main = "Histogram of total number of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
summary(steps_per_day)
```

```
##          date      steps.day    
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8841  
##  2012-10-04: 1   Median :10765  
##  2012-10-05: 1   Mean   :10766  
##  2012-10-06: 1   3rd Qu.:13294  
##  2012-10-07: 1   Max.   :21194  
##  (Other)   :47
```
The mean of the total number of steps taken per day is **10766**, the median is **10765**.

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
by_interval <- group_by(file1, interval)
steps_per_interval <- summarize(by_interval, steps.interval = mean(steps))
with(steps_per_interval, plot(interval, steps.interval, type = 'l', xlab = "5-min. interval", ylab = "average number of steps"))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
steps_per_interval[which.max(steps_per_interval$steps.interval),]
```

```
## # A tibble: 1 x 2
##   interval steps.interval
##      <int>          <dbl>
## 1      835       206.1698
```
The maximum number of steps is **206** steps contained in interval **835**.

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
sum(is.na(file))
```

```
## [1] 2304
```
The total number of missing values is **2304**.

```r
mean_per_interval <- summarize(by_interval, steps.mean = mean(steps, na.rm = TRUE))
file2 <- file
for (i in 1:nrow(file2)) {
    if(is.na(file2$steps[i])) {
        interval_value <- file$interval[i]
        mean_value <- unlist(mean_per_interval[mean_per_interval$interval == interval_value, "steps.mean"])
        file2$steps[i] <- mean_value
    }
}
file2 <- tbl_df(file2)

by_day_imputed <- group_by(file2, date)
steps_per_day_imputed <- summarize(by_day_imputed, steps.day = sum(steps))
hist(steps_per_day_imputed$steps.day, xlab = "Total number of steps per day (imputed)", main = "Histogram of total number of steps per day (imputed)")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
summary(steps_per_day_imputed)
```

```
##          date      steps.day    
##  2012-10-01: 1   Min.   :   41  
##  2012-10-02: 1   1st Qu.: 9819  
##  2012-10-03: 1   Median :10766  
##  2012-10-04: 1   Mean   :10766  
##  2012-10-05: 1   3rd Qu.:12811  
##  2012-10-06: 1   Max.   :21194  
##  (Other)   :55
```
The mean of the total number of steps taken per day is **10766**, the median is now **10766**. The median of the imputed data 
differs slightly from the non-imputed data.

## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
week <- weekdays(as.Date(file2$date))
file2$week <- "weekday"
file2$week[week %in% c("Saturday", "Sunday")] <- "weekend"
file2$week <- as.factor(file2$week)

by_interval_week <- group_by(file2, interval, week)
steps_per_interval_week <- summarize(by_interval_week, steps.interval = mean(steps))

ggplot(steps_per_interval_week, aes(interval, steps.interval)) + 
    facet_wrap(~week, ncol = 1) +
    geom_line() + 
    labs(x = "Interval", y = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
