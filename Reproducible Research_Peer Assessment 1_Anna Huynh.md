---
title: "Reproducible Research_Peer Assessment 1"
output:
  html_document: default
  keep_md: yes
  pdf_document: default
---

## Loading and preprocessing the data
### Show any code that is needed to

1. Load the data (i.e. read.csv())

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
setwd("C:/Users/Anhuynh/Desktop/Data Science_Cousera/Reproducible Research/Assignment")
if(!file.exists("data1")) {
        dir.create("data1")
}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = "./data1/activity.zip")
library(readr)
data = read_csv("./data1/activity.zip")
```

```
## Parsed with column specification:
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```
## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

### 1. Make a histogram of the total number of steps taken each day



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
data_fixed <- data[!is.na(data$steps), ]
subgroup_date <- ddply(data_fixed, ~date, summarise, sum = sum(steps))
```

```
## Error in ddply(data_fixed, ~date, summarise, sum = sum(steps)): could not find function "ddply"
```

```r
hist(subgroup_date$sum, col = "green")
```

```
## Error in hist(subgroup_date$sum, col = "green"): object 'subgroup_date' not found
```

```r
rug(subgroup_date$sum)
```

```
## Error in as.vector(x): object 'subgroup_date' not found
```

```r
abline(v = median(subgroup_date$sum, na.rm=TRUE), col = "magenta", lwd = 4)
```

```
## Error in median(subgroup_date$sum, na.rm = TRUE): object 'subgroup_date' not found
```

```r
abline(v = mean(subgroup_date$sum, na.rm=TRUE), col = "blue", lwd = 3)
```

```
## Error in mean(subgroup_date$sum, na.rm = TRUE): object 'subgroup_date' not found
```

```r
legend("topright", pch = 15, col=c("magenta", "blue"), legend=c("Median", "Mean"))
```

```
## Error in strwidth(legend, units = "user", cex = cex, font = text.font): plot.new has not been called yet
```

### 2. Calculate and report the mean and median total number of steps taken per day

```r
#the mean total number of steps taken per day
library(dplyr)
data_fixed <- data[!is.na(data$steps), ]
subgroup_date <- ddply(data_fixed, ~date, summarise, sum = sum(steps))
```

```
## Error in ddply(data_fixed, ~date, summarise, sum = sum(steps)): could not find function "ddply"
```

```r
mean(subgroup_date$sum)
```

```
## Error in mean(subgroup_date$sum): object 'subgroup_date' not found
```


```r
#the median total number of steps taken per day
library(dplyr)
data_fixed <- data[!is.na(data$steps), ]
subgroup_date <- ddply(data_fixed, ~date, summarise, sum = sum(steps))
```

```
## Error in ddply(data_fixed, ~date, summarise, sum = sum(steps)): could not find function "ddply"
```

```r
median(subgroup_date$sum)
```

```
## Error in median(subgroup_date$sum): object 'subgroup_date' not found
```

## What is the average daily activity pattern?

### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
subgroup_interval <- ddply(data_fixed, ~interval, summarise, mean=mean(steps))
```

```
## Error in ddply(data_fixed, ~interval, summarise, mean = mean(steps)): could not find function "ddply"
```

```r
with(subgroup_interval, plot(interval, mean, type = "p", col = "darkgreen", main = "Average Daily Activity", xlab = "Interval", ylab = "Number of steps"))
```

```
## Error in with(subgroup_interval, plot(interval, mean, type = "p", col = "darkgreen", : object 'subgroup_interval' not found
```

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
subgroup_interval <- ddply(data_fixed, ~interval, summarise, mean=mean(steps))
```

```
## Error in ddply(data_fixed, ~interval, summarise, mean = mean(steps)): could not find function "ddply"
```

```r
with(subgroup_interval, plot(interval, mean, type = "p", col = "darkgreen", main = "Average Daily Activity", xlab = "Interval", ylab = "Number of steps"))
```

```
## Error in with(subgroup_interval, plot(interval, mean, type = "p", col = "darkgreen", : object 'subgroup_interval' not found
```

```r
abline(v = subgroup_interval$interval[which.max(subgroup_interval$mean)], col = "green", lwd = 2)
```

```
## Error in int_abline(a = a, b = b, h = h, v = v, untf = untf, ...): object 'subgroup_interval' not found
```

```r
# determine max value
maxVal <- max(subgroup_interval$mean)
```

```
## Error in eval(expr, envir, enclos): object 'subgroup_interval' not found
```

```r
print(maxVal)
```

```
## Error in print(maxVal): object 'maxVal' not found
```

```r
maxLine <- subgroup_interval[subgroup_interval$mean == maxVal, ]
```

```
## Error in eval(expr, envir, enclos): object 'subgroup_interval' not found
```

```r
# find the interval
maxInterval <- maxLine$interval
```

```
## Error in eval(expr, envir, enclos): object 'maxLine' not found
```

```r
print(maxInterval)
```

```
## Error in print(maxInterval): object 'maxInterval' not found
```

## Imputing missing values
### Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(data))
```

```
## [1] 2304
```

```r
# Percentage of missing data over a dataset (4.37%)
mean(is.na(data))
```

```
## [1] 0.04371585
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


# Replacing all missing features values by the same constant/mean or median of the attribute

# Or using the most probable value to fill in the missing value (e.g. regression, inference-based tools using Bayesian formalism or decision tree induction)


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# replace a subset of columns ("steps") with their median

library(dplyr)
newData <- data %>% 
    mutate_at(vars(steps), ~ifelse(is.na(.), median(., na.rm = TRUE), .))

dim(data)
```

```
## [1] 17568     3
```

```r
dim(newData)
```

```
## [1] 17568     3
```
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# the impact of imputing missing data on the estimates of the total daily number of steps keeps value of median and mean of new dataset less than the original one (refer to two histograms built for visualization)
totalstep <- tapply(newData$steps, newData$date, sum)
hist(totalstep, col = "gold")
rug(totalstep)
abline(v = median(totalstep, na.rm=TRUE), col = "magenta", lwd = 4)
abline(v = mean(totalstep, na.rm=TRUE), col = "blue", lwd = 4)
legend("topright", pch = 15, col=c("magenta", "blue"), legend=c("Median", "Mean"))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

```r
# the mean total number of steps taken per day
library(dplyr)

new_subgroup_date <- ddply(newData, ~date, summarise, sum = sum(steps))
```

```
## Error in ddply(newData, ~date, summarise, sum = sum(steps)): could not find function "ddply"
```

```r
mean(new_subgroup_date$sum)
```

```
## Error in mean(new_subgroup_date$sum): object 'new_subgroup_date' not found
```

```r
# the median total number of steps taken per day
median(new_subgroup_date$sum)
```

```
## Error in median(new_subgroup_date$sum): object 'new_subgroup_date' not found
```

## Are there differences in activity patterns between weekdays and weekends?
### For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
# add a new column containing day of week
weekday_newData = weekdays(newData$date)
# add a new column containing either Weekday OR Weekend
dateType <- ifelse(weekday_newData == "Saturday" | weekday_newData == 
    "Sunday", "Weekend", "Weekday")
# convert column to factor
dateType_factor <- factor(dateType)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:


```r
# make a new dataset grouping data by interval and weekday[type]
library(plyr)
```

```
## ---------------------------------------------------------------------------------------------------------------------
```

```
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
```

```
## ---------------------------------------------------------------------------------------------------------------------
```

```
## 
## Attaching package: 'plyr'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise, summarize
```

```r
groupedBy_int_dat <- ddply(newData, .(interval, dateType_factor), summarise, 
    mean = mean(steps, na.rm = TRUE))

# get the level of numeric values
groupedBy_int_dat$interval <- as.numeric(as.character(groupedBy_int_dat$interval))
library(lattice)
xyplot(mean ~ interval | dateType_factor, groupedBy_int_dat, type = "l", 
    layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)

```r
install.packages("knitr")
```

```
## Error in install.packages : Updating loaded packages
```

```r
library(knitr)
render_html("Reproducible Research_Peer Assessment 1_Anna Huynh.Rmd")
```

```
## Error in render_html("Reproducible Research_Peer Assessment 1_Anna Huynh.Rmd"): unused argument ("Reproducible Research_Peer Assessment 1_Anna Huynh.Rmd")
```
