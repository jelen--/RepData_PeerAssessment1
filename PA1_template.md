---
title: "Peer Assessment 1"
output: html_document
---
## Load data

Below are shown the dimensions of the dataset (1768 observations and 3 variables), the head of the dataset and the summary of each variable.


```r
activity <- read.csv("C:/Users/Elena/Desktop/uni/cursos/Reproducible research-JHU/wk2/peer assessment/activity.csv")
dim(activity)
```

```
## [1] 17568     3
```

```r
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

```r
summary(activity)
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-03:  288   Median :1178  
##  Mean   : 37.4   2012-10-04:  288   Mean   :1178  
##  3rd Qu.: 12.0   2012-10-05:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355  
##  NA's   :2304    (Other)   :15840
```


## Clean data

Data is cleaned by removing the rows with "Not Available" items. Below are the new dimensions, head and summary of the data.


```r
complete_activity <- complete.cases(activity)
clean_activity <- activity[complete_activity, ]
dim(clean_activity)
```

```
## [1] 15264     3
```

```r
head(clean_activity)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```

```r
summary(clean_activity)
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-02:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-03:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-04:  288   Median :1178  
##  Mean   : 37.4   2012-10-05:  288   Mean   :1178  
##  3rd Qu.: 12.0   2012-10-06:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-07:  288   Max.   :2355  
##                  (Other)   :13536
```


## What is the mean total number of steps taken per day?

The average number of steps taken per day is calculated with the function "tapply" and the "Not Available" rows are cleaned. Below is shown the head of the total number of steps taken per day, extracted from the cleaned data.


```r
daysteps <- tapply(clean_activity$steps, clean_activity$date, sum)
goodsteps <- complete.cases(daysteps)
clean_daysteps <- daysteps[goodsteps]
head(clean_daysteps)
```

```
## 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##        126      11352      12116      13294      15420      11015
```


```r
hist(clean_daysteps, main = "Histogram of total number of steps taken per day", xlab = "Total number of steps")
```

![plot of chunk steps histogram](figure/steps histogram.png) 


```r
mean_daysteps <- mean(clean_daysteps)
median_daysteps <- median(clean_daysteps)
```

The mean total number of steps taken per day is 10765, and the median is 10765.


## What is the average daily activity pattern?

The mean steps taken in each 5-minute interval is calculated with the "tapply" function. Below is shown the head of the mean steps taken in each interval, averaged across all days.


```r
intervalsteps <- tapply(clean_activity$steps, clean_activity$interval, mean)
head(intervalsteps)
```

```
##       0       5      10      15      20      25 
## 1.71698 0.33962 0.13208 0.15094 0.07547 2.09434
```


```r
plot(rownames(intervalsteps), intervalsteps, type = "l", xlab = "5-minute interval", ylab = "Average number of steps", main = "Time series plot of the 5-minute interval and the average number of steps")
```

![plot of chunk interval plot](figure/interval plot.png) 


```r
maxsteps <- intervalsteps[intervalsteps == max(intervalsteps)]
ms <- names(maxsteps)
```

The 5-minute interval with the maximum number of steps is 835, which contains a mean of 206.1698 steps.


## Imputing missing values


```r
NAsteps <- is.na(activity$steps)
mv <- length(which(NAsteps == TRUE))
```

The total number of missing values is 2304.

The NA values are filled in with the mean number of steps of each interval, averaged across all days. The dimensions of the filled in activity, "fiactivity", are compared to the dimensions of the activity (they must be the same). The summary of the NA values of the "fiactivity" is also checked, since the number of NAs (TRUEs) should be 0.


```r
ind <- which(NAsteps)
fiactivity <- activity

fiactivity$steps[ind] = intervalsteps
dim(fiactivity) == dim(activity)
```

```
## [1] TRUE TRUE
```

```r
summary(is.na(fiactivity[, 1]))
```

```
##    Mode   FALSE    NA's 
## logical   17568       0
```


```r
fi_daysteps <- tapply(fiactivity$steps, fiactivity$date, sum)
head(fi_daysteps)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##      10766        126      11352      12116      13294      15420
```

```r
fi_daysteps_mean <- mean(fi_daysteps)
fi_daysteps_median <- median(fi_daysteps)
hist(fi_daysteps, main = "Histogram of total number of steps in filled in data", xlab = "Mean number of steps")
```

![plot of chunk hist mean median](figure/hist mean median.png) 

Above are shown the head of the new dataset and the histogram of the total number of steps taken per day. The new mean of steps per day is 1.0766 &times; 10<sup>4</sup>, and the new median is 1.0766 &times; 10<sup>4</sup>. The histogram is now more centered, and the new median corresponds with the mean.


## Are there differences in activity patterns between weekdays and weekends?

In this part of the analysis, the dataset used is the filled in dataset, "fiactivity".

A new dataset with a column named "day" is created with the constant "Weekday". The day of the week is checked in the date column and a list with this variable is created, "wkday". Then, the index number of the "wkday" list corresponding to saturday or sunday is used in order to replace the "Weekday" value by "Weekend" in the day column of the fiactivity dataset.

* The conditional list is done with "sábado" or "domingo" because I have an Spanish R Package, that is the same as "saturday" and "sunday". 


```r
fiactivity$day <- "Weekday"
wkday <- weekdays(as.Date(fiactivity$date))
indwk <- which(wkday[] == "sábado" | wkday[] == "domingo")
fiactivity$day[indwk] = "Weekend"
```

The mean number of steps is calculated in each interval among weekdays or weekends.


```r
wkdaysteps <- tapply(fiactivity[fiactivity$day=="Weekday",]$steps, fiactivity[fiactivity$day=="Weekday",]$interval, mean)
wkendsteps <- tapply(fiactivity[fiactivity$day=="Weekend",]$steps, fiactivity[fiactivity$day=="Weekend",]$interval, mean)
```

* Graphic is not supported, attached appart.

