---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data  


```r
library(dplyr); library(lubridate); library(ggplot2)
unzip('activity.zip')
data_init <- read.csv('activity.csv')

# Transform the dataframe 
# time: a interval column is transformed to a time series
# dayofweek: a date column is transformed to a day of week
# weekEnds: label Mon-Fri>'Weekday' Sat-Sun>'WeekEnd'
data <- data_init %>%
  mutate(date=ymd(date)) %>%
  mutate(time=rep(hms::as_hms(seq(from=0,to=24*60*60-1,by=300)),times=61)) %>%
  mutate(dayofweek=wday(data_init$date,label=TRUE)) %>%
  mutate(weekEnds=sapply(wday(date),function(x){
    if (x %in% 2:6) {x <- 'Weekday'}else{x <- 'weekend' }        
  }))
```

## What is mean total number of steps taken per day?
1. Calcullate the total number of steps taken per day.  

```r
dataNaRemoved <- data[!is.na(data$steps),]
byDate <- group_by(dataNaRemoved, date)
totalSteps <- summarize(byDate, total=sum(steps))
```

2. Make a histogram of the total number of steps taken each day.  

```r
g <- ggplot(totalSteps, aes(x=total))
g <- g + geom_histogram(binwidth=500)
g <- g + labs(title='Total Number of Steps per day (for 61 days)',
              y='Count (days)',
              x='The number of steps' )
print(g)
```

![](PA1_template_files/figure-html/mean_histogram-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day.  

```r
meanSteps <- round(mean(totalSteps$total),0)  
medianSteps <- median(totalSteps$total)
```
The number of steps per day:  
  
Mean:   1.0766\times 10^{4}  
Median: 10765  

## What is the average daily activity pattern?
1. Make a time series plot of the 5-minute interval and the aveerage number of steps taken, averaged across all days.


```r
byTime <- group_by(data, time)
avgByTime <- summarize(byTime, average=mean(steps,na.rm=TRUE))
brks <- c(0,120*60,240*60,360*60,480*60,600*60,720*60,840*60,960*60,1080*60,
          1200*60,1320*60,1440*60) # ticks of xaxis every 2hrs(in seconds)
lbls <- c('0:00','2:00','4:00','6:00','8:00','10:00','12:00','14:00','16:00',
          '18:00','20:00','22:00','24:00') # xticks labels
g <- ggplot(avgByTime, aes(x=time,y=average))
g <- g + geom_line() 
g <- g + scale_x_continuous(breaks=brks,labels=lbls)
g <- g + labs(title='Average Daily Activity Pattern (5min interval)',
              y='Average steps',
              x='Time' )
print(g)
```

![](PA1_template_files/figure-html/dailyActivityPattern-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  


```r
maxStepsTime <- avgByTime[avgByTime$average==max(avgByTime$average),]
maxStepsTime
```

```
## # A tibble: 1 × 2
##   time   average
##   <time>   <dbl>
## 1 08:35     206.
```
On average, 8:35-40(260 steps) is the most active.

## Imputing missing values
1.Calculate and report the total number of missing values in the dataset.  

```r
naVals <- colSums(is.na(data))
naVals
```

```
##     steps      date  interval      time dayofweek  weekEnds 
##      2304         0         0         0         0         0
```

2. Devise a strategy for filling in all of the missing values in the dataset.  


```r
naRows <- is.na(data$steps)

for (i in 1:sum(naRows)){
  # get interval time where steps is NA
  naTime <- data[naRows,'time'][i] 
  # fill NA with corresponding average value computed previously 
  data[naRows,'steps'][i]<- avgByTime$average[avgByTime$time==naTime]
}
```
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.  


```r
head(data)
```

```
##       steps       date interval     time dayofweek weekEnds
## 1 1.7169811 2012-10-01        0 00:00:00       Mon  Weekday
## 2 0.3396226 2012-10-01        5 00:05:00       Mon  Weekday
## 3 0.1320755 2012-10-01       10 00:10:00       Mon  Weekday
## 4 0.1509434 2012-10-01       15 00:15:00       Mon  Weekday
## 5 0.0754717 2012-10-01       20 00:20:00       Mon  Weekday
## 6 2.0943396 2012-10-01       25 00:25:00       Mon  Weekday
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
byDate <- group_by(data, date)
totalSteps <- summarize(byDate, total=sum(steps))
g <- ggplot(totalSteps, aes(x=total))
g <- g + geom_histogram(binwidth=500)
g <- g + labs(title='Total Number of Steps per day (for 61 days)',
              subtitle = '(NA values are imputed with an average in each interval)',
              y='Count (days)',
              x='The number of steps' )
print(g)
```

![](PA1_template_files/figure-html/newHistogram-1.png)<!-- -->

```r
meanSteps <- mean(totalSteps$total)
medianSteps <- median(totalSteps$total)
```


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
byWeekTime <- group_by(data, weekEnds,time)
avgByWeekTime <- summarize(byWeekTime, average=mean(steps))
```

```
## `summarise()` has grouped output by 'weekEnds'. You can override using the
## `.groups` argument.
```

2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).  

```r
byWeekTime <- group_by(data, weekEnds,time)
avgByWeekTime <- summarize(byWeekTime, average=mean(steps))
```

```
## `summarise()` has grouped output by 'weekEnds'. You can override using the
## `.groups` argument.
```

```r
g <- ggplot(avgByWeekTime, aes(x=time,y=average))
g <- g + geom_line() 
g <- g + facet_grid(weekEnds~.)
g <- g + scale_x_continuous(breaks=brks,labels=lbls)
g <- g + labs(title='Average Daily Activity Pattern Difference',
              subtitle='Between Weekdays and Weekends (5min interval)',
              y='Average steps',
              x='Time' )
print(g)
```

![](PA1_template_files/figure-html/panelPlot-1.png)<!-- -->
  
  
  
