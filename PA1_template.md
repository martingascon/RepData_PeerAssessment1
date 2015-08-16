# Reproducible Research: Peer Assessment 1
## Loading and preprocessing the data
Show any code that is needed to
    
1. Load the data (i.e. read.csv())


```r
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
data_noNAs <- na.omit(data)
data$date <-as.Date(data$date, "%Y-%m-%d")
data <-data.frame(data)
head(data)
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
For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day   
    

```r
# separate by date
spd <- aggregate(steps ~ date, data = data, FUN = "sum")
head(spd)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```
  
2. Make a histogram of the total number of steps taken each day
   

```r
# make histogram
barplot(spd$steps, names.arg = spd$date,
         main="Total number of steps per day", 
         xlab="Date", ylab="Steps", col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 
    
3. Calculate and report the mean and median total number of steps taken per day
 

```r
# calculate the mean 
mean(spd$steps)
```

```
## [1] 10766.19
```

```r
# calculate the median
median(spd$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
    
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
spi <- aggregate(steps ~ interval, data=data, FUN="mean")
plot(spi, type = "l", main="Time series steps",
     xlab="Interval", ylab="Average Number of Steps", 
     col="red", lwd=3)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
 

```r
max_Nsteps <- which.max(spi$steps)
spi[max_Nsteps,]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
NAs = sum(is.na(data$steps))
NAs
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

We use data (which contains NAs) and fill it with the 5-minute interval mean value


```r
new_NAs <- numeric()
for (i in 1:nrow(data)){
  if (is.na(data$steps[i])){
    new_NAs[i]<-spi$steps[which(spi$interval == data$interval[i])]
  }
}
```
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
new_data <-data
for (i in 1:nrow(data)){
   if (is.na(data$steps[i])){
    new_data$steps[i]  <-new_NAs[i]
  }
}
```
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
new_spd <- aggregate(steps ~ date, data = new_data, FUN = sum)
barplot(new_spd$steps, names.arg = new_spd$date,
         main="Total number of steps per day", 
         xlab="Date", ylab="Steps", col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

```r
mean(new_spd$steps)
```

```
## [1] 10766.19
```

```r
median(new_spd$steps)
```

```
## [1] 10766.19
```
Do these values differ from the estimates from the first part of the assignment? 

* The histogram and the mean are the same, but the median is different. 

What is the impact of imputing missing data on the estimates of the total daily number of steps?
 
* The daily number of steps is larger.
 
## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
day <- weekdays(new_data$date)
new_data$dow <- ifelse(day == "Saturday" | day == "Sunday", "weekend","weekday")
new_spd_we <- aggregate(steps ~ interval  + dow , data = new_data, FUN = "mean")
head(new_spd_we)
```

```
##   interval     dow      steps
## 1        0 weekday 2.25115304
## 2        5 weekday 0.44528302
## 3       10 weekday 0.17316562
## 4       15 weekday 0.19790356
## 5       20 weekday 0.09895178
## 6       25 weekday 1.59035639
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(lattice)
xyplot(steps ~ interval | dow, data=new_spd_we, type="l", 
      layout=c(1,2),  
      main="Total number of steps per day", 
      xlab="Interval", ylab="Steps", col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 




