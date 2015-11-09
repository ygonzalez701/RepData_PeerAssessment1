# Reproducible Research: Peer Assessment 1

### Loading and preprocessing the data

>Show any code that is needed to

>>1\. Load the data (i.e. `read.csv()`)

>>2\. Process/transform the data (if necessary) into a format suitable for your analysis


```r
setwd("C:/Users/Yago/Documents/Statistical Studies/The Data Science Track/Reproducible Research/Assignment 1")
require("dplyr")
```

```
## Loading required package: dplyr
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
require("lattice")
```

```
## Loading required package: lattice
```

```r
options(scipen=999, digits=2)
data<-read.csv("activity.csv")
data<-tbl_df(data)
```

### What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in
the dataset.

>>1\. Make a histogram of the total number of steps taken each day


```r
by_day<-
     data %>% 
     group_by(date) %>% 
     summarise(total.steps=sum(steps))

hist(by_day$total.steps, col="cadetblue1", border="pink", xlab= "Number of Steps", main="Frequency of Steps taken by day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

>>2\. Calculate and report the **mean** and **median** total number of steps taken per day

```r
mean<-mean((by_day$total.steps), na.rm=TRUE)
median<-median((by_day$total.steps), na.rm=TRUE)
```

The **mean** total number of steps taken per day is **10766.19**.

The **median** total number of steps taken per day is **10765**.


### What is the average daily activity pattern?

>>1\. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
data$five.interval<-as.factor(data$interval)
five_Interval<-
     data %>%
     group_by(five.interval) %>%
     summarise(mean.steps=mean(steps, na.rm=TRUE))

five_Interval$interval <- as.numeric(as.character(five_Interval$five.interval))

plot(five_Interval$interval, five_Interval$mean.steps, type = "l", col=4, xlab="5-Minute Intervals", 
      ylab="Mean Steps", main="Average Daily Activity Pattern" )
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

>>2\. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_Steps_Interval<-which.max(five_Interval$mean.steps)
print(five_Interval[max_Steps_Interval,])
```

```
## Source: local data frame [1 x 3]
## 
##   five.interval mean.steps interval
## 1           835        206      835
```


### Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

>>1\. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
data$weekday <- weekdays(as.Date(data$date))
data$weekday <- factor(data$weekday, levels= c("Monday", 
                                                       "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

sum(is.na(data$steps))  
```

```
## [1] 2304
```

>>2\. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I calculated the average number of steps for each day of week/interval combination and completed the dataset by substituting the corresponding value for each day of the week/interval that had NAs.


```r
activity_day <- data %>% 
     group_by(weekday, five.interval) %>% 
     summarise(mean.steps = mean(steps, na.rm =TRUE))
```

>>3\. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity_impute <- merge(data, activity_day, by=c("weekday","five.interval"))
activity_impute$impute.steps <- ifelse(is.na(activity_impute$steps), 
                                       activity_impute$mean.steps, activity_impute$steps)
```

>>4\. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
activity_impute_mean <- activity_impute %>% 
     group_by(date) %>% 
     summarise(total.steps = sum(impute.steps))

hist(activity_impute_mean$total.steps, 
     col="cadetblue1", border="pink",
     xlab="Number of Steps",
     main = "Frequency of Steps taken by Day (with no NAs)")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

```r
new_mean<-mean((activity_impute_mean$total.steps), na.rm=TRUE)
new_median<-median((activity_impute_mean$total.steps), na.rm=TRUE)
```

The mean of the dataset without NAs is **10873.55**, which compares to the value of **10766.19** obtained with the dataset that included NAs.

The median of the dataset without NAs is **11015**, which compares to the value of **10765** obtained with the dataset that included NAs.

## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

>>1\. Create a new factor variable in the dataset with two levels â âweekdayâ and âweekendâ indicating whether a given date is a weekday or weekend day.

```r
activity_impute <- activity_impute %>% 
          mutate(weekend = ifelse(weekday == "Saturday" | weekday == "Sunday", "weekend", "weekday"))
activity_impute_mean <- activity_impute %>% group_by(weekend, interval) %>% 
     summarise(mean.steps = mean(impute.steps))
```

>>2\. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
xyplot(mean.steps ~ interval | weekend, data = activity_impute_mean, 
       type = "l", layout = c(1,2), xlab = "Interval", ylab = "# of Steps", 
       main = "5-minute Interval for Weekends and Weekdays (Average Steps)")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

There is a visible difference as we would expect people to be more active on weekdays than on weekends.
