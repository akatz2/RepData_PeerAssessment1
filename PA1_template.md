# Reproducible Research: Peer Assessment 1
A Katz  
# Overview

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a [Fitbit](http://www.fitbit.com/), [Nike Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or [Jawbone Up](https://jawbone.com/up). These type of devices are part of the [“quantified self” movement](http://quantifiedself.com/guide/) – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This analysis makes use of a dataset of collected from an activity monitoring devices.  It investigates the overall trends of activity including: aggregate statistics, as well as time of day and day of week effects.

# Data Description

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.  The data for this assignment can be downloaded from the web site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip).  This data was last accessed 2/1/2016.

The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as 𝙽𝙰)
* date: The date on which the measurement was taken in YYYY-MM-DD format
* interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

# Analysis

## Libraries
In addition the the standard R libraries, we are using functions in the following:

[ggplot2](http://ggplot2.org/) - the Grammar of Graphics library for plotting


```r
library(ggplot2)
```

## Loading and preprocessing the data

The data is contained in file "activity.zip", which contains a single csv file "activity.csv".
The data can be loaded directly into R, no additional preprocessing beyond file extraction
is necessary.


```r
unzip("activity.zip")
DAT <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day
2. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day

Quesitons 1, 2, and 3 are computed in the code below, and shown in the figure "Histogram of Daily Steps".  By summing all of the step measurements by day, we find that the mean number of steps
taken daily is 10766.19, and the median number of steps taken daily is 10765.


```r
STEPS_BY_DATE <- aggregate(steps ~ date, data=DAT, FUN="sum")

M <- mean(STEPS_BY_DATE$steps,na.rm=TRUE)
MD <- median(STEPS_BY_DATE$steps,na.rm=TRUE)
ggplot(STEPS_BY_DATE,aes(x=steps)) + 
  geom_histogram(binwidth=1000) +
  ggtitle("Histogram of Daily Steps") +
  theme(plot.title=element_text(face="bold", size=20)) +
  geom_vline(xintercept=M, col="orange", linetype="longdash") +
  annotate("text",label=sprintf("Mean Daily Steps: %0.2f",M), x=17000, y=9) +
  annotate("text",label=sprintf("Median Daily Steps: %0.2f",MD), x=17000, y=8)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)



## What is the average daily activity pattern?

1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

The code shown below generates the graph and finds the point of maximum activity.  The findings show that across all days, interval 835 contains the maximum average number of steps at 206.17.


```r
AVG_5_MIN_INT <- aggregate( steps ~ interval, data=DAT, FUN=mean)

MAX_STEP_ROW <- AVG_5_MIN_INT[ which.max(AVG_5_MIN_INT$steps), ]

ggplot(AVG_5_MIN_INT,aes(x=interval,y=steps)) +
  geom_line() + 
  ggtitle("Average steps by 5-min Interval across all days") +
  theme(plot.title=element_text(face="bold", size=14)) + 
  geom_point(aes(x=interval,y=steps),data=MAX_STEP_ROW, col="orange") +
  annotate("text",label=sprintf("Max Daily Steps: %0.2f at Interval: %0.0f",MAX_STEP_ROW$steps, MAX_STEP_ROW$interval), x=1600, y=200) 
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as 𝙽𝙰). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

The code below computes the answers for this section and generates the histgram for question 4.  I found that there are a total of 2304 missing values in the dataset.  To fill the missing values, I computed the average number of steps in each interval, and used this value to fill in missing data for matching intervals.  Using this method, the new mean and median were both 10766.19, indicating that the mean had not changed, and the new median was slightly higher.


```r
DAT2 <- DAT

# Compute the number of missing elements
NUM_NA <- sum(is.na(DAT2$steps))
print(NUM_NA)
```

```
## [1] 2304
```

```r
# Replace the missing rows with the average of each interval

# we need to create a function that we can apply to each element that will simulate a lookup
fill_steps <- function(steps, interval) {
  retval <- steps
  if( is.na(steps) ) {
    retval <- AVG_5_MIN_INT[ AVG_5_MIN_INT$interval == interval, "steps" ]
  }
  return( retval )
}

# apply the na filling function to all elements
DAT2$steps <- mapply(fill_steps, DAT2$steps, DAT2$interval)

STEPS_BY_DATE2 <- aggregate(steps ~ date, data=DAT2, FUN="sum")
M2 <- mean(STEPS_BY_DATE2$steps )
MD2 <- median(STEPS_BY_DATE2$steps )
ggplot(STEPS_BY_DATE2,aes(x=steps)) + 
  geom_histogram(binwidth=1000) +
  ggtitle("Histogram of Daily Steps with NA replaced by 5-min interval mean") +
  theme(plot.title=element_text(face="bold", size=14)) +
  geom_vline(xintercept=M2, col="orange", linetype="longdash") +
  annotate("text",label=sprintf("Mean Daily Steps: %0.2f",M2), x=17000, y=9) +
  annotate("text",label=sprintf("Median Daily Steps: %0.2f",MD2), x=17000, y=8)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

The graph to answer this question is computed and shown below.  My impression is that the graph shows that there is higher activity throughout the day on the weekends as compared to weekdays.  There appears to be a spike in activity at approximately 8:30 am and 6:00 pm on weekdays, which would roughly correspond to the start and end of a work day. 


```r
# Label each aggregated record as weekday or weekend
DAT2$dayofweek <- weekdays( as.Date( DAT2$date ) )
DAT2$dow_cat <- ifelse( (DAT2$dayofweek %in% c("Saturday","Sunday")), "Weekend", "Weekday")
DAT2$dow_cat <- factor( DAT2$dow_cat )

STEPS_BY_DOW_CAT <- aggregate(steps ~ interval + dow_cat, data=DAT2, FUN="mean")

ggplot( STEPS_BY_DOW_CAT, aes( x=interval, y=steps, group=dow_cat ) ) +
  geom_line( aes(colour=dow_cat) ) +
  ggtitle("Avergage steps for 5-min interval by Day of Week  ") +
  theme(plot.title=element_text(face="bold", size=14)) +
  scale_colour_discrete(name="Day of Week\nCategory")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)
