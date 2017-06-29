# Reproducible Research: Peer Assessment 1

## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The variables in this data set are
- steps: Number of steps taking in a 5-minute interval (missing values are coded as 𝙽𝙰)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken.

The data set is stored in a CSV file, which can be accessed from Roger Peng's Github repository (https://github.com/rdpeng/RepData_PeerAssessment1).  The data set has 17,568 observations in the three variables listed above.



## Loading and preprocessing the data

```r
## Load packages

library(ggplot2)
library(plyr)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
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
library(knitr)

## set working directory
setwd("~/Desktop/RepData_PeerAssessment1")

## unzip file "activity.zip""
unzip(zipfile = "./activity.zip") 

## read "activity.csv" and store to a data frame called "activity"
activity <- read.csv("activity.csv")

## Look at structure of data frame
str(activity) # 17568 observations of 3 variables: "steps", "date", "interval"
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
## make new variable "day" to capture day of week to address questions below
## reference: https://stackoverflow.com/questions/9216138/find-the-day-of-a-week
activity$day <- weekdays(as.Date(activity$date))  
```
## What is mean total number of steps taken per day?  (Ignore missing values in the dataset.)


```r
## 1 Calculate total number of steps taken per day.

TotalSteps <- aggregate(activity$steps ~ activity$date, FUN = sum)
# change column names for clarity
colnames(TotalSteps) <- c("Date", "Steps")
## output TotalSteps to answer question posed
TotalSteps
```

```
##          Date Steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

```r
## 2 Make a histogram of the total number of steps taken each day.

ggplot(TotalSteps, aes(x = TotalSteps$Steps)) + 
        geom_histogram(fill = "blue", binwidth = 1000) +
        labs( x = "Total Steps", y = "Frequency") +
        ggtitle("Frequency Histogram of Daily Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
## 3 Calculate and report the mean and median of the total number of steps taken per day.

## mean of steps
MeanSteps <- (mean(TotalSteps$Steps))
## median of steps
MedianSteps <- (median(TotalSteps$Steps))

## output to answer question posed
MeanSteps
```

```
## [1] 10766.19
```

```r
MedianSteps
```

```
## [1] 10765
```

The mean of the total steps taken per day is 10766.19.  The median of the total steps taken per day is 10765.

## What is the average daily activity pattern?


```r
## 1 Make a time series plot (type = "l") of the 5 minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

## remove NAs from data, set to data frame "clean".  This data frame has 15264 observations in 4 variables ("steps", "date", "interval", "day")
clean <- activity[!is.na(activity$steps) ,]

## calculate average steps per interval, store in data frame "Interval".  Reference:  https://www.researchgate.net/post/How_can_I_calculate_the_average_values_in_dataframe_with_R

Interval <- clean %>% 
group_by(interval) %>% #group by intervals
summarize(average = mean(steps)) #average

## Note: "Interval"'s variables are "interval" and "average"

## plot time series
ggplot(Interval, aes(x = interval, y = average)) +
        geom_line(color = "blue", size = 1) +
        labs(title = "Average Steps per Interval, Averaged Across Days", x = "Interval", y = "Average Daily Steps" )
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
## 2 Which 5 minute interval, on average across all the days in the dataset, contains the maximum number of steps?

## find max
MaxSteps <- max(Interval$average) 
## return the interval where the MaxSteps occurs to answer the question posed
Interval[Interval$average == MaxSteps, 1] 
```

```
## # A tibble: 1 x 1
##   interval
##      <int>
## 1      835
```
The maximum number of steps for a five minute interval was 206.1698 (rounded to 206).  The five minute interval containing this maximum starts at 8:35 am.

## Imputing missing values


```r
## 1 Calculate and report the total number of missing values in the dataset.
nrow(activity[is.na(activity$steps)  ,])
```

```
## [1] 2304
```
There are 2304 missing values in the "steps" variable in this dataset.



```r
## 2 Devise a strategy for filling in all the missing values in the dataset.  You could use the mean/median for the day, or for the 5 minute interval, etc.
```
My strategy will be to replace the missing values with the mean value of that interval for that day of the week.  This is justified because individuals often have weekly routines (i.e., basketball practice on Tuesday at 6 pm).


```r
## 3 Create a new dataset that is equal to the original data set but with the missing data filled in.

## reference: https://stackoverflow.com/questions/9322773/how-to-replace-na-with-mean-by-subset-in-r-impute-with-plyr

## create average number of steps per weekday and interval disregarding NAs.  
## split data frame, apply function, return results in data frame which has variables "interval", "day", "Average"
AverageDayInterval <- ddply(activity, .(interval, day), summarize, Average = mean(steps, na.rm = TRUE))


## create a data frame with the NAs for subbing.  Variables are "steps" (always = NA), "date", "interval", "day"
NAData <- activity[is.na(activity$steps) ,]

## merge NA data with average weekday interval in preparation for imputing.
Merged <- merge(NAData, AverageDayInterval, by = c("interval", "day"))

## reorder column names to match order in data frame "clean" in preparation for row binding.  Desired order is "steps", "date", "interval", "day".
## reference: https://stackoverflow.com/questions/16047750/change-the-order-of-the-columns-in-a-dataframe-using-r

Merged2 <- Merged[, c("steps", "date", "interval", "day", "Average")]

## replace "steps" in Merged2 with "Average".  This is where data is imputed.

## make a new variable so we don't overwrite previous data frame
Merged3 <- Merged2 
## replace
Merged3$steps <- Merged3$Average

# make dimensions of data frames the same in preparation for rbind
clean$Average <- mean(clean$steps)  

## rbind
## this data frame contains all the original data with NAs replaced by averages.
MergedFinal <- rbind(clean, Merged3)
#show first 15 rows to answer prompt 3.
head(MergedFinal,15)
```

```
##     steps       date interval     day Average
## 289     0 2012-10-02        0 Tuesday 37.3826
## 290     0 2012-10-02        5 Tuesday 37.3826
## 291     0 2012-10-02       10 Tuesday 37.3826
## 292     0 2012-10-02       15 Tuesday 37.3826
## 293     0 2012-10-02       20 Tuesday 37.3826
## 294     0 2012-10-02       25 Tuesday 37.3826
## 295     0 2012-10-02       30 Tuesday 37.3826
## 296     0 2012-10-02       35 Tuesday 37.3826
## 297     0 2012-10-02       40 Tuesday 37.3826
## 298     0 2012-10-02       45 Tuesday 37.3826
## 299     0 2012-10-02       50 Tuesday 37.3826
## 300     0 2012-10-02       55 Tuesday 37.3826
## 301     0 2012-10-02      100 Tuesday 37.3826
## 302     0 2012-10-02      105 Tuesday 37.3826
## 303     0 2012-10-02      110 Tuesday 37.3826
```

```r
## 4 Make a histogram with the total number of steps taken each day and calculate and report the mean and median of the total number of steps per day.  Do they differ from the estimates in the first part of the assignment?  What is the impact of imputing missing data?

## repeat code from above, now on data frame MergedFinal that has imputed values

## Calculate total number of steps taken per day
TotalSteps2 <- aggregate(MergedFinal$steps ~ MergedFinal$date, FUN = sum)

# change column names
colnames(TotalSteps2) <- c("Date", "Steps")

## mean of steps
MeanStepsImpute <- (mean(TotalSteps2$Steps))
## median of steps
MedianStepsImpute <- (median(TotalSteps2$Steps))

## output to answer question posed
MeanStepsImpute
```

```
## [1] 10821.21
```

```r
MedianStepsImpute
```

```
## [1] 11015
```
In the data set with imputed values, the mean steps per day is 10821.21.  The median steps per day is 11015.  Both statistics are **higher** in the imputed set.  (The original set had a mean of 10766.19 and median of 10765.)
Additonally, in the histogram below, we see that there is less variance and frequencies are more clustered around the mean (we expect this using mean substituition), although the distribution has not changed greatly.  More sophisticated options for data imputation might be found in the package "mice."


```r
## histogram, same code as previously, except with TotalSteps2 as data frame.
ggplot(TotalSteps2, aes(x = TotalSteps2$Steps)) + 
        geom_histogram(fill = "blue", binwidth = 1000) +
        labs( x = "Total Steps", y = "Frequency") +
        ggtitle("Frequency Histogram of Daily Steps with Imputed Data")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?


```r
# 1 Create a new factor variable in the dataset with two levels: "weekday" and "weekend."

MergedFinalDayType <- MergedFinal # rename so we don't overwrite they previous data frame.
#create a new factor variable based on the days of the week.
MergedFinalDayType$DayCategory <- ifelse(MergedFinalDayType$day %in% c("Saturday","Sunday"), "Weekend", "Weekday")

Interval2 <- ddply(MergedFinalDayType, .(interval, DayCategory), summarize, average = mean(steps)) # variables are "interval", "DayCategory", "average".


# 2 Make a panel plot containing a time series plot (type = "l") of the 5 minutes interval (x-axis) and the average number of steps taken averaged across all weekday days or all weekend days (y-axis)

ggplot(Interval2, aes(x = interval, y = average)) +
        geom_line() +
        labs(title = "Average Daily Steps by Day Type", x = "Interval", y = "Number of Steps") +
        facet_wrap(~DayCategory, ncol = 1, nrow =2)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
Yes, there are differences in activity patterns.  On weekdays, activity peaks around 8:30 am (likely during a commute to work).  It also appears that individuals remain immobile longer on weekend mornings.
