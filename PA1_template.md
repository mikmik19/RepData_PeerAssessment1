# Reproducible Research: Peer Assessment 1
## Introduction
The goal of this assignment is to analyse the data from a step-tracking accesory, such as Nike's fuel bank. 

This analysis will explore the following questions:  
- What is mean total number of steps taken per day?
- What is the average daily activity pattern?  

Then after Imputing missing values and checking for any diferences in the question addressed above, I go on to address the last question:  
- Are there differences in activity patterns between weekdays and weekends?  

## Importing the relevant packages

```r
        library(ggplot2)
```

## Loading and preprocessing the data

```r
        dat = read.csv('activity.csv')
```

## What is mean total number of steps taken per day?
First I must sum the number of steps taken per day. For this I use the aggregate function.

```r
        dat_date_aggregated <- aggregate(steps ~ date, dat, sum)
```
The mean and median of steps taken per day is the following:

```r
        summary(dat_date_aggregated$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```
To check the distribution of steps taken per day I make a histogram.

```r
        ggplot(data=dat_date_aggregated, aes(dat_date_aggregated$steps)) + geom_histogram(binwidth = 1000)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

The steps taken per day are distributed around 1000 steps.

## What is the average daily activity pattern?
To analyse the daily activity I plot the average number of steps taken for each 5 minute interval throughout the day. That is, each 5-minute interval is average across all days.

Avaging the number of steps taken each 5-min interval across all days:

```r
        five_min_interval_aggregate <- aggregate(steps ~ interval, dat, sum)        
```

Finding the 5-min interval with the highest average number of steps

```r
        index_of_max_setps <- which.max(five_min_interval_aggregate$steps)
        max_interval <- five_min_interval_aggregate$interval[index_of_max_setps]
        max_step <- five_min_interval_aggregate$steps[index_of_max_setps]
```

Showing the time series plot together with the position of the maximum interval.

```r
        label_string = paste('Max interval (', toString(max_interval), ', ', toString(max_step),')')
        ggplot(data=five_min_interval_aggregate, aes(interval,steps)) + geom_line() + geom_point(aes(max_interval, max_step, colour='red')) + geom_text(aes(max_interval+500, max_step, label = label_string, colour = 'red') ) + theme(legend.position = "none")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

The 5-minute interval, on averaged across all the days in the dataset, containing the maximum number of steps is interval number: 835, and the avaerage number og steps taken in that interval is: 10927  

## Imputing missing values
The total number of missing values in the dataset is:

```r
        sum(is.na(dat))
```

```
## [1] 2304
```

For each missing value I fill it out with the average number of steps taken in the interval where the number is missing.

```r
        dat_na_filled_in <- dat
        per_interval_mean <- aggregate(steps ~ interval, dat_na_filled_in, mean) 
        
        indices <- which(is.na(dat_na_filled_in$steps))
        relevant_intervals <- dat_na_filled_in$interval[indices]
        indices_of_intervals <- which(per_interval_mean == relevant_intervals)
        relevant_mean  <- per_interval_mean$steps[indices_of_intervals]
        
        dat_na_filled_in$steps[indices] <- relevant_mean
```

I check if this changes the histogram in any meaninful way: 

```r
        dat_na_filled_in_date_aggregate <- aggregate(steps ~ date, dat_na_filled_in, sum)
        ggplot(data=dat_na_filled_in_date_aggregate, aes(dat_na_filled_in_date_aggregate$steps)) + geom_histogram(binwidth = 1000)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


I thhen calculate the mean and median total number of steps taken per day, to see if they have changed due to the insertion of the missing values. For easy reference, the values before filling in the NAs were removed:

```r
        summary(dat_date_aggregated)
```

```
##          date        steps      
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8841  
##  2012-10-04: 1   Median :10765  
##  2012-10-05: 1   Mean   :10766  
##  2012-10-06: 1   3rd Qu.:13294  
##  2012-10-07: 1   Max.   :21194  
##  (Other)   :47
```

After they are removed the numbers are:

```r
        summary(dat_na_filled_in_date_aggregate)
```

```
##          date        steps      
##  2012-10-01: 1   Min.   :   41  
##  2012-10-02: 1   1st Qu.: 9819  
##  2012-10-03: 1   Median :10766  
##  2012-10-04: 1   Mean   :10766  
##  2012-10-05: 1   3rd Qu.:12811  
##  2012-10-06: 1   Max.   :21194  
##  (Other)   :55
```

The median and mean do not change in a significant way.

## Are there differences in activity patterns between weekdays and weekends?
To investigate this I must first figure out which days are “weekday” and which are “weekend”. Therefore I add an extra column in the dataframe contining this information:

```r
        dat_na_filled_in$day <- weekdays(as.Date(dat_na_filled_in$date))
        dat_na_filled_in$day[grepl("S|L", dat_na_filled_in$day)] <- "weekend"
        dat_na_filled_in$day[grepl("M|T|O|F", dat_na_filled_in$day)] <- "weekday"
```

I then make a plot of the number of steps taken for each five minute interval on weekdays (red) and weekends (black).

```r
        five_min_interval_aggregate_weekend <- aggregate(steps ~ interval, 
                data = subset(dat_na_filled_in, dat_na_filled_in$day=="weekend"), 
                sum)  
        five_min_interval_aggregate_weekday <- aggregate(steps ~ interval, 
                data = subset(dat_na_filled_in, dat_na_filled_in$day=="weekday"), 
                sum) 
```


```r
        pl1 <- ggplot(data=five_min_interval_aggregate_weekend,
                aes(interval, steps)) + 
                geom_line(colour='black') 
        pl1 <- pl1 + geom_line(data=five_min_interval_aggregate_weekday,
                aes(interval, steps), 
                colour='red') 
        pl1 <- pl1 + xlab('minutes since midnight') +
                ylab('number of steps taken') + 
                geom_text(aes(2000, 8500, label = "Weekday", colour = 'red') ) + 
                geom_text(aes(2000, 7500, label = "Weekend", colour = 'black') ) +
                theme(legend.position = "none")
        pl1
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

Why the blue text is blue when I'm telling it to be black will forever be a mystery to me.

There is indeed a difference between weekdays and weekends: People move less on weekends, and this is especially true for the weekend mornings! I wonder how the age of the participants are distributed, probably there aren't a look of parents with young children in the dataset.

