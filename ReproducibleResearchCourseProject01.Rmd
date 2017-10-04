---
title: "Course Project 1"
author: "Mehdi"
date: "4 October 2017"
output: html_document
---

Code for reading in the dataset and/or processing the data
```{r}
data <- read.csv(file="activity.csv",header = TRUE, sep = ",")
summary(data)
str(data)
```
```{r}
steps_1<-with(data,tapply(steps,date,sum,na.rm=TRUE))
hist(steps_1,xlab = "The total number of steps taken each day")
```
Histogram of the total number of steps taken each day
Mean and median number of steps taken each day


```{r}
library(ggplot2)
total.steps <- tapply(data$steps, data$date, sum, na.rm=TRUE)
qplot(total.steps, binwidth=1000, xlab="The total number of steps taken each day")
mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)
```

Time series plot of the average number of steps taken

```{r}
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) + 
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")
```

The 5-minute interval that, on average, contains the maximum number of steps

```{r}
averages[which.max(averages$steps),]
```

Code to describe and show a strategy for imputing missing data

```{r}
missing <- is.na(data$steps)

table(missing)
```

Histogram of the total number of steps taken each day after missing values are imputed
Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```{r}
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "steps"])
    return(filled)
    }
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)
## ------------------------------------------------------------------------
steps_2<-with(data,tapply(steps,date,sum,na.rm=TRUE))
hist(steps_2,xlab = "The total number of steps taken each day")
## ------------------------------------------------------------------------
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="The total number of steps taken each day")
mean(total.steps)
median(total.steps)

## ------------------------------------------------------------------------
weekday.or.weekend <- function(date) {
        day <- weekdays(date)
        if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
                return("weekday")
        else if (day %in% c("Saturday", "Sunday"))
                return("weekend")
        else
                stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)

## ------------------------------------------------------------------------
averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
        xlab("5-minute interval") + ylab("Number of steps")
```