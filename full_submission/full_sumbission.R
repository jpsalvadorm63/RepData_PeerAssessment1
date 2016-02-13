## Full submission

## setwd("C:/Users/jp/Desktop/coursera.org/Reproducible research/w1materials/project")

## ---------------------------------------------------------------------------------------------
## 1. Code for reading in the dataset and/or processing the data###
## ---------------------------------------------------------------------------------------------
## I will Work with english language
Sys.setlocale("LC_ALL","English")
## read activity dataset
activity <- read.csv("activity.csv", header = TRUE, sep = ",")
summary(activity)
head(activity, n = 8)



## ---------------------------------------------------------------------------------------------
## 2. Histogram of the total number of steps taken each day###
## ---------------------------------------------------------------------------------------------
## colors
mycolors = c("red", "yellow", "green", "violet", "orange", "blue", "pink", "cyan")
## Aggregate total steps by date
aggdata <- aggregate(x = list(activity$steps), by=list(date=as.Date(activity$date)), FUN=sum, na.rm=FALSE)
## fix name of columns (for some reazon the date column name is corrupted)
colnames(aggdata) <- c("date", "steps")
## build the histogram for totals
hist(aggdata$steps, main="Histogram for total number of steps per day", col=mycolors, xlab="Steps")
## Aggregate mean steps by date
aggdatamean <-aggregate(x = list(activity$steps),
                        by=list(date=as.Date(activity$date)),
                        FUN=mean)
## fix name of columns (for some reazon the date column name is corrupted)
colnames(aggdatamean) <- c("date", "steps")
hist(aggdatamean$steps, main="Histogram for mean of steps per day", col=mycolors, xlab="Steps")
## "garbage collector"
aggdata <- NULL
aggdatamean <- NULL



## ---------------------------------------------------------------------------------------------
## 3. Mean and median number of steps taken each day, show 8 top rows of every dataset
## ---------------------------------------------------------------------------------------------
## Aggregate media steps by date just for complete cases
meandata <-aggregate(x = list(activity$steps), by=list(date=as.Date(activity$date)), FUN=mean, na.rm=TRUE)
## fix name of columns (for some reazon the date column name is corrupted)
colnames(meandata) <- c("date", "mean.steps")
head(meandata, n=8)
## Aggregate media steps by date
mediandata <-aggregate(x = list(activity$steps), by=list(date=as.Date(activity$date)), FUN=median, na.rm=TRUE)
## fix name of columns (for some reazon the date column name is corrupted)
colnames(mediandata) <- c("date", "median.steps")
head(mediandata, n=8)



## ---------------------------------------------------------------------------------------------
## 4. Time series plot of the average number of steps taken
## ---------------------------------------------------------------------------------------------
## Aggregate media steps by interval just for complete cases
meandata2 <- aggregate(x = list(activity$steps), by=list(interval=activity$interval), FUN=mean, na.rm=TRUE)
## fix name of columns
colnames(meandata2) <- c("interval", "steps")
## plot(meandata, main='Time series')
plot(meandata2$interval, meandata2$steps, type="l", xlab="Interval", ylab="average number of steps")
## "garbage collector"
meandata2 <- NULL



## ---------------------------------------------------------------------------------------------
## 5. The 5-minute interval that, on average, contains the maximum number of steps
## ---------------------------------------------------------------------------------------------
## Aggregate media steps by  just for complete cases
meandata2 <-aggregate(x = list(activity$steps), by=list(interval=activity$interval), FUN=mean, na.rm=TRUE)
## rename columns
colnames(meandata2) <- c("Interval", "Average.steps")
## build and visualize results
knitr::kable(meandata2[which(meandata2$Average.steps == max(meandata2$Average.steps)),])
## "garbage collector"
meandata2 <- NULL



## ---------------------------------------------------------------------------------------------
## 6. Code to describe and show a strategy for imputing missing data
## ---------------------------------------------------------------------------------------------
## set seed, so another user can reproduce exactly the same data a same plots
set.seed(9876)
## add new column, weekday: TRUE if it is MON, TUE, . . . FRIDAY; FALSE for SAT and SUN
activity$weekday <- ifelse(weekdays(as.Date(activity$date)) %in% c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"), TRUE, FALSE)
## add new column, weekdayname: "TRUE if it is MON, TUE, . . . FRIDAY; FALSE for SAT and SUN"Monday", "Tuesday", . ."
activity$weekdayname <- weekdays(as.Date(activity$date))
## set the dataset for weekdays
activityWeekday <- activity[activity$weekday,]
## set the dataset for weekend
activityWeekend <- activity[!activity$weekday,]
## Set dataset for each day of the week
activityMonday <- activity[activity$weekdayname == "Monday",]
activityTuesday <- activity[activity$weekdayname == "Tuesday",]
activityWednesday <- activity[activity$weekdayname == "Wednesday",]
activityThursday <- activity[activity$weekdayname == "Thursday",]
activityFriday <- activity[activity$weekdayname == "Friday",]
activitySaturday <- activity[activity$weekdayname == "Saturday",]
activitySunday <- activity[activity$weekdayname == "Sunday",]

## This function will generate randomic dataset based en existing dataset,
## what I'm doing is: on valid data I get max, min, mean, sd steps.
## Also get the number of data to be generate. Using this calculated params
## I get the normal distribution (dnorm) so a proceed to generate missing data
## that is used to fill the missing data
fillDay <- function(activityDay) {
    dayds <- activityDay
    ## get current max steps for complete cases data
    maxSteps <- max(dayds[complete.cases(dayds),]$steps)
    ## get current min steps for complete cases data
    minSteps <- min(dayds[complete.cases(dayds),]$steps)
    ## get current mean od steps for complete cases data
    meanSteps <- mean(dayds[complete.cases(dayds),]$steps)
    ## get current standar deviation steps for complete cases data
    sdSteps <- sd(dayds[complete.cases(dayds),]$steps)
    ## get the number of data to be filled
    genSteps <- length(dayds[!complete.cases(dayds),]$steps)
    ## calculate the normal distribution for data tobe generated, according to min, max, mean and
    ## standar deviation of the step for current complete cases data
    myprob <- dnorm(seq(minSteps, maxSteps, by = 1), mean = meanSteps, sd = sdSteps)
    ## gen the data
    gendata <- sample(seq(minSteps, maxSteps, by = 1), size=genSteps, replace=TRUE, prob=myprob)
    ## FILL MISSING DATA WITH GENERATED ONE !!!
    dayds[!complete.cases(dayds),]$steps <- gendata
    ## "garbage collector"
    myprob <- NULL
    gendata <- NULL
    ## return results
    return(dayds)
}

## for each day dataset it will be gendrated the missin data using the fillDay function
## in activity2 datase I will build the filled the new datase mantaining the original one
activity2 <- fillDay(activityMonday)
activity2 <- rbind(activity2, fillDay(activityTuesday))
activity2 <- rbind(activity2, fillDay(activityWednesday))
activity2 <- rbind(activity2, fillDay(activityThursday))
activity2 <- rbind(activity2, fillDay(activityFriday))
activity2 <- rbind(activity2, fillDay(activitySaturday))
activity2 <- rbind(activity2, fillDay(activitySunday))

## "garbage collector"
activityMonday <- NULL
activityTuesday <- NULL
activityWednesday <- NULL
activityThursday <- NULL
activityFriday <- NULL
activitySaturday <- NULL
activitySunday <- NULL



## ---------------------------------------------------------------------------------------------
## 7. Histogram of the total number of steps taken each day after missing values are imputed
## ---------------------------------------------------------------------------------------------
## colors
mycolors = c("red", "yellow", "green", "violet", "orange", "blue", "pink", "cyan")
## Aggregate total step by date
aggdata2 <-aggregate(x = list(activity2$steps), by=list(date=as.Date(activity2$date)), FUN=sum, na.rm=FALSE)
## fix name of columns (for some reazon the date column name is corrupted)
colnames(aggdata2) <- c("date", "steps")
hist(aggdata2$steps, main="Histogram with missing data filled", col=mycolors, xlab="Steps")



## 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends***
## WITH MISSIN DATA NOT INPUTED
meandataWeekday <-aggregate(x = list(activityWeekday$steps), by=list(date=as.Date(activityWeekday$date)), FUN=mean, na.rm=TRUE)
colnames(meandataWeekday) <- c("date","steps")
meandataWeekend <-aggregate(x = list(activityWeekend$steps), by=list(date=as.Date(activityWeekend$date)), FUN=mean, na.rm=TRUE)
colnames(meandataWeekend) <- c("date","steps")
hist(meandataWeekday$steps, main="Histogram, weekdays (missing data not imputed)", col=mycolors, xlab="Steps")
hist(meandataWeekend$steps, main="Histogram, weekend (missing data not imputed)", col=mycolors, xlab="Steps")



## 9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report***
## PA1_template.Rmd

