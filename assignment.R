## Reproducible Research WK2 assignment
## Initial setting
library(lattice)
library(tidyverse)

## 1-1 and 1-2: Load the data, process/transform the data
setwd("~/coursera/JHU_reproducible/WK2/Assignment/")
myData0 <- read.csv("activity.csv") %>%
        mutate(date = as.Date(date))

## 2-1: Total number of steps taken per day
myData1 <- myData0 %>%
        group_by(date) %>%
        summarize(dailyTotal = sum(steps))

## 2-2: Histogram of the total number of steps taken each day
hist(myData1$dailyTotal, breaks = seq(0, 25000, 1000), 
     col = "blue", 
     main = "Histogram - daily total # of steps",
     xlab = "daily total number of steps")

## 2-3: Mean and median of the total number of steps taken per day
myMean <- mean(myData1$dailyTotal, na.rm = TRUE)
myMedian <- median(myData1$dailyTotal, na.rm = TRUE)

## 3-1: Time series plot of 5-minute interval
myData2 <- myData0 %>%
        group_by(interval) %>%
        summarize(meanSteps = mean(steps, na.rm = TRUE))
plot(x = myData2$interval, y = myData2$meanSteps, 
     type = "l", col = "blue",
     main = "Average Number of Steps Taken",
     xlab = "5 munites interval",
     ylab = "Average steps")

## 3-2: Maximum number of steps
myMaxInterval <- filter(myData2, meanSteps == max(meanSteps))[1]
myMaxIntervalValue <- filter(myData2, meanSteps == max(meanSteps))[2]

## 4-1: Total number of missing values
myData3 <- myData0 %>%
        mutate(isna = is.na(steps)) %>%
        filter(isna == TRUE)
myMissingValues <- nrow(myData3)

## 4-2: Strategy(step-1)
modelData <- myData0 %>%
        mutate(wkdays = weekdays(date)) %>%
        group_by(wkdays, interval) %>%
        summarise(mean = mean(steps, na.rm = TRUE)) %>%
        mutate(index = paste(wkdays, interval)) %>%
        select(index, mean)

## 4-2: Strategy(step-2)
myData4 <- myData0 %>%
        mutate(wkdays = weekdays(date)) %>%
        mutate(index = paste(wkdays, interval))

## 4-2: Strategy(step-3)
f <- function() {
        for (i in 1:nrow(myData4)) {
                if(is.na(myData4$steps[i]) == TRUE) {
                        modelData_Matched <- filter(modelData, 
                                                    index == myData4$index[i])
                        myData4$steps[i] <<- modelData_Matched$mean[1]
                }
        }        
        invisible(myData4)
}
f()

## 4-3: New dataset
myData4Output <- select(myData4, -wkdays, -index)
head(myData4Output)
str(myData4Output)

## 4-4: Checking the validity of myData4
myData5 <- mutate(myData4, isna = is.na(steps), count = 1) %>%
        group_by(date) %>%
        summarize(dailyTotal = sum(steps, na.rm = TRUE),
                  dailyNAs = sum(isna))
        totalDailyNAs <- unique(myData5$dailyNAs)

## 4-4: Histogram(after imputing) and comparison(before/after)
par(mfrow = c(1, 2))
hist(myData5$dailyTotal, breaks = seq(0, 25000, 1000),
     col = "skyblue",
     main = "Daily total number of steps \n(NAs imputed)",
     xlab = "daily total number of steps")

hist(myData5$dailyTotal, breaks = seq(0, 25000, 1000),
     col = "skyblue",
     main = "Comparison \n(Original overplotted)",
     xlab = "daily total number of steps")
hist(myData1$dailyTotal, breaks = seq(0, 25000, 1000),
     col='blue', add=TRUE)


## 4-4: Mean & median total number of steps (after NAs imputation)
myMeanNew <- mean(myData5$dailyTotal, na.rm = TRUE)
myMedianNew <- median(myData5$dailyTotal, na.rm = TRUE)

## Preparation (Function to divide weekend and weekday)
f2 <- function(x) {
        if (x == "“y—j“ú" | x == "“ú—j“ú"){output <- "weekend"}
        else {output <- "weekday"}
        return(output)
}

## 5-1: Create factor variable (gweekdayh and gweekendh)
wkendOrNot <- lapply(myData4$wkdays, function(x) f2(x))
myData4 <- mutate(myData4, wkendOrNot = wkendOrNot)
myData4_weekend <- myData4 %>%
        filter(wkendOrNot == "weekend") %>%
        group_by(interval) %>%
        summarize(meanSteps = mean(steps, na.rm = TRUE)) %>%
        mutate(wkendOrNot = "weekend")
myData4_weekday <- myData4 %>%
        filter(wkendOrNot == "weekday") %>%
        group_by(interval) %>%
        summarize(meanSteps = mean(steps, na.rm = TRUE)) %>%
        mutate(wkendOrNot = "weekday")
myData4 <- rbind(myData4_weekend, myData4_weekday)

## 5-2: Panel plot
xyplot(meanSteps ~ interval | wkendOrNot, myData4, 
       type = "l", layout=c(1,2))