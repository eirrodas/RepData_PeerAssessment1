Reproducible Research Project 1
===============================

Code needed to set up data

    library(reshape2)
    library(knitr)
    opts_chunk$set(echo=TRUE, results='hold')

    library(data.table)

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:reshape2':
    ## 
    ##     dcast, melt

    library(ggplot2)

    activity <-read.csv("C:/Users/EIRR/Desktop/coursera/activity.csv")

What is the mean total number of steps taken per day?
-----------------------------------------------------

    meltedActivity <- melt(activity, id=c("date"), na.rm=TRUE, measure.vars="steps")
    castedActivity <- dcast(meltedActivity, date ~ variable, sum)
    hist(castedActivity$steps)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)<!-- -->

What is the average daily activity pattern?
===========================================

    actMean <- format(round(mean(castedActivity$steps), 2), nsmall = 2)
    actMedian <- median(castedActivity$steps)

    meltedInterval <- melt(activity, id=c("interval"), na.rm=TRUE, measure.vars="steps")
    castedInterval <- dcast(meltedInterval, interval ~ variable, mean)
    plot( castedInterval$interval, castedInterval$steps, type="l")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)<!-- -->

    maxRow <- castedInterval[castedInterval$steps==max(castedInterval$steps),]

Imputing missing values
=======================

    x <- activity$steps
    x1 <- length(which(is.na(x)))

    activityNa <- is.na(activity$steps)
    castedIntervalAdj <- cbind(castedInterval, as.integer(round(castedInterval$steps)))
    nonNaActivity <- activity[!activityNa,]
    NaActivity <- activity[activityNa,]
    NaResolved <- merge(NaActivity, castedIntervalAdj, by.x = "interval", by.y = "interval", all=FALSE )
    NaResolved$steps.x <- NULL
    NaResolved$steps.y <- NULL
    names(NaResolved)[3] <- paste("steps")
    NaResolvedActivity <- rbind(NaResolved, nonNaActivity)
    meltedActivity <- melt(NaResolvedActivity, id=c("date"), na.rm=TRUE, measure.vars="steps")
    castedActivity <- dcast(meltedActivity, date ~ variable, sum)
    hist(castedActivity$steps)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)<!-- -->

    impMean <- format(round(mean(castedActivity$steps), 2), nsmall = 2)
    impMedian <- median(castedActivity$steps)

Are there differences in activity patterns between weekdays and weekends?
=========================================================================

    wd <- !(weekdays(as.Date(NaResolvedActivity$date)) %in% c('Saturday','Sunday'))
    wdwe <- c("", "")
    for (i in 1:length(wd)) {
      if (wd[i]) {wdwe[i] <- "Weekday"} else {wdwe[i] <- "Weekend"}
    }
    NaResolvedActivity[, "dayType"] <- factor(wdwe)
    p <- ggplot(NaResolvedActivity, aes(x=interval, y=steps)) + geom_line()
    melted <- melt(NaResolvedActivity, id=c("interval", "dayType"), na.rm=TRUE, measure.vars="steps")
    casted <- dcast(melted, interval + dayType ~ variable, mean)
    p <- ggplot(casted, aes(x=interval, y=steps)) + geom_line() + ylab("Number of Steps")
    p + facet_wrap(~ dayType, ncol=1)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)<!-- -->
