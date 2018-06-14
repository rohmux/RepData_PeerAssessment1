library(ggplot2)
library(chron)

Sys.setlocale("LC_ALL","English")
datadir <- "data"
datafilename <- "activity.zip"
datasource <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
wDays <- list('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
allDays <- list('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')

if (file.exists(datadir)){
} else {
  dir.create(file.path(datadir))
}

download.file(datasource, paste(datadir, "/", datafilename, sep=""), mode="wb")

temp <- unz(paste(datadir, "/", datafilename, sep=""), "activity.csv")
df <- read.csv(temp)
timeString <- sprintf("%04d", df$interval, df$interval)
df$date <- as.POSIXct(paste(df$date, " ", substr(timeString, 0, 2), ":", substr(timeString, 3, 4), sep=""))
df$interval <- paste(substr(timeString, 0, 2), ":", substr(timeString, 3, 4), sep="")
df$day <- weekdays(as.Date(df$date))
df$wdays <- with(df, ifelse(df$day %in% wDays, "weekday", "weekend"))

sumStepsPerDay <- aggregate(df[,c('steps')], by=list(df$day), FUN=sum, na.rm=TRUE)

meanStepsPerDay <- aggregate(df[,c('steps')], by=list(df$day), FUN=mean, na.rm=TRUE)

medianStepsPerDay <- aggregate(df[,c('steps')], by=list(df$day), FUN=median, na.rm=TRUE)

meanStepsPerInterval <- aggregate(df[,c('steps')], by=list(df$interval), FUN=mean, na.rm=TRUE)

maxStepsPerIntervalPerDay <- aggregate(df[,c('interval')], by=list(df$steps), FUN=max, na.rm=TRUE)


ggplot(data=meanStepsPerInterval, aes(x=Group.1, y=x, group=1)) + geom_line()


#meanStepsPerDay <- aggregate(df[,c('steps')], by=list(as.Date(df$date)), FUN=mean, na.rm=FALSE)
#meanStepsPerDay$day <- weekdays(as.Date(meanStepsPerDay$Group.1))

#ggplot(data=meanStepsPerDay, aes(x=Group.1, y=x, group=1)) + geom_line() + scale_x_date(date_breaks = "1 weeks")
