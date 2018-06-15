library(ggplot2)
library(chron)

Sys.setlocale("LC_ALL","English")
datadir <- "data"
datafilename <- "activity.zip"
datasource <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
wDays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
allDays <- data.frame(day = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'), dayNumber = c(1,2,3,4,5,6,7))

if (file.exists(datadir)){
} else {
  dir.create(file.path(datadir))
}

download.file(datasource, paste(datadir, "/", datafilename, sep=""), mode="wb")

temp <- unz(paste(datadir, "/", datafilename, sep=""), "activity.csv")
df <- read.csv(temp)
timeString <- sprintf("%04d", df$interval, df$interval)
df$datetime <- as.POSIXct(paste(df$date, " ", substr(timeString, 0, 2), ":", substr(timeString, 3, 4), sep=""))
df$interval <- paste(substr(timeString, 0, 2), ":", substr(timeString, 3, 4), sep="")
df$day <- weekdays(as.Date(df$date))
df$wdays <- with(df, ifelse(df$day %in% wDays, "weekday", "weekend"))

sumStepsPerDay <- aggregate(df[,c('steps')], by=list(df$date), FUN=sum, na.rm=TRUE)
sumStepsPerDay$day <- weekdays(as.Date(sumStepsPerDay$Group.1))
sumStepsPerDay$wDay <- with(df, ifelse(sumStepsPerDay$day %in% wDays, "weekday", "weekend"))
qplot(sumStepsPerDay$x, xlab = "Steps per Day", ylab = "Count", main = "Histogramm of Steps taken per Day", bins= "20")

meanStepsPerDay <- aggregate(sumStepsPerDay[,c('x')], by=list(sumStepsPerDay$day), FUN=mean, na.rm=TRUE)

medianStepsPerDay <- aggregate(sumStepsPerDay[,c('x')], by=list(sumStepsPerDay$day), FUN=median, na.rm=TRUE)

meanStepsPerInterval <- aggregate(df[,c('steps')], by=list(df$interval), FUN=mean, na.rm=TRUE)
ticks <- meanStepsPerInterval[seq(1, nrow(meanStepsPerInterval), 24),][,1]
ggplot(data=meanStepsPerInterval, aes(x=Group.1, y=x, group=1)) + geom_line() + scale_x_discrete(breaks = c(ticks))

highestInterval <- meanStepsPerInterval[ meanStepsPerInterval$x == max(meanStepsPerInterval$x),]

countNA <- sum(is.na(df$steps))

df$steps <- with(df, ifelse(is.na(df$steps), aggregate(df[,c('steps')], by=list(df$interval), FUN=mean, na.rm=TRUE)[,2], df[,1]))
