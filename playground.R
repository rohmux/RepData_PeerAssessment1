Sys.setlocale("LC_ALL","English")
datadir <- "data"
datafilename <- "activity.zip"
datasource <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
wDays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

if (file.exists(datadir)){
} else {
  dir.create(file.path(datadir))
}

download.file(datasource, paste(datadir, "/", datafilename, sep=""), mode="wb")

temp <- unz(paste(datadir, "/", datafilename, sep=""), "activity.csv")
df <- read.csv(temp)
df$day <- weekdays(as.Date(df$date))
df$wdays <- with(df, ifelse(df$day %in% wDays, "weekday", "weekend"))

meanStepsPerDay <- aggregate(df[,c('steps')], by=list(as.Date(df$date)), FUN=mean, na.rm=FALSE)