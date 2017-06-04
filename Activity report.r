#Install and setup required packages

if (!require("pacman")) install.packages("pacman")
p_load(plyr, dplyr, downloader, lubridate, ggplot2, xtable, gridExtra)

library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

#Read in data

setwd("/")
if(!file.exists("Activity monitoring data.zip")){
  dir.create("Activity monitoring data.zip")
}
setwd("./Activity monitoring data.zip")
dir.create("Activity monitoring data")
setwd("./data.zip/Activity monitoring data")

download("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",dest="./Activity monitoring data.zip",mode="wb")
unzip("Activity monitoring data.zip")
setwd("/Activity monitoring data.zip")

#Process data

df <- read.csv("activity.csv", header=TRUE)

#Create aggregated summary data frame
sumstep_day <- aggregate(steps ~ date, df, FUN=sum, drop=FALSE)
meanstep_day <- aggregate(steps ~ date, df, FUN=mean, drop=FALSE)
medstep_day <- aggregate(steps ~ date, df, FUN=median, drop=FALSE)
summarystep_day <- sumstep_day %>% merge(meanstep_day, by="date") %>%
  merge(medstep_day,by="date")
names(summarystep_day) <- c("date","sumstep","meanstep","medstep")

#Histogram of the total number of steps taken each day

ggplot(data=summarystep_day,aes(x=date,y=sumstep))+ geom_histogram(stat="identity") + 
  theme(axis.text.x = element_text(angle = 55, hjust = 1))

#Mean and median number of steps taken each day

kable(summarystep_day)


#Time series plot of the average number of steps taken

meanstep_int <- aggregate(steps ~ interval, df, FUN=mean, drop=FALSE)
maxstep_int <- aggregate(steps ~ interval, df, FUN=max, drop=FALSE)
summarystep_int <- meanstep_int %>% merge(maxstep_int,by="interval", drop=FALSE)
names(summarystep_int) <- c("interval","meanstep","maxstep")

ggplot(data=summarystep_int, aes(x=interval,y=meanstep)) + geom_line()

#The 5-minute interval that, on average, contains the maximum number of steps

tmp <- arrange(summarystep_int,desc(meanstep))[1:6,1:2]
kable(tmp)

#Code to describe and show a strategy for imputing missing data

impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
df_impute <- ddply(df, ~ interval, transform, steps = impute.mean(steps))

#Histogram of the total number of steps taken each day after missing values 
#are imputed

sumstep_day_imp <- aggregate(steps ~ date, df_impute, FUN=sum, drop=FALSE)
names(sumstep_day_imp) <- c("date", "sumstep")

ggplot(data=sumstep_day_imp,aes(x=date,y=sumstep))+ 
  geom_histogram(stat="identity") + 
  theme(axis.text.x = element_text(angle = 55, hjust = 1))

#Panel plot comparing the average number of steps taken per 5-minute interval 
#across weekdays and weekends

meanstep_day_imp <- mutate(df_impute,day=wday(date,label=TRUE))

meanstep_day_imp_wkend <- filter(meanstep_day_imp,day=="Sat" | day=="Sun")
meanstep_day_imp_wkend <- aggregate(steps ~ interval, meanstep_day_imp_wkend, FUN=mean, drop=FALSE)

meanstep_day_imp_wkday <- filter(meanstep_day_imp,day=="Mon" | day=="Tues" | 
                                   day=="Wed" | day=="Thurs" | day=="Fri")
meanstep_day_imp_wkday <- aggregate(steps ~ interval, meanstep_day_imp_wkday, FUN=mean, drop=FALSE)

p1 <- ggplot(meanstep_day_imp_wkend, aes(x=interval, y=steps)) + geom_line() + ggtitle("Mean Weekend Steps") + ylim(0,250)
p2 <- ggplot(meanstep_day_imp_wkday, aes(x=interval,y=steps)) + geom_line() + ggtitle("Mean Weekday Steps") + ylim(0,250)

grid.arrange(p1, p2, ncol=2)