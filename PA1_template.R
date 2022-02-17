

########Reproducible Research: Peer Assessment 1#########
        
#### Loading and preprocessing the data#######

#The code assumes the zip file is already in the working directory
file <-unzip("activity.zip")
data0 <- read.csv(file)
summary(data0)


####### What is mean total number of steps taken per day? #######

#Table of the sum of steps per day
total_steps_day1 <- aggregate(steps~date, data = data0, FUN = sum, na.rm=TRUE)
head(total_steps_day1)


#Plot of the sum of steps per day
b <- total_steps_day1$steps
hist(b, labels = paste0(round(hist(b,plot = FALSE)$counts/length(b)*100,1),"%"), 
     col = "yellow", xlab = "steps per day", ylim = c(0,30),
     main = "Total Number of Steps Per Day")

#Mean and Median of the sum of steps per day
mean(total_steps_day1$steps)
median(total_steps_day1$steps)


######## What is the average daily activity pattern? ############

#Table of average of steps by interval
avg_steps_interval <- aggregate(steps~interval, data0, FUN = mean, na.rm=TRUE)
head(avg_steps_interval)


#Time series plot of the average of steps by interval
plot(avg_steps_interval$steps~avg_steps_interval$interval, type="l", 
     xlab="intervals", ylab = "average number of steps", 
     main="Time series plot of the average of steps by interval", col="red")

#Maximum average number of steps by 5-minute interval
avg_steps_interval$interval[which.max(avg_steps_interval$steps)]

####### Imputing missing values #########

#Sum of NAs
sum(is.na(data0))

#Replace Nas with average total steps to get a new dataset
data0$steps[is.na(data0$steps)==T]<-mean(data0$steps, na.rm = TRUE) 

#Rename the new dataset
data1 <-data0
head(data1)

#Table showing the sum of total steps by date of the new dataset
total_steps_day2 <-aggregate(steps~date, data1, FUN = sum)
head(total_steps_day2)

#Histogram showing the sum of total steps by date of the new dataset
hist(total_steps_day2$steps, labels = TRUE, xlab = "Steps per day", 
     main = "Total Number of Steps Per Day of The New Dataset", col="green")


#Mean and media of the total steps by date of the new dataset
mean(total_steps_day2$steps)
median(total_steps_day2$steps)


###Are there differences in activity patterns between weekdays and weekends?###

#Create a function to group days into weekday or weekend
wkday_fun <- function(z) {
        wd <- weekdays(as.Date(z, '%Y-%m-%d'))
        if  (!(wd == 'Saturday' || wd == 'Sunday')) {
                x <- 'Weekday'
        } 
        else {
                x <- 'Weekend'
        }
        x
}

#Factoring Weekday verses Weekend
data1$weekday <- weekdays(as.Date(data1$date))
data1$week_type <- as.factor(sapply(data1$date, wkday_fun))
levels(data1$week_type)
head(data1)

#Average of total steps by interval for weekday and weekend
average_by_weektype <- aggregate(steps~interval+week_type, data1, FUN=mean)
head(average_by_weektype)

#load lattice library
library(lattice)
y <- average_by_weektype$steps
x <- average_by_weektype$interval
f <- average_by_weektype$week_type

#Plot with lattice, the Average of total steps by interval for weekday/weekend
xyplot(y~x|f, layout=c(1,2), type="l", xlab = "5-minutes Interval", 
       ylab = "Number of Steps")
