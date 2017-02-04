
# Set working directory
# setwd('~/Documents/Github Repos/DataScienceSpecialisation/5. Reproducible Research/week 2'); dir()

# Clear memory
rm(list=ls())

# Loading and preprocessing the data
fdata_orig <- read.csv("activity.csv")
d1 <- sprintf("%04d", as.numeric(fdata_orig$interval))
d2 <- format(strptime(d1, format="%H%M"), format = "%H:%M")
d3 <- as.POSIXct(d2, format="%H:%M")

fdata_orig$interval <- d3

fdata_imp_NA <- fdata_orig

# What is mean total number of steps taken per day?
tot_steps_pd_orig <- aggregate(steps ~ date, fdata_orig, sum, na.rm=TRUE)
hist(tot_steps_pd_orig$steps, col="red", xlab="Total steps per day (-)",
     ylab="Frequency", main="Histogram - Days with 10,000 to 12,000 steps occur most often (16 out of 53).",
     breaks=10, labels=TRUE)
mean_tot_steps_pd_orig <- mean(tot_steps_pd_orig$steps)
median_tot_steps_pd_orig <- median(tot_steps_pd_orig$steps)

# What is the average daily activity pattern?
avg_steps_per_interv_orig <- aggregate(steps ~ interval, fdata_orig, mean, na.rm=TRUE)

find_max_orig <- which.max(avg_steps_per_interv_orig$steps)
plot(avg_steps_per_interv_orig,type="l")
points(avg_steps_per_interv_orig[find_max_orig,],col="red", pch=19)
text(avg_steps_per_interv_orig$interval[find_max_orig]+50,avg_steps_per_interv_orig$steps[find_max_orig],
     "The most active 5-min interval is the 104th interval, which is around 8:35AM.", adj=0)

# Imputting missing values.

# Total number of missing values in the dataset, i.e. the total number of rows with NA.
no_of_rows_w_NA <- sum(!complete.cases(fdata_orig))

# Replace all missing values in the steps column with the mean for that 5-minute interval.
# Create a new dataset that is equal to the original dataset but with the missing data filled in.
fdata_imp_NA$steps <- ifelse(is.na(fdata_imp_NA$steps),
                      avg_steps_per_interv_orig$steps[match(fdata_imp_NA$interv, avg_steps_per_interv_orig$interv)],
                      fdata_imp_NA$steps)

# Make a histogram of the tot number of steps taken each day.
tot_steps_pd_imp_NA <- aggregate(steps ~ date, fdata_imp_NA, sum, na.rm=TRUE)
hist(tot_steps_pd_imp_NA$steps, col="red", xlab="Total steps per day (-)",
     ylab="Frequency", main="Histogram - Days with 10,000 to 12,000 steps occur most often (24 out of 61).",
     breaks=10, labels=TRUE)

# Calculate number of NA's per day - how are NA's distributed across days.
no_NA_pd <- lapply(split(fdata_orig,fdata_orig$date), 
                   function (fdata_orig) {sum(is.na(fdata_orig$steps))})
table(as.numeric(no_NA_pd))

# Calculate and report the mean and median tot no of steps taken per day.
mean_tot_steps_pd_imp_NA <- mean(tot_steps_pd_imp_NA$steps)
median_tot_steps_pd_imp_NA <- median(tot_steps_pd_imp_NA$steps)
# Do these values differ from the estimates from the first part of the assignment?
# What is the impact of imputing missing data on the estimates of the total daily number of steps?

# Are there differences in activity patterns between weekdays and weekends?
fdata_imp_NA$weekday <- with(fdata_imp_NA, weekdays(as.Date(fdata_imp_NA$date, format="%Y-%m-%d")))

fdata_imp_NA$daytype <- with(fdata_imp_NA, ifelse(fdata_imp_NA$weekday %in% c("Saturday","Sunday"),
                                                  "Weekend",
                                                  "Weekday"))

avg_steps_per_interv_imp_NA_weekday <- aggregate(steps ~ interval,
                                                 fdata_imp_NA[fdata_imp_NA$daytype=="Weekday",],
                                                 FUN=mean)
avg_steps_per_interv_imp_NA_weekend <- aggregate(steps ~ interval,
                                                 fdata_imp_NA[fdata_imp_NA$daytype=="Weekend",],
                                                 FUN=mean)

avg_steps_per_interv_imp_NA_Saturday <- aggregate(steps ~ interval,
                                                 fdata_imp_NA[fdata_imp_NA$weekday=="Saturday",],
                                                 FUN=mean)

avg_steps_per_interv_imp_NA_Sunday <- aggregate(steps ~ interval,
                                                  fdata_imp_NA[fdata_imp_NA$weekday=="Sunday",],
                                                  FUN=mean)


par(mfrow=c(2,1), oma=c(5,5,5,1), mar=c(0,0,0,0))

plot(avg_steps_per_interv_imp_NA_weekday, xaxt="n", ylim=c(0,250), type="l", lwd=3)
abline(v=avg_steps_per_interv_imp_NA_weekday$interval[c(98,112)],lty=3,lwd=3,col="grey40")
text(avg_steps_per_interv_imp_NA_weekday$interval[1], 150, "(A)", adj=0)
legend(avg_steps_per_interv_imp_NA_weekend$interval[260], 200, c("Weekday Average"), lty=1, lwd=2.5, col="black")
# Most activity takes place between 8:05-9:15 hour (grey dotted lines).

plot(avg_steps_per_interv_imp_NA_weekend, ylim=c(0,250), type="l", lwd=3)
lines(avg_steps_per_interv_imp_NA_Saturday, col="red", lwd=2)
lines(avg_steps_per_interv_imp_NA_Sunday, col="blue", lwd=2)
abline(v=avg_steps_per_interv_imp_NA_weekend$interval[c(100,116)],lty=3,lwd=3,col="grey40")
text(avg_steps_per_interv_imp_NA_weekend$interval[1], 150, "(B)", adj=0)
legend(avg_steps_per_interv_imp_NA_weekend$interval[260], 200, c("Weekend Average", "Saturday", "Sunday"), lty=c(1, 1, 1), lwd=c(2.5, 2.5, 2.5), col=c("black", "red", "blue"))
# Peak is less pronounced, and occurs later 8:15-9:35 hour (grey dotted lines).

mtext("More activity during weekdays than in weekends, and more early activity on Saturday than on Sunday.", outer=TRUE, line = 1)
mtext("Average number of steps per 5 min time interval", side=2, line=3, outer=TRUE, srt=90)
mtext("Time Interval (0:00 - 23:55 hours)", side=1, line=3, outer=TRUE)
