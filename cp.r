# Load and read data
library(ggplot2)

act <- read.csv("activity.csv")
act$date <- as.POSIXct(act$date, "%Y-%m-%d")
weekday <- weekdays(act$date)
act <- cbind(act,weekday)

# Total number of steps taken sans NA rows. Also, showing
act_ts <- with(act, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(act_ts) <- c("date", "steps")
hist(act_ts$steps, main = "Total number of steps taken per day", xlab = "Total steps taken per day", col = "blue")
mean(act_ts$steps)
median(act_ts$steps)

# Average daily act pattern
avg_da <- aggregate(act$steps, by=list(act$interval), FUN=mean, na.rm=TRUE)
names(avg_da) <- c("interval", "mean")
plot(avg_da$interval, avg_da$mean, type = "l", col="red", xlab="Interval", ylab="Average number of steps", main="Average number of steps per intervals")
#max steps
avg_da[which.max(avg_da$mean), ]$interval

## Imputing missing values
isteps <- avg_da$mean[match(act$interval, avg_da$interval)]
iact <- transform(act, steps = ifelse(is.na(act$steps), yes = isteps, no = act$steps))
ts_imp <- aggregate(steps ~ date, iact, sum)
names(ts_imp) <- c("date", "daily_steps")
hist(ts_imp$daily_steps, col = "lightblue", xlab = "Total steps per day", main = "Total number of steps taken each day")
#mean and median
mean(ts_imp$daily_steps)
median(ts_imp$daily_steps)


# Comparison weekdays and Weekends
act$date <- as.Date(strptime(act$date, format="%Y-%m-%d"))
act$daytype <- sapply(act$date, function(x) {
  if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday") 
  {y <- "Weekend"} else 
  {y <- "Weekday"}
  y
}) 
act_day <- aggregate(steps~interval + daytype, act, mean, na.rm = TRUE)
p<- ggplot(act_day, aes(x = interval , y = steps, color = daytype)) +
  geom_line() +
  labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
  facet_wrap(~daytype, ncol = 1, nrow=2)
print(p)
