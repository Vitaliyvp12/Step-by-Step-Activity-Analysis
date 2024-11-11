data <- read.csv("C:/Users/VV/OneDrive/Документы/activity.csv")

# Total steps taken per day (ignoring NAs)
total_steps_per_day <- aggregate(steps ~ date, data, sum, na.rm=TRUE)

# Histogram of the total number of steps taken per day
hist(total_steps_per_day$steps, main="Histogram of Total Steps Per Day", xlab="Steps")

# Calculating the mean and median number of steps per day
mean_steps <- mean(total_steps_per_day$steps)
median_steps <- median(total_steps_per_day$steps)

mean_steps
median_steps

# Average number of steps for each 5-minute interval
average_steps_per_interval <- aggregate(steps ~ interval, data, mean, na.rm=TRUE)

# Plotting a time series graph
plot(average_steps_per_interval$interval, average_steps_per_interval$steps, type="l", 
     main="Average Steps for Each 5-Minute Interval", 
     xlab="Interval", ylab="Average Steps")

# Finding the interval with the maximum number of steps
max_interval <- average_steps_per_interval[which.max(average_steps_per_interval$steps), ]
max_interval

# Number of missing values
na_count <- sum(is.na(data$steps))

# Filling in missing values with the mean for that interval
data_filled <- data
for (i in 1:nrow(data_filled)) {
  if (is.na(data_filled$steps[i])) {
    interval_val <- data_filled$interval[i]
    data_filled$steps[i] <- average_steps_per_interval$steps[average_steps_per_interval$interval == interval_val]
  }
}

# Recalculating total steps per day with filled values
total_steps_per_day_filled <- aggregate(steps ~ date, data_filled, sum)

# Histogram with filled values
hist(total_steps_per_day_filled$steps, main="Histogram of Total Steps Per Day (With Filled Values)", xlab="Steps")

# Mean and median after filling missing values
mean_steps_filled <- mean(total_steps_per_day_filled$steps)
median_steps_filled <- median(total_steps_per_day_filled$steps)

mean_steps_filled
median_steps_filled

# Convert the date column to Date format
data_filled$date <- as.Date(data_filled$date, format="%Y-%m-%d")

# Add a factor variable for weekdays and weekends
data_filled$day_type <- ifelse(weekdays(data_filled$date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

# Average number of steps for each interval by day type
average_steps_by_day_type <- aggregate(steps ~ interval + day_type, data_filled, mean)

# Panel plot for weekdays and weekends
library(ggplot2)
ggplot(average_steps_by_day_type, aes(x=interval, y=steps, color=day_type)) +
  geom_line() +
  facet_wrap(~day_type, ncol=1) +
  labs(title="Average Number of Steps on Weekdays and Weekends", x="Interval", y="Average Steps")
