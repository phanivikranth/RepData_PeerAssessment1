

unzip("repdata%2Fdata%2Factivity.zip")
activity1 <- read.csv("activity.csv")
hist((aggregate(steps ~ date,activity1,sum,na.action = NULL,na.rm = TRUE))$steps,main = "Histogram of total steps taken per day")
mean((aggregate(steps ~ date,activity1,sum,na.action = NULL,na.rm = TRUE))$steps)
median((aggregate(steps ~ date,activity1,sum,na.action = NULL,na.rm = TRUE))$steps)

plot.ts(aggregate(steps ~ interval,activity1,mean))
plot(aggregate(steps ~ interval,activity1,mean),type = 'l')

aggregate(steps ~ interval,activity1,mean)[which.max(aggregate(steps ~ interval,activity1,mean)$steps),]$interval

sum(rowSums(is.na(activity1)))

# activity1[activity1$date == "2012-10-01",]

activitiy1AddMissingValue  <- transform(activity1, steps = ifelse(is.na(steps), round(mean(steps, na.rm=TRUE)), steps))

sum(rowSums(is.na(activitiy1AddMissingValue)))

hist((aggregate(steps ~ date,activitiy1AddMissingValue,sum,na.action = NULL,na.rm = TRUE))$steps,main = "Histogram of total steps taken per day with missing value filled in")

mean((aggregate(steps ~ date,activitiy1AddMissingValue,sum,na.action = NULL,na.rm = TRUE))$steps)
median((aggregate(steps ~ date,activitiy1AddMissingValue,sum,na.action = NULL,na.rm = TRUE))$steps)

mean((aggregate(steps ~ date,activity1,sum,na.action = NULL,na.rm = TRUE))$steps) - mean((aggregate(steps ~ date,activitiy1AddMissingValue,sum,na.action = NULL,na.rm = TRUE))$steps)

median((aggregate(steps ~ date,activity1,sum,na.action = NULL,na.rm = TRUE))$steps) - median((aggregate(steps ~ date,activitiy1AddMissingValue,sum,na.action = NULL,na.rm = TRUE))$steps)

weekdays(as.Date(activitiy1AddMissingValue$date))

activityfactorweekday0rweekend <- factor(weekdays(as.Date(activitiy1AddMissingValue$date)) %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'),levels = c(FALSE,TRUE),labels = c('weekend','weekday'))

summary(activityfactorweekday0rweekend)

activitymissingtest <- activitiy1AddMissingValue
activitymissingtest$WeekdayorWeekend <- activityfactorweekday0rweekend

library(dplyr)

activityAggregate <- activitymissingtest  %>% 
  +   group_by(WeekdayorWeekend, interval) %>%
  +   summarize(steps3 = mean(steps))

library(ggplot2)


ggplot(activityAggregate, aes(interval,steps3))+geom_line(color="blue")+
  +     facet_wrap(~WeekdayorWeekend, ncol=1)