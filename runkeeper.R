# https://github.com/jrnold/ggthemes
library(ggthemes)
library(ggplot2)
library(grid)
library(gridExtra)
library(RMySQL)
library(reshape)
library(scales)
library(lattice)

data = read.csv("cardioActivities.csv", check.names=FALSE)

summary(data)

# data <- data[order(nrow(data):1),] # Reverse
data <- data[order(data$Date),]
data$ymd <- as.Date(data$Date)
data$month <- as.Date(cut(data$ymd, breaks = "month"))
data$week <- as.Date(cut(data$ymd, breaks = "week")) + 1
data$distance <- data$'Distance (mi)'
data$distance_total <- cumsum(data$distance)
data$speed <- data$'Average Speed (mph)'
data$time_hours <- data$distance/data$'Average Speed (mph)'
data$time_mins <- data$time_hours * 60
data$time_mins_total <- cumsum(data$time_mins)
data$qs <- cut(data$distance, breaks = quantile(data$distance), include.lowest=TRUE)

subset(data, is.na(data$qs))

ggplot(data = data,
  aes(month, distance)) +
  stat_summary(fun.y = sum,
    geom = "bar") +
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "1 month")

ggplot(data = data,
  aes(week, distance)) +
  stat_summary(fun.y = sum,
    geom = "bar") +
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "1 week")

ggplot(data=data,
  aes(x=month, y=distance, group=qs, color=qs)) +
  stat_summary(fun.y = sum, geom = "line") +
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "1 month")

ggplot(data = data,
  aes(month, distance)) +
  stat_summary(fun.y = sum, geom = "bar") +
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "1 month") +
  facet_grid(qs ~ .)

ggplot(data = data,
  aes(month, time_mins)) +
  stat_summary(fun.y = mean, geom = "line") +
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "1 month") +
  facet_grid(qs ~ .)

ggplot(data, aes(distance)) +
  geom_density(alpha = 0.2)

ggplot(data, aes(distance, fill=qs)) +
  geom_density(alpha = 0.2)

ggplot(data, aes(speed, fill=qs)) +
  geom_density(alpha = 0.2)

ggplot(data, aes(speed)) +
  geom_density(alpha = 0.2) +
  facet_grid(qs ~ .)

histogram( ~ distance + time_mins ,data = data)

ggplot(data=data, aes(ymd, distance_total)) +
  stat_summary(fun.y = sum, geom = "line") +
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 80, hjust = 1)) + xlab('Month')
  + theme_tufte()

ggplot(data=data, aes(ymd, time_mins_total)) +
  stat_summary(fun.y = sum, geom = "line")

ggplot(data=data, aes(ymd, speed)) +
  stat_summary(fun.y = sum, geom = "line")

