# https://github.com/jrnold/ggthemes
# http://www.r-bloggers.com/bot-botany-k-means-and-ggplot2/

library(ggplot2)
library(grid)
library(gridExtra)
library(reshape)
library(scales)
library(lattice)
library(ggthemes)

setwd("~/Dropbox/dev/web/runkeeper-stats")
data = read.csv("cardioActivities.csv", check.names=FALSE)

summary(data)

data <- data[order(data$Date),] # Sort ascending by date
data$ymd <- as.Date(data$Date)
data$month <- as.Date(cut(data$ymd, breaks = "month"))
data$week <- as.Date(cut(data$ymd, breaks = "week")) + 1
data$distance <- data$'Distance (mi)'
data$distance_total <- cumsum(data$distance)
data$speed <- data$'Average Speed (mph)'
data$time_hours <- data$distance/data$'Average Speed (mph)'
data$time_hours_total <- cumsum(data$time_hours)
data$time_mins <- data$time_hours * 60
data$time_mins_total <- cumsum(data$time_mins)
data$qs <- cut(data$distance, breaks = quantile(data$distance), include.lowest=TRUE)

data.qs_monthly <- ddply(data, .(qs, month), function(x) data.frame(distance=sum(x$distance), time_mins=sum(x$time_mins)))
data.qs_monthly$speed <- data.qs_monthly$distance/(data.qs_monthly$time_mins/60)

data.summary <- ddply(data,~qs,summarise,mean_speed=mean(speed),sd_speed=sd(speed),mean_distance=mean(distance),sd_distance=sd(distance))

m <- as.matrix(cbind(data$speed, data$distance),ncol=2)
cl <- kmeans(m,3)

data$cluster <- factor(cl$cluster)
centers <- as.data.frame(cl$centers)

png('speed-vs-distance.png', width=800, height=800)
ggplot(data=data, aes(x=speed, y=distance)) + 
  geom_point() +
  theme_economist() +
  scale_color_economist() +
  geom_abline() +
  theme(axis.text.x = element_text(angle = 80, hjust = 1), plot.title=element_text(hjust=0.5)) +
  xlab('Speed') +
  ylab('Distance') +
  ggtitle("Speed vs Distance")
dev.off()

png('speed-vs-distance-clusters.png', width=800, height=800)
ggplot(data=data, aes(x=speed, y=distance, color=cluster)) + 
  theme_economist() +
  scale_color_economist() +
  geom_point(legend=FALSE) +  
  geom_point(data=centers, aes(x=V1,y=V2, color='Center'), size=52, alpha=.3, legend=FALSE) +
  theme(axis.text.x = element_text(angle = 80, hjust = 1), plot.title=element_text(hjust=0.5)) +
  xlab('Speed') +
  ylab('Distance') +
  ggtitle("Speed vs Distance - Clustered")
dev.off()

png('speed-month-qs.png',width = 800, height = 600)
ggplot(data = data.qs_monthly,
  aes(month, speed)) +
  geom_line() +
  facet_grid(qs ~ .) +  
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "1 month") +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 80, hjust = 1), plot.title=element_text(hjust=0.5)) +
  xlab('Month') +
  ylab('Distance') +
  ggtitle("Speed by Month")
dev.off()

png('distance-month.png',width = 800, height = 600)
ggplot(data = data,
  aes(month, distance)) +
  stat_summary(fun.y = sum,
    geom = "bar") +
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "1 month") +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 80, hjust = 1), plot.title=element_text(hjust=0.5)) +
  xlab('Month') +
  ylab('Distance') +
  ggtitle("Distance by Month")
dev.off()

png('distance-week.png',width = 800, height = 600)
ggplot(data = data,
  aes(week, distance)) +
  stat_summary(fun.y = sum,
    geom = "bar") +
  scale_x_date(
    labels = date_format("%Y-%m-%d"),
    breaks = "4 week") +
  xlab('Week') +
  ylab('Distance') +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 80, hjust = 1), plot.title=element_text(hjust=0.5)) +
  xlab('Week') +
  ylab('Distance') +
  ggtitle("Distance by Week")
dev.off()

png('speed-distribution-qs.png',width = 800, height = 600)
ggplot(data, aes(speed, fill=qs)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept=mean_speed), data=data.summary) +
  facet_grid(qs ~ .) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 80, hjust = 1), plot.title=element_text(hjust=0.5)) +
  xlab('Speed') +
  ylab('Density') +
  ggtitle("Speed Distribution by Distance")
dev.off()

png('distance-cumulative.png',width = 800, height = 600)
ggplot(data=data, aes(ymd, distance_total)) +
  stat_summary(fun.y = sum, geom = "line") +
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "1 month") +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 80, hjust = 1), plot.title=element_text(hjust=0.5)) +  
  xlab('Month') +
  ylab('Distance') +
  ggtitle("Cumulative distance")
dev.off()

png('speed-daily.png',width = 800, height = 600)
ggplot(data=data, aes(ymd, speed)) +
  stat_summary(fun.y = sum, geom = "line") +
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "1 month") +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 80, hjust = 1), plot.title=element_text(hjust=0.5)) +  
  xlab('Month') +
  ylab('Speed') +
  ggtitle("Speed by Run")
dev.off()

png('time-cumulative.png',width = 800, height = 600)
ggplot(data=data, aes(ymd, time_hours_total)) +
  stat_summary(fun.y = sum, geom = "line") +
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "1 month") +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 80, hjust = 1), plot.title=element_text(hjust=0.5)) +  
  xlab('Month') +
  ylab('Time (hours)') +
  ggtitle("Cumulative Time")
dev.off()

unused <- function() {
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

  ggplot(data, aes(speed, fill=qs)) +
    geom_density(alpha = 0.5) +
    facet_grid(qs ~ .) +
    theme_economist()

  histogram( ~ distance + time_mins, data = data)
}