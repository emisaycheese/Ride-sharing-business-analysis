require(sqldf)
require(plyr)
require(ggplot2)
library(reshape2)
library(dplyr)
library(RColorBrewer)

setwd("/Users/ruiqianyang/Desktop/Interview prep/LYFT/LTV")
driver_ids <- read.csv("driver_ids.csv")
ride_ids <- read.csv("ride_ids.csv")
ride_timestamps <- read.csv("ride_timestamps.csv")
head(driver_ids)
head(ride_ids)
head(ride_timestamps)
summary(driver_ids)
nrow(ride_ids)
nrow(ride_timestamps)


#Number of Rides and Active Drivers over time (in weekly buckets)
rides_over_time<-"select week,count(ride_id) as num_rides from
(select r.*,date(r.timestamp) as dt,strftime('%W', date(r.timestamp)) as week
from ride_timestamps r
where event='dropped_off_at'
order by dt
) tmp
group by week"
rot<-sqldf(rides_over_time,stringsAsFactors = FALSE)

head(rot)
rot
#visualize
rot1<-rot[-nrow(rot),]
c <- ggplot(rot1, aes(x=week, y=num_rides, colour=week))
c <- c + geom_point(size=3) +
  geom_line(aes(group=1), size=2, alpha=0.9) +
  geom_smooth(aes(group=1), size=2, colour='turquoise', method = 'lm', se=FALSE) +
  labs(title='How numbers of rides changes over weeks?')
c
#c + theme(panel.background = element_blank())


#num of Active Drivers over time (in weekly buckets, cohorted by onboard week)

active_driver_over_time<-"select week,on_board_week,count(distinct driver_id) as num_drivers from
(select i.driver_id,strftime('%W', date(d.driver_onboard_date)) as on_board_week,r.*,date(r.timestamp) as dt,strftime('%W', date(r.timestamp)) as week
from ride_timestamps r
join ride_ids i
on r.ride_id=i.ride_id and r.event='dropped_off_at'
join driver_ids d
on d.driver_id=i.driver_id
order by dt
) tmp
group by week,on_board_week"
adot<-sqldf(active_driver_over_time,stringsAsFactors = FALSE)

#export to csv
write.csv(adot, file = "active_driver_over_time.csv")


#visualize
cohort_drivers <- data.frame(cohort=c('13(2016-03-28)', '14', '15', '16', '17', '18', '19(2016-05-15)'),
                             week13=c(126,0,0,0,0,0,0),
                             week14=c(113,146,0,0,0,0,0),
                             week15=c(97,131,125,0,0,0,0),
                             week16=c(94,125,115,118,0,0,0),
                             week17=c(89,113,101, 115,108,0,0),
                             week18=c(86,115,101,108, 99,104,0),
                             week19=c(83, 102,92,102,91,100,94),
                             week20=c(78,101,94,90,84, 90,88),
                             week21=c(74,99,89,85,85,84,85),
                             week22=c(78,103,85,89,81,79,75),
                             week23=c(77,103,84,76,74,76,73),
                             week24=c(71,99,82, 79,64,69,66),
                             week25=c(67,90,85,76,64,66,67))
View(cohort_drivers)
cohort_fig2 <- melt(cohort_drivers, id.vars = 'cohort')
colnames(cohort_fig2) <- c('cohort', 'week', 'no_of_drivers')
View(cohort_fig2)
num_driver_pal <-colorRampPalette(c('#e0f3db',
                             '#ccebc5',
                             '#a8ddb5',
                             '#7bccc4',
                             '#4eb3d3',
                             '#2b8cbe',
                             '#0868ac'))
pp <- ggplot(cohort_fig2, aes(x=week, y=no_of_drivers, group=desc(cohort)))
pp <-pp + geom_area(aes(fill = cohort)) +
 scale_fill_manual(values = num_driver_pal(nrow(cohort_drivers))) +
 ggtitle('Active Drivers by Cohort')
pp + theme(panel.background = element_blank())                               

##retention analysis
cohort_drivers1 <- cohort_drivers 
totcols <- ncol(cohort_drivers)
for (i in 1:nrow(cohort_drivers1)) { 
  df <- cohort_drivers1[i,] 
  df <- df[ , !df[]==0] 
  partcols <- ncol(df)
  if (partcols < totcols) df[, c((partcols+1):totcols)] <- 0
  cohort_drivers1[i,] <- df 
}
View(cohort_drivers1)
colnames(cohort_drivers1)<-c('cohort','week0','week1','week2','week3','week4','week5','week6',
                             'week7','week8','week9','week10','week11','week12')
a <- cohort_drivers1[,c(2:totcols)]
b <- cohort_drivers1[,2]   #start number of driver per cohort
retention <- apply(a, 2, function(a) a/b )

#add back cohort column
retention <- data.frame(cohort=(cohort_drivers1$cohort), retention)
View(retention)
retention <- retention[,-2]
cohort_plot <- melt(retention, id.vars = 'cohort')
colnames(cohort_plot) <- c('cohort', 'week', 'retention')
cohort_plot <- filter(cohort_plot, retention != 0)
c <- ggplot(cohort_plot, aes(x=week, y=retention, group=cohort, colour=cohort))
c <-c + geom_line(size=2, alpha=0.5) +
  geom_point(size=3, alpha=1) +
  geom_smooth(aes(group=1), method = 'loess', size=2, colour='turquoise', se=FALSE) +
  labs(title='Cohorts Retention Ratio')
c + scale_colour_brewer(palette='Set3')
#c + scale_colour_brewer(palette='Set3') + theme(panel.background = element_blank())



#4th week Retention Analysis
cohort_plot1 <- filter(cohort_plot, week=='week4') 
c <- ggplot(cohort_plot1, aes(x=cohort, y=retention, colour=cohort))
c <- c + geom_point(size=3, alpha=1) +
geom_line(aes(group=1), size=2, alpha=0.8) +
geom_smooth(aes(group=1), size=2, colour='turquoise', method = 'lm', se=FALSE) +
labs(title='How does the users retain in their 4th week?')
c + scale_colour_brewer(palette="Pastel2") 


#cycle plot
cohort_cycle_plot <- cohort_plot
cohort_cycle_plot <- mutate(cohort_cycle_plot, week_cohort = paste(week, cohort))
cp <- ggplot(cohort_cycle_plot, aes(x=week_cohort, y=retention, group=week, colour=week))
d1 <- filter(cohort_cycle_plot, cohort=='18')
d2 <- filter(cohort_cycle_plot, cohort=='17')
cp <- cp + geom_point(size=3) +
  geom_line(aes(group=week), size=2, alpha=1/2) +
  labs(title='Cycle plot of Cohorts Retention') + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  #geom_line(data=d1, aes(group=1), colour='turquoise', size=2, alpha=0.6) +
 # geom_line(data=d2, aes(group=1), colour='turquoise', size=2, alpha=0.6) +
cp




#Trend of 4-week moving average of time to pickup (time elapsed between
#accepted_at and picked_up_at)

elapsed<-"select week,avg(elapsed) as avg_time from
(select distinct r.ride_id,r.timestamp as t_acc,r2.timestamp as t_pick, 
strftime('%s', r2.timestamp)-strftime('%s', r.timestamp)
as elapsed,
strftime('%W', date(r.timestamp)) as week
from ride_timestamps r
join ride_timestamps r2
on r.ride_id=r2.ride_id and r.event='accepted_at' and r2.event='picked_up_at'
order by 2
) tmp
group by week"
elps<-sqldf(elapsed,stringsAsFactors = FALSE)

ma_acc_pick<-"select wk1 as end_week, avg(avg_time) as MA_sec_elapse from
(select e1.week as wk1,e2.week as wk2,e2.avg_time from
elps e1
join elps e2
on e1.week-e2.week<4 and e1.week-e2.week>=0) tmp
group by 1
"
maap<-sqldf(ma_acc_pick,stringsAsFactors = FALSE)
maap
maap_plot <- ggplot(maap, aes(x=end_week, y=MA_sec_elapse))
maap_plot <- maap_plot + geom_point(size=3) +
  geom_line(aes(group=1), size=2, alpha=1/2,colour='purple') +
  geom_smooth(aes(group=1), method = 'loess', size=2, colour='turquoise', se=FALSE) +
  labs(title='Trend of 4-week moving average of time to pickup',x='End week for moving average',y='Time elapse') + theme(axis.text.x = element_text(angle = 90, hjust = 1))
maap_plot

#change format from factor to datetime
driver_ids$driver_onboard_date=as.POSIXct(driver_ids$driver_onboard_date,format="%Y-%m-%d %H:%M:%S")


#LTV PART



#test
test1<-"select i.ride_id,i.driver_id,strftime('%W', date(d.driver_onboard_date)) as on_board_week,r.*,date(r.timestamp) as dt,strftime('%W', date(r.timestamp)) as week
from ride_ids i
left join ride_timestamps r
on r.ride_id=i.ride_id and r.event='dropped_off_at'
left join driver_ids d
on d.driver_id=i.driver_id
order by dt
"
t1<-sqldf(test1,stringsAsFactors = FALSE)
nrow(t1)
summary(t1)
length(t1$on_board_week[is.na(t1$on_board_week)])
length(t1$dt[is.na(t1$dt)])


test2<-"select distinct ride_id from ride_ids"
t2<-sqldf(test2,stringsAsFactors = FALSE)
nrow(t2)