# Data-driven mobility modeling and simulation (CUSP-GX 9007)
# Homework 4
# Cumulative and Oblique Curve 
# By Shalmali Kulkarni


# Set up your work directory that include your datasets
# Include a libraty to read txt.gz files
library("utils")  

## Question 1:
## Read the file 
Filename <- "d04_text_station_5min_2013_12_18.txt.gz"
PeMS <- read.delim(gzfile(Filename), sep="," ,header=FALSE)
dim(PeMS)
PeMS[1,]

# Selection a loop detector station with a given ID = 404922
SelectedID <- 404922   
StationData <- subset(PeMS,PeMS [,2]==SelectedID)
StationData[1,]
dim(StationData)
StationData2[,12]
# Selection a loop detector station with a given ID = 404916
SelectedID2 <- 404916   
StationData2 <- subset(PeMS,PeMS [,2]==SelectedID2)
StationData2[1,]
dim(StationData2)
## Calculate the average occupancy 
avg_occ <- (mean(StationData2[,11])*100)
## Calculate the mean speed
ave_speed <- mean(StationData[,12])
print(ave_speed)

ave_speed2 <- mean(StationData2[,12])
print(ave_speed2)

# Show multiple plots on the same window
dev.off()
dev.new(width=8.4, height=7.0)
par(mfrow=c(3,2))
# Note: ts()--convert the timestamp to a time sequence
# Note: max()--find the max value of a set of data
TimeInterval <- 5
LowTime <- 0
UpTime <- length(Filename)*24*60/TimeInterval
plot(ts(StationData[,1]),StationData[,10],type="l",xlim=c(LowTime,UpTime),ylim=c(0,max(StationData[,10])*1.1),xlab="Timestamp (Interval=5min)",ylab="Flow (veh/5min)",main="Temporal Flow Distribution for Sensor ID= 404922",col="blue")
plot(ts(StationData[,1]),StationData2[,10],type="l",xlim=c(LowTime,UpTime),ylim=c(0,max(StationData[,10])*1.1),xlab="Timestamp (Interval=5min)",ylab="Flow (veh/5min)",main="Temporal Flow Distribution for Sensor ID= 404916",col="red")
plot(ts(StationData[,1]),StationData[,12],type="l",xlim=c(LowTime,UpTime),ylim=c(0,100),xlab="Timestamp (Interval=5min)",ylab="Average Speed (mph)",main="Temporal Average Speed Distribution for Sensor ID= 404922",col="blue")
plot(ts(StationData[,1]),StationData2[,12],type="l",xlim=c(LowTime,UpTime),ylim=c(0,100),xlab="Timestamp (Interval=5min)",ylab="Average Speed (mph)",main="Temporal Average Speed Distribution for Sensor ID= 404916",col="red")
plot(ts(StationData[,1]),StationData[,11]*100,type="l",xlim=c(LowTime,UpTime),ylim=c(0,30),xlab="Timestamp (Interval=5min)",ylab="Average Occupancy (%)",main="Temporal Average Occupancy Distribution for Sensor ID= 404922",col="blue")
abline(h=avg_occ,col='black',lty=2)
plot(ts(StationData[,1]),StationData2[,11]*100,type="l",xlim=c(LowTime,UpTime),ylim=c(0,30),xlab="Timestamp (Interval=5min)",ylab="Average Occupancy (%)",main="Temporal Average Occupancy Distribution for Sensor ID= 404916",col="red")
abline(h=avg_occ,col='black',lty=2)

## Question 2: 
## Cumulative and Oblique Curves #

## Flow Cummulative and Oblique plots
dev.off()
dev.new(width=8.4, height=7.0)
par(mfrow=c(2,2))
# Assume capacity is 1000 for each lane
lanes=5  # Number of lanes of the roadway at a given direction
obliqueQ <- 1000*lanes/12*(1:288)
plot(ts(StationData[,1]),cumsum(StationData[,10]),type="l",xlim=c(LowTime,UpTime),ylim=c(0,max(obliqueQ,cumsum(StationData[,10]))*1.5),xlab="Timestamp (Interval=5min)",ylab="Cumulative Flow N (t) (veh/5min)",main="Time vs. Flow (Sensor ID-404922)",col="blue")
lines(1:288,obliqueQ,lwd=2,col=2)
plot(ts(StationData[,1]),cumsum(StationData[,10])-obliqueQ,type="l",xlim=c(LowTime,UpTime),ylim=c(min(cumsum(StationData[,10])-obliqueQ)*1.1,max(cumsum(StationData[,10])-obliqueQ)*1.1),xlab="Timestamp (Interval=5min)",ylab="Oblique Flow: N(t)-q*t (veh/5min)",main="Time vs. Flow (Sensor ID-404922)",col="blue")
## plot for station ID 404916
plot(ts(StationData2[,1]),cumsum(StationData2[,10]),type="l",xlim=c(LowTime,UpTime),ylim=c(0,max(obliqueQ,cumsum(StationData2[,10]))*1.5),xlab="Timestamp (Interval=5min)",ylab="Cumulative Flow N (t) (veh/5min)",main="Time vs. Flow (Sensor ID-404916)",col="blue")
lines(1:288,obliqueQ,lwd=2,col=2)
plot(ts(StationData2[,1]),cumsum(StationData2[,10])-obliqueQ,type="l",xlim=c(LowTime,UpTime),ylim=c(min(cumsum(StationData2[,10])-obliqueQ)*1.1,max(cumsum(StationData2[,10])-obliqueQ)*1.1),xlab="Timestamp (Interval=5min)",ylab="Oblique Flow: N(t)-q*t (veh/5min)",main="Time vs. Flow (Sensor ID-404916)",col="blue")

## Occupancy Cummulative and Oblique plots
# Assume normal occupancy is 6% i.e the average occupancy for the data
dev.off()
dev.new(width=8.4, height=7.0)
par(mfrow=c(2,2))
obliqueO <- 0.06*(1:288)*100
plot(ts(StationData[,1]),cumsum(StationData[,11])*100,type="l",xlim=c(LowTime,UpTime),ylim=c(0,max(cumsum(StationData[,11]))*100*1.5),xlab="Timestamp (Interval=5min)",ylab="Cumulative Occupancy O(t) (%)",main="Time vs. Average Occupancy (Sensor ID-404922)",col="blue")
lines(1:288,obliqueO,lwd=2,col=2)
plot(ts(StationData[,1]),cumsum(StationData[,11])*100-obliqueO,type="l",xlim=c(LowTime,UpTime),ylim=c(min(cumsum(StationData[,11])*100-obliqueO)*1.1,max(cumsum(StationData[,11])*100-obliqueO)*1.1),xlab="Timestamp (Interval=5min)",ylab="Oblique Occupancy: O(t)-occ*t (%)",main="Time vs. Average Occupancy (Sensor ID-404922)",col="blue")
## plot for station ID 404916
plot(ts(StationData2[,1]),cumsum(StationData2[,11])*100,type="l",xlim=c(LowTime,UpTime),ylim=c(0,max(cumsum(StationData2[,11]))*100*1.5),xlab="Timestamp (Interval=5min)",ylab="Cumulative Occupancy O(t) (%)",main="Time vs. Average Occupancy (Sensor ID-404916)",col="blue")
lines(1:288,obliqueO,lwd=2,col=2)
plot(ts(StationData2[,1]),cumsum(StationData2[,11])*100-obliqueO,type="l",xlim=c(LowTime,UpTime),ylim=c(min(cumsum(StationData2[,11])*100-obliqueO)*1.1,max(cumsum(StationData2[,11])*100-obliqueO)*1.1),xlab="Timestamp (Interval=5min)",ylab="Oblique Occupancy: O(t)-occ*t (%)",main="Time vs. Average Occupancy (Sensor ID-404916)",col="blue")


