# Data-driven mobility modeling and simulation (CUSP-GX 9007)
# Homework 2 
# R Visualization and Functions 
# By Shalmali Kulkarni

# read the dataset
data <- read.csv("HWData.csv")

## cummulative sum using built-in funtion
cumsum(data$volume)

## calculate the length of the data 
length_data <- length(data$volume)

## cummulative sum function defnition 

vehicle = c(data$volume)

cummsum <- function(x)
{
  total <- list()
  sum1 = 0
  for (i in seq(1,length_data))
  {
    sum1 <- data[i,1] + sum1
    append(total,sum1,after = length(total))
  }
  return(list(total))
}
cummsum(length_data)

## Add a random time stamp with a 15 minutes interval 
time <- seq.POSIXt(as.POSIXlt(strptime("08:00:00", "%H:%M:%S")), by = "15 min", length.out = 24)
print(time)
## plot of cummulative volume against the time interval
plot(time, cumsum(data$volume),typ = 'l', col = 6, lwd = 2,  xlab = "Time", ylab = "Flow", main = "Time vs Flow",cex.axis=0.8)
grid(8,10)

## get the x co-ordinate for the minimum speed data point.
X = data$volume[data$speed == min(data$speed)]
print(X)
## plot of volume against speed
par(mar = c(4, 4, 4, 4), mgp = c(3, 1, 0))
plot(data$volume, data$speed, typ ='p', xlab = "Volume (no. of vehicles)", ylab = "Speed (mph)", main = "Volume vs Speed", cex.axis=0.8,
     cex.lab=1, col.main ='red')
points(X,min(data$speed),col='red',pch = 23, lwd=3)  ## X is the volume at which the speed is minimum 

