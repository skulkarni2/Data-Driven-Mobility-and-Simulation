## Homework 1 
## Shalmali Kulkarni
## sck408

## Read the data
data <- read.csv("HW1_Dataset.csv")

## Check the data
head(data)
tail(data)

## Dimensions of the dataset
dim(data)

## Min Actual Speed
min(data$Actual_Speed.mph)

## Min Historical Speed
min(data$Historical_Speed.mph)

## Maximum actual travel time 
max_travel = min(data$Segment_Length.mile.) / min(data$Actual_Speed.mph)
print(paste("The maximum actual travel time is:", max_travel))

## Maximum historical travel time 
max_hist = min(data$Segment_Length.mile.)  / min(data$Historical_Speed.mph) 
print(paste("The maximum historcial travel time is:", max_hist))


## Calculate the travel times and the differencee between the travel times
data$Actual_traveltime <- (data$Segment_Length.mile. / data$Actual_Speed.mph.) / 60
data$historical_traveltime <- (data$Segment_Length.mile. / data$Historical_Speed.mph.) / 60
data$diff_traveltime <- data$Actual_traveltime - data$historical_traveltime

## 3. Varience of each hour

var(data$Actual_traveltime[1:12])
var(data$Actual_traveltime[13:24])
var(data$Actual_traveltime[25:36])

## 4. 
## Visulization
plot(data$Index, data$diff_traveltime, xlab = "Time interval", ylab = "Difference in Travel time",
     pch = 12, cex = 0.7, cex.lab = 1.2, font.lab = 2 , type = "l")
print(paste("The maximum difference in travel time is:", max(data$diff_traveltime)))

total = len(data)
i = 0 
for i<=total 
{
  if data$diff_traveltime[i] == max(data$diff_traveltime)
    {
      print(paste("The maximum travel time difference is in ", data$Local_time[i])
      break
  }  else 
  {
    next
  }
}

## 5. 
## Regression
reg1 <- lm(Actual_traveltime ~ historical_traveltime, data = data)
summary(reg1)

## Visulaize the regression 
plot(data$Actual_traveltime, data$historical_traveltime, xlab = "Actual Travel Time", ylab = "Historical Travel time",
     pch = 12, cex = 0.7, cex.lab = 1.2, font.lab = 2 )
abline(reg1, col = 2, lwd = 2)
