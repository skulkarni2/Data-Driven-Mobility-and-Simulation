# Data-driven mobility modeling and simulation (CUSP-GX 9007)

# HW6 - Shalmali Kulkarni (sck408)
# Simulation in R using simmer


library(simmer)
library(parallel)

## Set parameters
mu <- 30               ## service time for each car in seconds
N_toll <- 4            ## Number of Toll Plaza
lambda <- 2.5          ## Arrival rate for cars
N_Car <- 200           ## Number of cars 
SIM_TIME <- 1000       ## Simulation time in seconds

rho <- 0.0833          ## lambda / mu # 2.5 / 30 = 0.083334

# setup
set.seed(3199772)

## Car Trajectory
car <- trajectory("Car's path") %>%
  select(c("TollBooth1", "TollBooth2", "TollBooth3", "TollBooth4"), policy = "shortest-queue") %>%
  seize_selected(amount=1) %>%
timeout(function() {rnorm(1, mu, 10)}) %>%
  release_selected(amount=1)

TollPlaza.env <- simmer() %>% 
  add_resource("TollBooth1", capacity=1, queue_size=Inf) %>%
  add_resource("TollBooth2", capacity=1, queue_size=Inf) %>%
  add_resource("TollBooth3", capacity=1, queue_size=Inf) %>%
  add_resource("TollBooth4", capacity=1, queue_size=Inf) %>%
  add_generator("Car", car, function() {c(0, rep(lambda, N_Car),-1)}) %>%
  run(SIM_TIME) 

## Get the service time for each car
TollPlaza.env %>% 
  get_mon_arrivals %>%
  dplyr::mutate(service_start_time = end_time - activity_time) %>%
  dplyr::arrange(start_time)

## Get the tollbooth times
df <- 
  TollPlaza.env %>% 
  get_mon_resources %>%
  dplyr::arrange(time)


result <- 
  TollPlaza.env %>% 
  get_mon_arrivals %>%
  dplyr::mutate(waiting_time = end_time - start_time - activity_time)
paste("Average wait for ", sum(result$finished), " completions was ",
      mean(result$waiting_time), "seconds.")

mean(df$queue)


TollPlaza.arrivals <- get_mon_arrivals(TollPlaza.env)
TollPlaza.N <- rho/(1-rho)
TollPlaza.t_system <- TollPlaza.arrivals$end_time - TollPlaza.arrivals$start_time
TollPlaza.T <- TollPlaza.N / lambda

TollBooth1 <- df[df$resource =='"TollBooth1"'] 
TollBooth1$
library(ggplot2)

library(lattice)

dev.new(width=8.4, height=5.0)
par(mfrow=c(1,1))
par(mar=0.1+c(1.7, 1.8, 1.5, 2.0), mgp=c(0.9, 0.1, 0.0)) # Bottom, Left, Top, Right.
plot(NA, NA, xlim=c(0,1000), ylim=c(0,40), xlab="Time", ylab="Queue Length", 
     main=NA, cex.main=0.7, tck=0.01, cex.lab=0.8, xaxt='n', yaxt='n', xaxs="i", yaxs="i")
axis(1, at=seq(0,1000,10), labels=seq(0,1000,10),lty=1, col=1, tck=-0.01, cex.axis=0.7, las=1)
axis(2, at=seq(0,40,10), labels=seq(0,40,10),lty=1, col=1, tck=-0.01, cex.axis=0.7, las=1)
title('Queue Length vs Time')
legend("topright", c("TollBooth-1", "TollBooth-2", "TollBooth-3", "TollBooth-4"), pch=c(19, 18, 5, 4), col=c(1, 2, 3, 4), lty=1, cex=0.8)

## Queue Length
# TollBooth-1
points(df[df$resource == 'TollBooth1',]$time, df[df$resource == 'TollBooth1',]$queue, pch=19,col=1, cex=0.8)
lines(df[df$resource == 'TollBooth1',]$time,df[df$resource == 'TollBooth1',]$queue, lty=1, col=1)

# TollBooth-2
points(df[df$resource == 'TollBooth2',]$time, df[df$resource == 'TollBooth2',]$queue, pch=18, col=2, cex=0.8)
lines(df[df$resource == 'TollBooth2',]$time, df[df$resource == 'TollBooth2',]$queue, lty=1, col=2)

# TollBooth-3
points(df[df$resource == 'TollBooth3',]$time, df[df$resource == 'TollBooth3',]$queue, pch=18, col=3, cex=0.8)
lines(df[df$resource == 'TollBooth3',]$time, df[df$resource == 'TollBooth3',]$queue, lty=1, col=3)


# TollBooth-4
points(df[df$resource == 'TollBooth4',]$time, df[df$resource == 'TollBooth4',]$queue, pch=18, col=4, cex=0.8)
lines(df[df$resource == 'TollBooth4',]$time, df[df$resource == 'TollBooth4',]$queue, lty=1, col=4)


seeds <- c(999, 871005, 779555, 3199772, 87320, 456710, 63892, 874532, 983102, 67230)
queue_len <- c(17.25, 17.25,17.25,17.5, 17.5, 17.75, 17.25, 17.5, 17.25, 17.5)

barplot(queue_len, main = "Average Queue Length", ylim = c(15,18))
