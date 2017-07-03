# Data-driven mobility modeling and simulation (CUSP-GX 9007)

# HW5 - Shalmali Kulkarni (sck408)
# GM Car Following Model
rm(list=ls())

####1. Parameter Setting ####
## Initial Values
# Leading vehicle
dt <- 0.5  # Time interval                  
Xnt <- 78 # Initial position
Vnt <- 16 # Initial speed
Ant <- 0 # Initial Accelerlation
DeltT <- 0.5  # Reaction time                #change


# Following vehicle - 1
Xn1t <- 50
Vn1t <- 16
An1t <- 0

# Following vehicle - 2
Xn2t <- 20
Vn2t <- 16
An2t <- 0

# Following vehicle - 3
Xn3t <- 0
Vn3t <- 16
An3t <- 0


## Assumed Parameters
m <- 0.15 # from -2 to 2
L <- 1  # from -1 to 4
Alm <- 13 # sensitivity coefficient        #change
Time <- 1 #time interval                 #already changed


####2. Define GM Car Following Model ####
GMCarFollowing <- function(period, Acceleration)
{
  
  for(i in (length(Result[,1])+1):((period)/dt))
    
  {
    Ant[i] <- Acceleration
    Vnt[i] <- Result[i-1,2] + Result[i-1,1]*dt
    Xnt[i] <- Result[i-1,3]  + Result[i-1,2] * dt + 0.5*Result[i-1,1] *(dt)^2
    
    Vn1t[i] <- Result[i-1,5] + Result[i-1,4]*dt
    Xn1t[i] <- Result[i-1,6]  + Result[i-1,5] * dt + 0.5*Result[i-1,4] *(dt)^2
    
    Vn2t[i] <- Result[i-1,8] + Result[i-1,7]*dt
    Xn2t[i] <- Result[i-1,9]  + Result[i-1,8] * dt + 0.5*Result[i-1,7] *(dt)^2
    
    Vn3t[i] <- Result[i-1,11] + Result[i-1,10]*dt
    Xn3t[i] <- Result[i-1,12]  + Result[i-1,11] * dt + 0.5*Result[i-1,10] *(dt)^2
    
    
    if(abs((Result[max(i-DeltT/dt,1), 2] - Result[max(i-DeltT/dt,1), 5])) > 0)
    {
      An1t[i] <- (Alm* (Vn1t[i])^m /(Result[max(i-DeltT/dt,1),3]-Result[max(i-DeltT/dt,1),6])^L) * (Result[max(i-DeltT/dt,1),2]  - Result[max(i-DeltT/dt,1),5])
     
    }else{
      An1t[i] <- 0
    }
    
    if(abs((Result[max(i-DeltT/dt,1), 5] - Result[max(i-DeltT/dt,1), 8])) > 0)
    {
      An2t[i] <- (Alm* (Vn2t[i])^m /(Result[max(i-DeltT/dt,1),6]-Result[max(i-DeltT/dt,1),9])^L) * (Result[max(i-DeltT/dt,1),5]  - Result[max(i-DeltT/dt,1),8])
      
    }else{
      An2t[i] <- 0
    }
    
    if(abs((Result[max(i-DeltT/dt,1), 8] - Result[max(i-DeltT/dt,1), 11])) > 0)
    {
      An3t[i] <- (Alm* (Vn3t[i])^m /(Result[max(i-DeltT/dt,1),9]-Result[max(i-DeltT/dt,1),12])^L) * (Result[max(i-DeltT/dt,1),8]  - Result[max(i-DeltT/dt,1),11])
      
    }else{
      An3t[i] <- 0
    }
    
    Time[i] <- i
    Result <- rbind(Result, cbind(Ant[i], Vnt[i], Xnt[i], An1t[i], Vn1t[i], Xn1t[i], An2t[i], Vn2t[i], Xn2t[i], An3t[i], Vn3t[i], Xn3t[i], Time[i]))
  }
  
  return(Result)
  
}

####2. Define GM Car Following Model ####
GMCarFollowing2 <- function(period, Acceleration)
{
  
  for(i in (length(Result[,1])+1):((period)/dt))
    
  {
    Ant[i] <- Acceleration
    Vnt[i] <- Result[i-1,2] + Result[i-1,1]*dt
    Xnt[i] <- Result[i-1,3]  + Result[i-1,2] * dt + 0.5*Result[i-1,1] *(dt)^2
    
    Vn1t[i] <- Result[i-1,5] + Result[i-1,4]*dt
    Xn1t[i] <- Result[i-1,6]  + Result[i-1,5] * dt + 0.5*Result[i-1,4] *(dt)^2
    
    
    if(abs((Result[max(i-DeltT/dt,1), 2] - Result[max(i-DeltT/dt,1), 5])) > 0)
    {
      An1t[i] <- (Alm* (Vn1t[i])^m /(Result[max(i-DeltT/dt,1),3]-Result[max(i-DeltT/dt,1),6])^L) * (Result[max(i-DeltT/dt,1),2]  - Result[max(i-DeltT/dt,1),5])
      
    }else{
      An1t[i] <- 0
    }
    
    Time[i] <- i
    Result <- rbind(Result, cbind(Ant[i], Vnt[i], Xnt[i], An1t[i], Vn1t[i], Xn1t[i], An2t[i], Vn2t[i], Xn2t[i], An3t[i], Vn3t[i], Xn3t[i], Time[i]))
  }
  
  return(Result)
  
}


Result <- cbind(Ant, Vnt, Xnt, An1t, Vn1t, Xn1t, An2t, Vn2t, Xn2t, An3t, Vn3t, Xn3t, Time)
Result <- GMCarFollowing(2, 0)
Result <- GMCarFollowing(4, 1)
Result <- GMCarFollowing(6, -1)
Result <- GMCarFollowing(8, 0.2)
Result <- GMCarFollowing(10, 0.2)
Result <- GMCarFollowing(12, 0.2)
Result <- GMCarFollowing(13, 0.2)
Result <- GMCarFollowing2(14, 0)
Result <- GMCarFollowing2(16, 0)
Result <- GMCarFollowing2(18, 0)
Result <- GMCarFollowing2(20, 0)
Result <- GMCarFollowing2(21, 0)
Result


####3. Visulization ####
dev.new(width=8.4, height=5.0)
par(mfrow=c(1,1))
par(mar=0.1+c(1.7, 1.8, 1.5, 2.0), mgp=c(0.9, 0.1, 0.0)) # Bottom, Left, Top, Right.
grid(50,500)
plot(NA, NA, xlim=c(0,50), ylim=c(0,500), xlab="Time Step (Interval=0.5 s)", ylab="Distance(m)", 
     main="Time Space Diagram", cex.main=0.7, tck=0.01, cex.lab=0.8, xaxt='n', yaxt='n', xaxs="i", yaxs="i")
axis(1, at=seq(0,50,2), labels=seq(0,50,2),lty=1, col=1, tck=-0.01, cex.axis=0.7, las=1)
axis(2, at=seq(0,500,50), labels=seq(0,500,50),lty=1, col=1, tck=-0.01, cex.axis=0.7, las=1)
#axis(2, at=seq(0,400,10), labels=seq(0,400,10),lty=1, col=1, tck=-0.01, cex.axis=0.7)

## Trajectory
# Leading vehicle
points(Result[,13], Result[,3], pch=19,col=1, cex=0.8)
lines(Result[,13], Result[,3], lty=1, col=1)

# Following vehicle - 1
points(Result[,13], Result[,6], pch=18, col=2, cex=0.8)
lines(Result[,13], Result[,6], lty=1, col=2)

# Following vehicle - 2
points(Result[,13], Result[,9], pch=18, col=3, cex=0.8)
lines(Result[,13], Result[,9], lty=1, col=3)

# Following vehicle - 3
points(Result[,13], Result[,12], pch=18, col=4, cex=0.8)
lines(Result[,13], Result[,12], lty=1, col=4)

legend("topleft",c("Leading Vehicle","Following Vehicle -1", "Following Vehicle -2", "Following Vehicle -3"),col=c(1,2,3,4),lty=1,lwd=2,cex=0.8)


