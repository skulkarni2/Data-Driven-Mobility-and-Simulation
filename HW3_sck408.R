# Data-driven mobility modeling and simulation (CUSP-GX 9007)
# Homework 3 
# Statistical Applications using R 
# By Shalmali Kulkarni

## Question 1: 
## Enter the data
procedure1 = c(22.6,27.4,32.2,37.6,42.8,37.1,32.4,27.3,32.2,24.4,42.5,37.6,22.9,47.6)
procedure2 = c(22.6,32.8,30.9,42.3,37.4,32.6,37.6,22.7,37.1,22.4,47.5,32.3,27.0,42.1)

## 1.a
## calculate and print the mean of both the data
print(paste('The mean of procedure 1 is:',mean(procedure1)))
print(paste('The mean of procedure 2 is:',mean(procedure2)))

## calculate and print the standard deviation of both the data
print(paste('The standard deviation of procedure 1 is:',sd(procedure1)))
print(paste('The standard deviation of procedure 2 is:',sd(procedure2)))

## 1.b 
## calculate the mean standard error
se = sd(procedure1)/sqrt(length(procedure1))
print(paste('The mean standard error of procedure 1 is:',se))
#95% confidence intervals of the mean is 2 standard mean error
c(mean(procedure1)-2*se,mean(procedure1)+2*se)

## 1.c
## Null Hypothesis: Mean of procedure 1 and 
## Alternate Hypothesis : Difference in mean of procedure 1 and mean of procedure 2 is zero
## Assume variances are equal
n <- 10
sp <- sqrt(((n-1)*var(procedure1)+(n-1)*var(procedure2))/(n+n-2))
t <- (mean(procedure1)-mean(procedure2))/(sp*sqrt(1/n+1/n))
df <- n+n-2
qt(c(.05,.95), df=df)  # critical values
t.test(procedure1,procedure2, var.equal = TRUE)

## 1.d
cor(procedure1, procedure2)
t.test(procedure1, procedure2, var.equal = TRUE, paired = TRUE)

## 1.e Regression results
## Null Hypothsis : Procedure 1 and 2 have a linear relationship
## Alternate Hypothesis : Procedure 1 and 2 dont have a linear relationship 
mod <- lm(procedure1~procedure2)
summary(mod)
mod$coefficients
mod$fitted.values


## Question 2: 
library(LaplacesDemon)
speed_data <- read.csv('speed.csv')
std <- sd(speed_data$time.mean.speed)
for (i in 1:100){
  speed_data$vs[i] = speed_data$time.mean.speed[i] - ((std)/speed_data$time.mean.speed[i]) 
}

coeff <- cor(speed_data$space.mean.speed, speed_data$vs, method = 'pearson')
# chisq.test(speed_data$space.mean.speed, speed_data$vs)
print(coeff)

