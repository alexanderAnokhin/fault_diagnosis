library(ggplot2)
library(PerformanceAnalytics)

##
## Part 1. Signal behaviour
##

## How many seconds in one split
seconds <- 1

## Load transformed data
source("resources/load.R")

## Signal behaviour 
s1.f1 <- f.1$s1.mean[f.1$cycle=="301A"]
s1.n3 <- n.3$s1.mean[n.3$cycle=="301A"]

png(filename="images/signalBehaviour.png", width = 600, height = 600)
par(cex=1.2)
plot(s1.n3, type="l", xlab="Time (sec.)", ylab="Sensor 1", col=3, lwd=2)
points(s1.f1, type="l", xlab="Time (sec.)", ylab="Sensor 1", col=2, lwd=2)
legend("topright", legend=c("Normal 3", "Failure 1"), lty=1, col=c(3, 2), lwd=2)
dev.off()

rm(s1.f1); rm(s1.n3)

##
## Part 2. Perform visual inspection of classes. Only "Normal 1" is taken into account,
## because it is only one scenario without load.
##

## How many seconds in one split
seconds <- 5

## Load transformed data
source("resources/load.R")

## Feature plot for one sensor
png(filename="images/featureInspection1.png", width = 600, height = 600)
chart.Correlation(faults[, 3:6]
                  , pch="."
                  , col="lightblue"
                  , labels=c("mean", "std.", "max", "root mean square")
                  , main="Features of Sensor 1")
dev.off()

## Feature plot for different sensors
png(filename="images/featureInspection2.png", width = 600, height = 600)
chart.Correlation(faults[, c(4, 11, 18)]
                  , pch="."
                  , col="lightblue"
                  , labels=c("sensor 1", "sensor 2", "sensor 3")
                  , main="Std. of Signals for Different Sensors")
dev.off()

## First plot
png(filename="images/visualInspection1.png", width = 600, height = 600)
par(mfcol = c(2, 2), cex=1.2)
plot(faults$s1.sd, faults$s2.sd
     , pch="*"
     , col=ifelse(faults$fault == "Failure 1", "Red", "Blue")
     , xlab="Std. of Signal from Sensor 1"
     , ylab="Std. of Signal from Sensor 2"
     , main=paste0("Failure 1 (n = ", weights["Failure 1"], ")"))

plot(faults$s1.sd, faults$s2.sd
     , pch="*"
     , col=ifelse(faults$fault == "Failure 2", "Red", "Blue")
     , xlab="Std. of Signal from Sensor 1"
     , ylab="Std. of Signal from Sensor 2"
     , main=paste0("Failure 2 (n = ", weights["Failure 2"], ")"))

plot(faults$s1.sd, faults$s2.sd
     , pch="*"
     , col=ifelse(faults$fault == "Failure 3", "Red", "Blue")
     , xlab="Std. of Signal from Sensor 1"
     , ylab="Std. of Signal from Sensor 2"
     , main=paste0("Failure 1 (n = ", weights["Failure 3"], ")"))

plot(faults$s1.sd, faults$s2.sd
     , pch="*"
     , col=ifelse(faults$fault == "Normal 1", "Red", "Blue")
     , xlab="Std. of Signal from Sensor 1"
     , ylab="Std. of Signal from Sensor 2"
     , main=paste0("Normal 1 (n = ", weights["Normal 1"], ")"))
dev.off()

## Second plot
png(filename="images/visualInspection2.png", width = 600, height = 600)
par(mfcol = c(2, 2), cex=1.2)
plot(faults$s1.peak, faults$s2.peak
     , pch="*"
     , col=ifelse(faults$fault == "Failure 1", "Red", "Blue")
     , xlab="Peak of Signal from Sensor 1"
     , ylab="Peak of Signal from Sensor 2"
     , main=paste0("Failure 1 (n = ", weights["Failure 1"], ")"))

plot(faults$s1.peak, faults$s2.peak
     , pch="*"
     , col=ifelse(faults$fault == "Failure 2", "Red", "Blue")
     , xlab="Peak of Signal from Sensor 1"
     , ylab="Peak of Signal from Sensor 2"
     , main=paste0("Failure 2 (n = ", weights["Failure 2"], ")"))

plot(faults$s1.peak, faults$s2.peak
     , pch="*"
     , col=ifelse(faults$fault == "Failure 3", "Red", "Blue")
     , xlab="Peak of Signal from Sensor 1"
     , ylab="Peak of Signal from Sensor 2"
     , main=paste0("Failure 1 (n = ", weights["Failure 3"], ")"))

plot(faults$s1.peak, faults$s2.peak
     , pch="*"
     , col=ifelse(faults$fault == "Normal 1", "Red", "Blue")
     , xlab="Peak of Signal from Sensor 1"
     , ylab="Peak of Signal from Sensor 2"
     , main=paste0("Normal 1 (n = ", weights["Normal 1"], ")"))
dev.off()
