library(ggplot2)
library(PerformanceAnalytics)

##
## Part 1. Signal behaviour
##

## Visualize stable states
source("resources/preLoad.R")

## Signal behaviour Part 1
png(filename="images/signalBehaviour1.png", width = 800, height = 800)
par(mfcol=c(2, 1), cex=1.2)
plot(1:length(pre.n.3$s1.mean)*1/16, pre.n.3$s1.mean, type="l", xlab="Time (sec.)", ylab="Sensor 1", col=3
     , main="Normal 3 (cycle 302A)")
plot(1:length(pre.f.1$s1.mean)*1/16, pre.f.1$s1.mean, type="l", xlab="Time (sec.)", ylab="Sensor 1", col=3
     , main="Failure 1 (cycle 302A)")
dev.off()

## Signal behaviour Part 2
png(filename="images/signalBehaviour2.png", width = 800, height = 800)
par(mfcol=c(2, 1), cex=1.2)
plot(1:length(pre.n.3$s1.mean)*1/16, pre.n.3$s1.mean, type="l", xlab="Time (sec.)", ylab="Sensor 1", col=3
     , main="Normal 3 (cycle 302A)")
points(1:length(pre.n.3$s1.mean)*1/16, pre.n.3$s1.mean
       , pch=ifelse(pre.n.3$s1.sd > quantile(pre.n.3$s1.sd, .05), ".", "*")
       , col=ifelse(pre.n.3$s1.sd > quantile(pre.n.3$s1.sd, .05), 3, 2))
plot(1:length(pre.f.1$s1.mean)*1/16, pre.f.1$s1.mean, type="l", xlab="Time (sec.)", ylab="Sensor 1", col=3
     , main="Failure 1 (cycle 302A)")
points(1:length(pre.f.1$s1.mean)*1/16, pre.f.1$s1.mean
       , pch=ifelse(pre.f.1$s1.sd > quantile(pre.f.1$s1.sd, .05), ".", "*")
       , col=ifelse(pre.f.1$s1.sd > quantile(pre.f.1$s1.sd, .05), 3, 2))
dev.off()

rm(pre.n.3); rm(pre.f.1)

##
## Part 2. Perform visual inspection of classes. Only "Normal 1" is taken into account,
## because it is only one scenario without load.
##

## How many seconds in one split
seconds <- 5

## Load transformed data
source("resources/load.R")

## Feature plot for one sensor
png(filename="images/featureInspection1.png", width = 800, height = 800)
chart.Correlation(faults[, 3:6]
                  , pch="."
                  , col="blue"
                  , labels=c("mean", "std.", "max", "root mean square")
                  , main="Features of Sensor 1")
dev.off()

## Feature plot for different sensors
png(filename="images/featureInspection2.png", width = 800, height = 800)
chart.Correlation(faults[, c(4, 11, 18)]
                  , pch="."
                  , col="blue"
                  , labels=c("sensor 1", "sensor 2", "sensor 3")
                  , main="Std. of Signals for Different Sensors")
dev.off()

## First plot
png(filename="images/visualInspection1.png", width = 800, height = 800)
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
     , main=paste0("Failure 3 (n = ", weights["Failure 3"], ")"))

plot(faults$s1.sd, faults$s2.sd
     , pch="*"
     , col=ifelse(faults$fault == "Normal 1", "Red", "Blue")
     , xlab="Std. of Signal from Sensor 1"
     , ylab="Std. of Signal from Sensor 2"
     , main=paste0("Normal 1 (n = ", weights["Normal 1"], ")"))
dev.off()

## Second plot
png(filename="images/visualInspection2.png", width = 800, height = 800)
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
     , main=paste0("Failure 3 (n = ", weights["Failure 3"], ")"))

plot(faults$s1.peak, faults$s2.peak
     , pch="*"
     , col=ifelse(faults$fault == "Normal 1", "Red", "Blue")
     , xlab="Peak of Signal from Sensor 1"
     , ylab="Peak of Signal from Sensor 2"
     , main=paste0("Normal 1 (n = ", weights["Normal 1"], ")"))
dev.off()
