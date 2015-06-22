setwd("~/Documents/Uni_Munster/DA in Spare Parts Management/fault_diagnosis")

library(ggplot2)
library(PerformanceAnalytics)
library(fastICA)
library(mlr)
library(kernlab)
library(pso)

## Load transformed data
source("load.R")

## Set number of folds for cross-validation
folds <- 20

## Part 1
## Perform visual inspection of classes. Only "Normal 1" is taken into account,
## because it is only one scenario without load.
## 

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
par(mfcol = c(2, 2))
plot(faults$s1.sd, faults$s2.sd
     , pch=ifelse(faults$fault == "Failure 1", "*", ".")
     , col=ifelse(faults$fault == "Failure 1", "Red", "Blue")
     , xlab="Std. of signal from sensor 1"
     , ylab="Std. of signal from sensor 2"
     , main=paste0("Failure 1 (n = ", weights["Failure 1"], ")"))

plot(faults$s1.sd, faults$s2.sd
     , pch=ifelse(faults$fault == "Failure 2", "*", ".")
     , col=ifelse(faults$fault == "Failure 2", "Red", "Blue")
     , xlab="Std. of signal from sensor 1"
     , ylab="Std. of signal from sensor 2"
     , main=paste0("Failure 2 (n = ", weights["Failure 2"], ")"))

plot(faults$s1.sd, faults$s2.sd
     , pch=ifelse(faults$fault == "Failure 3", "*", ".")
     , col=ifelse(faults$fault == "Failure 3", "Red", "Blue")
     , xlab="Std. of signal from sensor 1"
     , ylab="Std. of signal from sensor 2"
     , main=paste0("Failure 1 (n = ", weights["Failure 3"], ")"))

plot(faults$s1.sd, faults$s2.sd
     , pch=ifelse(faults$fault == "Normal 1", "*", ".")
     , col=ifelse(faults$fault == "Normal 1", "Red", "Blue")
     , xlab="Std. of signal from sensor 1"
     , ylab="Std. of signal from sensor 2"
     , main=paste0("Normal 1 (n = ", weights["Normal 1"], ")"))
dev.off()

## Second plot
png(filename="images/visualInspection2.png", width = 600, height = 600)
par(mfcol = c(2, 2))
plot(faults$s1.peak, faults$s2.peak
     , pch=ifelse(faults$fault == "Failure 1", "*", ".")
     , col=ifelse(faults$fault == "Failure 1", "Red", "Blue")
     , xlab="Peak of signal from sensor 1"
     , ylab="Peak of signal from sensor 2"
     , main=paste0("Failure 1 (n = ", weights["Failure 1"], ")"))

plot(faults$s1.peak, faults$s2.peak
     , pch=ifelse(faults$fault == "Failure 2", "*", ".")
     , col=ifelse(faults$fault == "Failure 2", "Red", "Blue")
     , xlab="Peak of signal from sensor 1"
     , ylab="Peak of signal from sensor 2"
     , main=paste0("Failure 2 (n = ", weights["Failure 2"], ")"))

plot(faults$s1.peak, faults$s2.peak
     , pch=ifelse(faults$fault == "Failure 3", "*", ".")
     , col=ifelse(faults$fault == "Failure 3", "Red", "Blue")
     , xlab="Peak of signal from sensor 1"
     , ylab="Peak of signal from sensor 2"
     , main=paste0("Failure 1 (n = ", weights["Failure 3"], ")"))

plot(faults$s1.peak, faults$s2.peak
     , pch=ifelse(faults$fault == "Normal 1", "*", ".")
     , col=ifelse(faults$fault == "Normal 1", "Red", "Blue")
     , xlab="Peak of signal from sensor 1"
     , ylab="Peak of signal from sensor 2"
     , main=paste0("Normal 1 (n = ", weights["Normal 1"], ")"))
dev.off()

## Part 2
## Fit SVM with default settings on pairs of scenarios, then balance 
## data and compare results (choose best balance technique)
##

## not balanced fits
f1.n1 <- rbind(f.1, n.1)
f1.n1.notbalanced <- ksvm(x=as.matrix(f1.n1[, -c(1, 2, 24)])
                        , y=f1.n1$fault
                        , type="C-svc"
                        , kernel='rbfdot'
                        , cross = folds)

f2.n1 <- rbind(f.2, n.1)
f2.n1.notbalanced <- ksvm(x=as.matrix(f2.n1[, -c(1, 2, 24)])
                          , y=f2.n1$fault
                          , type="C-svc"
                          , kernel='rbfdot'
                          , cross = folds)

f3.n1 <- rbind(f.3, n.1)
f3.n1.notbalanced <- ksvm(x=as.matrix(f3.n1[, -c(1, 2, 24)])
                          , y=f3.n1$fault
                          , type="C-svc"
                          , kernel='rbfdot'
                          , cross = folds)

rm(f1.n1); rm(f2.n1); rm(f3.n1)

## TODO: balanced fits

## TODO: comparison

## Part 3
## Fit SVM with default settings on all classes, then balance/weight
## data and compare results (choose best balance technique)
##

## not balanced fits
fit.notbalanced <- ksvm(x=as.matrix(faults[, -c(1, 2, 24)])
                          , y=faults$fault
                          , type="C-svc"
                          , kernel='rbfdot'
                          , cross = folds)

## TODO: balanced fits

## TODO: weighted fits Hint: use class.weights parameter

## TODO: comparison

## Part 4
## Fit SVM with different kernels (maybe implementations) 
## and compare results (choose best kernel function)
##

## Part 5
## Fit k-class SVM and compare results with standard
## "One-Against-One" approach
##

## Part 6
## Explore how feature extraction affects result (PCA analysis, maybe ICA)
##

## Extract features
pca.fit <- princomp(faults[, -c(1, 2, 24)], cor=TRUE)

## TODO: explore ICA
ica.fit <- fastICA(faults[, 3:23], 2)

## Visualise results
cumsum(pca.fit$sdev^2)/sum(pca.fit$sdev^2)
plot(x=1:21, cumsum(pca.fit$sdev^2)/sum(pca.fit$sdev^2)
     , xlab="Number of Components"
     , ylab="Variance Explained"
     , type="l"
     , lwd=2)

## Repeat procedure and find optimal number of components

## Part 7
## Find best parametrization for SVN using PSO
##

## Best parametrization
fc <- function(x) {
  svp <- ksvm(pca.fit$scores[, 1:17]
       , faults$fault
       , type="C-svc"
       , kernel='rbfdot'
       , kpar=list("sigma"=.1)
       , C=x
       , scaled=c()
       , cross = 10)
  error <- attributes(svp)$cross
  error
}

optim.c <- psoptim(c(1), fc, lower = 0, upper = 100)

## Predict funal results
table(predict(svp, pca.fit$scores[, 1:2]), faults$fault) 