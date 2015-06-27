setwd("~/Documents/Uni_Munster/DA in Spare Parts Management/fault_diagnosis")

library(ggplot2)
library(PerformanceAnalytics)
library(fastICA)
library(mlr)
library(kernlab)
library(pso)

## Set random seed
set.seed(20150626)

## Load transformed data
source("load.R")

## Load resampling functions
source("balance.R")
source("helper.R")

## Set number of folds for cross-validation
folds <- 20

## Part 1----
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
par(mfcol = c(2, 2), cex=1.2)
plot(faults$s1.sd, faults$s2.sd
     , pch=ifelse(faults$fault == "Failure 1", "*", ".")
     , col=ifelse(faults$fault == "Failure 1", "Red", "Blue")
     , xlab="Std. of Signal from Sensor 1"
     , ylab="Std. of Signal from Sensor 2"
     , main=paste0("Failure 1 (n = ", weights["Failure 1"], ")"))

plot(faults$s1.sd, faults$s2.sd
     , pch=ifelse(faults$fault == "Failure 2", "*", ".")
     , col=ifelse(faults$fault == "Failure 2", "Red", "Blue")
     , xlab="Std. of Signal from Sensor 1"
     , ylab="Std. of Signal from Sensor 2"
     , main=paste0("Failure 2 (n = ", weights["Failure 2"], ")"))

plot(faults$s1.sd, faults$s2.sd
     , pch=ifelse(faults$fault == "Failure 3", "*", ".")
     , col=ifelse(faults$fault == "Failure 3", "Red", "Blue")
     , xlab="Std. of Signal from Sensor 1"
     , ylab="Std. of Signal from Sensor 2"
     , main=paste0("Failure 1 (n = ", weights["Failure 3"], ")"))

plot(faults$s1.sd, faults$s2.sd
     , pch=ifelse(faults$fault == "Normal 1", "*", ".")
     , col=ifelse(faults$fault == "Normal 1", "Red", "Blue")
     , xlab="Std. of Signal from Sensor 1"
     , ylab="Std. of Signal from Sensor 2"
     , main=paste0("Normal 1 (n = ", weights["Normal 1"], ")"))
dev.off()

## Second plot
png(filename="images/visualInspection2.png", width = 600, height = 600)
par(mfcol = c(2, 2))
plot(faults$s1.peak, faults$s2.peak
     , pch=ifelse(faults$fault == "Failure 1", "*", ".")
     , col=ifelse(faults$fault == "Failure 1", "Red", "Blue")
     , xlab="Peak of Signal from Sensor 1"
     , ylab="Peak of Signal from Sensor 2"
     , main=paste0("Failure 1 (n = ", weights["Failure 1"], ")"))

plot(faults$s1.peak, faults$s2.peak
     , pch=ifelse(faults$fault == "Failure 2", "*", ".")
     , col=ifelse(faults$fault == "Failure 2", "Red", "Blue")
     , xlab="Peak of Signal from Sensor 1"
     , ylab="Peak of Signal from Sensor 2"
     , main=paste0("Failure 2 (n = ", weights["Failure 2"], ")"))

plot(faults$s1.peak, faults$s2.peak
     , pch=ifelse(faults$fault == "Failure 3", "*", ".")
     , col=ifelse(faults$fault == "Failure 3", "Red", "Blue")
     , xlab="Peak of Signal from Sensor 1"
     , ylab="Peak of Signal from Sensor 2"
     , main=paste0("Failure 1 (n = ", weights["Failure 3"], ")"))

plot(faults$s1.peak, faults$s2.peak
     , pch=ifelse(faults$fault == "Normal 1", "*", ".")
     , col=ifelse(faults$fault == "Normal 1", "Red", "Blue")
     , xlab="Peak of Signal from Sensor 1"
     , ylab="Peak of Signal from Sensor 2"
     , main=paste0("Normal 1 (n = ", weights["Normal 1"], ")"))
dev.off()

## Part 2----
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

## Random Oversampling
f1.n1 <- balance(data = list(f.1, n.1), method = 'o', technique = list('under' = NULL, 'over' = ros))
f1.n1 <- rbind(f1.n1[[1]], f1.n1[[2]])
f1.n1.ros <- ksvm(x=as.matrix(f1.n1[, -c(1, 2, 24)])
                  , y=f1.n1$fault
                  , type="C-svc"
                  , kernel='rbfdot'
                  , cross = folds)

f2.n1 <- balance(data = list(f.2, n.1), method = 'o', technique = list('under' = NULL, 'over' = ros))
f2.n1 <- rbind(f2.n1[[1]], f2.n1[[2]])
f2.n1.ros <- ksvm(x=as.matrix(f2.n1[, -c(1, 2, 24)])
                  , y=f2.n1$fault
                  , type="C-svc"
                  , kernel='rbfdot'
                  , cross = folds)

f3.n1 <- balance(data = list(f.3, n.1), method = 'o', technique = list('under' = NULL, 'over' = ros))
f3.n1 <- rbind(f3.n1[[1]], f3.n1[[2]])
f3.n1.ros <- ksvm(x=as.matrix(f3.n1[, -c(1, 2, 24)])
                  , y=f3.n1$fault
                  , type="C-svc"
                  , kernel='rbfdot'
                  , cross = folds)

rm(f1.n1); rm(f2.n1); rm(f3.n1)

## Random Undersampling
f1.n1 <- balance(data = list(f.1, n.1), method = 'u', technique = list('under' = rus, 'over' = NULL))
f1.n1 <- rbind(f1.n1[[1]], f1.n1[[2]])
f1.n1.rus <- ksvm(x=as.matrix(f1.n1[, -c(1, 2, 24)])
                  , y=f1.n1$fault
                  , type="C-svc"
                  , kernel='rbfdot'
                  , cross = folds)

f2.n1 <- balance(data = list(f.2, n.1), method = 'u', technique = list('under' = rus, 'over' = NULL))
f2.n1 <- rbind(f2.n1[[1]], f2.n1[[2]])
f2.n1.rus <- ksvm(x=as.matrix(f2.n1[, -c(1, 2, 24)])
                  , y=f2.n1$fault
                  , type="C-svc"
                  , kernel='rbfdot'
                  , cross = folds)

f3.n1 <- balance(data = list(f.3, n.1), method = 'u', technique = list('under' = rus, 'over' = NULL))
f3.n1 <- rbind(f3.n1[[1]], f3.n1[[2]])
f3.n1.rus <- ksvm(x=as.matrix(f3.n1[, -c(1, 2, 24)])
                  , y=f3.n1$fault
                  , type="C-svc"
                  , kernel='rbfdot'
                  , cross = folds)

rm(f1.n1); rm(f2.n1); rm(f3.n1)

## Comparison
binary.svm <- list(f1.n1.notbalanced, f1.n1.ros, f1.n1.rus, f2.n1.notbalanced, f2.n1.ros, f2.n1.rus, f3.n1.notbalanced, f3.n1.ros, f3.n1.rus)
names(binary.svm) <- c('f1.n1.notbalanced', 'f1.n1.ros', 'f1.n1.rus', 'f2.n1.notbalanced', 'f2.n1.ros', 'f2.n1.rus', 'f3.n1.notbalanced', 'f3.n1.ros', 'f3.n1.rus')

f1.n1 <- rbind(f.1, n.1)
f2.n1 <- rbind(f.2, n.1)
f3.n1 <- rbind(f.3, n.1)

binary.data <- list(f1.n1, f2.n1, f3.n1)
names(binary.data) <- c('f1.n1', 'f2.n1', 'f3.n1')

## Cross-validation error
## Random Oversampling (lowest mean of cross-validation error)
cross.comp <- matrix(unlist(lapply(binary.svm, function(x) x@cross)), ncol = 3, byrow = T)
rownames(cross.comp) <- c("f1.n1", "f2.n1", "f3.n1")
colnames(cross.comp) <- c("original", "ros", "rus")
which(min(colMeans(cross.comp)) == colMeans(cross.comp))

## F-Measure
## From single confusion matrices
## Original and Random Oversampling are tied (highest mean of F-Measure)
fm.comp <- matrix(apply(cbind(1:9, rep(1:3, each = 3)), 1, function(x) {
  m <- table(predict(binary.svm[[x[1]]], as.matrix(binary.data[[x[2]]][, -c(1,2,24)])), binary.data[[x[2]]]$fault)
  p <- m[1, 1]/sum(m[1, ])
  r <- m[1, 1]/sum(m[, 1])
  2*(p*r)/(p+r)
}), nrow = 3, ncol = 3, byrow = T)

rownames(fm.comp) <- c("f1.n1", "f2.n1", "f3.n1")
colnames(fm.comp) <- c("original", "ros", "rus")
which(max(colMeans(fm.comp)) == colMeans(fm.comp))

## Using helper functions----
f1.n1 <- rbind(f.1, n.1)
p11.notbalanced <- cv.ksvm(x=as.matrix(f1.n1[, -c(1, 2, 24)])
                           , y=f1.n1$fault
                           , folds = folds
                           , type="C-svc"
                           , kernel='rbfdot')

f2.n1 <- rbind(f.2, n.1)
p21.notbalanced <- cv.ksvm(x=as.matrix(f2.n1[, -c(1, 2, 24)])
                        , y=f2.n1$fault
                        , folds = folds
                        , type="C-svc"
                        , kernel='rbfdot')

f3.n1 <- rbind(f.3, n.1)
p31.notbalanced <- cv.ksvm(x=as.matrix(f3.n1[, -c(1, 2, 24)])
                        , y=f3.n1$fault
                        , folds = folds
                        , type="C-svc"
                        , kernel='rbfdot')

rm(f1.n1); rm(f2.n1); rm(f3.n1)

## Random Oversampling
f1.n1 <- balance(data = list(f.1, n.1), method = 'o', technique = list('under' = NULL, 'over' = ros))
f1.n1 <- rbind(f1.n1[[1]], f1.n1[[2]])
p11.ros <- cv.ksvm(x=as.matrix(f1.n1[, -c(1, 2, 24)])
                , y=f1.n1$fault
                , folds = folds
                , type="C-svc"
                , kernel='rbfdot')

f2.n1 <- balance(data = list(f.2, n.1), method = 'o', technique = list('under' = NULL, 'over' = ros))
f2.n1 <- rbind(f2.n1[[1]], f2.n1[[2]])
p21.ros <- cv.ksvm(x=as.matrix(f2.n1[, -c(1, 2, 24)])
                , y=f2.n1$fault
                , folds = folds
                , type="C-svc"
                , kernel='rbfdot')

f3.n1 <- balance(data = list(f.3, n.1), method = 'o', technique = list('under' = NULL, 'over' = ros))
f3.n1 <- rbind(f3.n1[[1]], f3.n1[[2]])
p31.ros <- cv.ksvm(x=as.matrix(f3.n1[, -c(1, 2, 24)])
                , y=f3.n1$fault
                , folds = folds                
                , type="C-svc"
                , kernel='rbfdot')
                
rm(f1.n1); rm(f2.n1); rm(f3.n1)

## Random Undersampling
f1.n1 <- balance(data = list(f.1, n.1), method = 'u', technique = list('under' = rus, 'over' = NULL))
f1.n1 <- rbind(f1.n1[[1]], f1.n1[[2]])
p11.rus <- cv.ksvm(x=as.matrix(f1.n1[, -c(1, 2, 24)])
                   , y=f1.n1$fault
                   , folds = folds
                   , type="C-svc"
                   , kernel='rbfdot')

f2.n1 <- balance(data = list(f.2, n.1), method = 'u', technique = list('under' = rus, 'over' = NULL))
f2.n1 <- rbind(f2.n1[[1]], f2.n1[[2]])
p21.rus <- cv.ksvm(x=as.matrix(f2.n1[, -c(1, 2, 24)])
                   , y=f2.n1$fault
                   , folds = folds
                   , type="C-svc"
                   , kernel='rbfdot')

f3.n1 <- balance(data = list(f.3, n.1), method = 'u', technique = list('under' = rus, 'over' = NULL))
f3.n1 <- rbind(f3.n1[[1]], f3.n1[[2]])
p31.rus <- cv.ksvm(x=as.matrix(f3.n1[, -c(1, 2, 24)])
                   , y=f3.n1$fault
                   , folds = folds
                   , type="C-svc"
                   , kernel='rbfdot')

rm(f1.n1); rm(f2.n1); rm(f3.n1)

## Comparison Alternative
binary.cv.svm <- list(p11.notbalanced, p11.ros, p11.rus, p21.notbalanced, p21.ros, p21.rus, p31.notbalanced, p31.ros, p31.rus)
names(binary.cv.svm) <- c('f1.n1.notbalanced', 'f1.n1.ros', 'f1.n1.rus', 'f2.n1.notbalanced', 'f2.n1.ros', 'f2.n1.rus', 'f3.n1.notbalanced', 'f3.n1.ros', 'f3.n1.rus')

## F-Measure
## Random Oversampling (highest mean of F-Measure)
fm.cv.comp <- matrix(unlist(lapply(binary.cv.svm, function(x) mean(f.cv(x)))), ncol = 3, byrow = T)
rownames(fm.cv.comp) <- c("f1.n1", "f2.n1", "f3.n1")
colnames(fm.cv.comp) <- c("original", "ros", "rus")
which(max(colMeans(fm.cv.comp)) == colMeans(fm.cv.comp))

## G-Mean
## Random Oversampling (highest mean of G-Mean)
gm.cv.comp <- matrix(unlist(lapply(binary.cv.svm, function(x) mean(gmean.cv(x)))), ncol = 3, byrow = T)
rownames(gm.cv.comp) <- c("f1.n1", "f2.n1", "f3.n1")
colnames(gm.cv.comp) <- c("original", "ros", "rus")
which(max(colMeans(gm.cv.comp)) == colMeans(gm.cv.comp))

## Part 3----
## Fit SVM with default settings on all classes, then balance/weight
## data and compare results (choose best balance technique)
##

all.list <- list(f.1, f.2, f.3, n.1, n.2, n.3)

## not balanced fits
fit.notbalanced <- ksvm(x=as.matrix(faults[, -c(1, 2, 24)])
                        , y=faults$fault
                        , type="C-svc"
                        , kernel='rbfdot'
                        , cross = folds)

## SMean resampling
f.smean <- balance.multi(data = all.list, method = "smean", technique = list('under' = rus, 'over' = ros))
f.smean <- do.call(rbind.data.frame, f.smean)
fit.smean <- ksvm(x=as.matrix(f.smean[, -c(1, 2, 24)])
                        , y=f.smean$fault
                        , type="C-svc"
                        , kernel='rbfdot'
                        , cross = folds)

## SMedian resampling
f.smedian <- balance.multi(data = all.list, method = "smedian", technique = list('under' = rus, 'over' = ros))
f.smedian <- do.call(rbind.data.frame, f.smedian)
fit.smedian <- ksvm(x=as.matrix(f.smedian[, -c(1, 2, 24)])
                  , y=f.smedian$fault
                  , type="C-svc"
                  , kernel='rbfdot'
                  , cross = folds)

## Naive weights
weight <- 1/(sapply(all.list, nrow)/sum(sapply(all.list, nrow)))
fit.weighted <- ksvm(x=as.matrix(faults[, -c(1, 2, 24)])
                     , y=faults$fault
                     , type="C-svc"
                     , kernel='rbfdot'
                     , cross = folds
                     , class.weight = weight)

## Comparison
multi.svm <- list(fit.notbalanced, fit.smean, fit.smedian, fit.weighted)
names(multi.svm) <- c('notbalanced', 'smean', 'smedian', 'weighted')

## Cross-validation error
## SMean resampling (lowest mean of cross-validation error)
multi.cross.comp <- unlist(lapply(multi.svm, function(x) x@cross))
which(multi.cross.comp == min(multi.cross.comp))

## Part 4
## Fit SVM with different kernels (maybe implementations) 
## and compare results (choose best kernel function)
##

## Set random seed
set.seed(20150626)

## Perform selection
f.smean <- balance.multi(data = all.list, method = "smean", technique = list('under' = rus, 'over' = ros))
f.smean <- do.call(rbind.data.frame, f.smean)

errors <- sapply(c("vanilladot", "polydot", "rbfdot", "laplacedot", "anovadot")
                 , function(kernel) { 
                    fit <- ksvm(x=as.matrix(f.smean[, -c(1, 2, 24)])
                         , y=f.smean$fault
                         , type="C-svc"
                         , kernel=kernel
                         , cross = folds)
                    attributes(fit)$cross*100 } )
names(errors) <- c("Linear", "Polynomial", "Radial Basis", "Laplacian", "ANOVA RBF")

png(filename="images/kernels.png", width = 800, height = 600)
par(mfcol=c(1, 1), cex=1.5)
barplot(errors, xlab="Kernel", ylab="Cross Validation Error Rate (%)")
dev.off()

## Part 5
## Explore how feature extraction affects result (PCA analysis, maybe ICA)
##

## Extract features
pca.fit <- princomp(rbind(f.1, f.2, f.3, n.1, n.2, n.3)[, -c(1, 2, 24)], cor=TRUE)

## TODO: explore ICA
ica.fit <- fastICA(faults[, 3:23], 2)

## Visualise results, 11 components explaine about 99% of variance
png(filename="images/pcas.png", width = 800, height = 600)
cumsum(pca.fit$sdev^2)/sum(pca.fit$sdev^2)
plot(x=1:21, cumsum(pca.fit$sdev^2)/sum(pca.fit$sdev^2)*100
     , xlab="Number of Principal Components"
     , ylab="Variance Explained (%)"
     , type="l"
     , lwd=2)
dev.off()

## Get balanced pca data
f.1.pca <- data.frame(cbind(f.1[, c(1, 2)], pca.fit$scores[1:dim(f.1)[1], ], f.1[, 24, drop=FALSE]))
f.2.pca <- data.frame(cbind(f.2[, c(1, 2)], pca.fit$scores[1:dim(f.2)[1], ], f.2[, 24, drop=FALSE]))
f.3.pca <- data.frame(cbind(f.3[, c(1, 2)], pca.fit$scores[1:dim(f.3)[1], ], f.3[, 24, drop=FALSE]))
n.1.pca <- data.frame(cbind(n.1[, c(1, 2)], pca.fit$scores[1:dim(n.1)[1], ], n.1[, 24, drop=FALSE]))
n.2.pca <- data.frame(cbind(n.2[, c(1, 2)], pca.fit$scores[1:dim(n.2)[1], ], n.2[, 24, drop=FALSE]))
n.3.pca <- data.frame(cbind(n.3[, c(1, 2)], pca.fit$scores[1:dim(n.3)[1], ], n.3[, 24, drop=FALSE]))

names(f.1.pca) <- names(f.1)
names(f.2.pca) <- names(f.2)
names(f.3.pca) <- names(f.3)
names(n.1.pca) <- names(n.1)
names(n.2.pca) <- names(n.2)
names(n.3.pca) <- names(n.3)

all.list.pca <- list(f.1.pca, f.2.pca, f.3.pca, n.1.pca, n.2.pca, n.3.pca)

f.smean.pca <- balance.multi(data = all.list.pca, method = "smean", technique = list('under' = rus, 'over' = ros))
f.smean.pca <- do.call(rbind.data.frame, f.smean)


## Repeat procedure and find optimal number of components


## Part 6
## Fit k-class SVM and compare results with standard
## "One-Against-One" approach
##

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