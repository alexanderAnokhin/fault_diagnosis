library(unbalanced)
library(ggplot2)
library(mlr)
library(kernlab)
library(pso)
library(DMwR)

## Set random seed
set.seed(20150626)

## How many seconds in one split
seconds <- 5

## Load transformed data
source("resources/load.R")

## Load resampling functions
source("resources/balance.R")

## Set number of folds for cross-validation
folds <- 20

## Part 2----
## Fit SVM with default settings on pairs of scenarios, then balance 
## data and compare results (choose best balance technique)
##

## Set data
f1.n1 <- rbind(f.1, n.1)
levels(f1.n1$fault) <- c(1, 0)
f2.n1 <- rbind(f.2, n.1)
levels(f2.n1$fault) <- c(1, 0)
f3.n1 <- rbind(f.3, n.1)
levels(f3.n1$fault) <- c(1, 0)

binary.data <- list(f1.n1[, -c(1, 2)], f2.n1[, -c(1, 2)], f3.n1[, -c(1, 2)])
names(binary.data) <- c('f1.n1', 'f2.n1', 'f3.n1')

## Train classifiers
binary.svm <- lapply(binary.data, function(x) {
  res <- list()
  
  # Original
  res$ori <- ksvm(x = as.matrix(x[, -22])
                  , y = x[, 22]
                  , type = "C-svc"
                  , kernel = 'rbfdot'
                  , cross = folds)
  
  # Random oversampling
  data <- ubOver(X = x[, -22], Y = x[, 22], k = 0)
  res$ros <- ksvm(x = as.matrix(data$X)
                  , y = data$Y
                  , type = "C-svc"
                  , kernel = 'rbfdot'
                  , cross = folds)
  
  # Synthetic minority over-sampling technique
  data <- ubSMOTE(X = x[, -22], Y = x[, 22], k = 5, perc.over = 200, perc.under = 200) 
  res$smo <- ksvm(x = as.matrix(data$X)
                  , y = data$Y
                  , type = "C-svc"
                  , kernel = 'rbfdot'
                  , cross = folds)
  
  # Random undersampling
  data <- ubUnder(X = x[, -22], Y = x[, 22], propMinClass = 50) 
  res$rus <- ksvm(x = as.matrix(data$X)
                  , y = data$Y
                  , type = "C-svc"
                  , kernel = 'rbfdot'
                  , cross = folds)
  
  # One-sided sampling
  data <- ubOSS(X = x[, -22], Y = x[, 22]) 
  res$oss <- ksvm(x = as.matrix(data$X)
                  , y = data$Y
                  , type = "C-svc"
                  , kernel = 'rbfdot'
                  , cross = folds)
  
  # Condensed nearest neighbours
  data <- ubCNN(X = x[, -22], Y = x[, 22], k = 1) 
  res$cnn <- ksvm(x = as.matrix(data$X)
                  , y = data$Y
                  , type = "C-svc"
                  , kernel = 'rbfdot'
                  , cross = folds)
  
  # Edited nearest neighbours
  data <- ubENN(X = x[, -22], Y = x[, 22], k = 3) 
  res$enn <- ksvm(x = as.matrix(data$X)
                  , y = data$Y
                  , type = "C-svc"
                  , kernel = 'rbfdot'
                  , cross = folds)
  
  # Neighbourhood cleaning rule
  data <- ubNCL(X = x[, -22], Y = x[, 22], k = 3) 
  res$ncl <- ksvm(x = as.matrix(data$X)
                  , y = data$Y
                  , type = "C-svc"
                  , kernel = 'rbfdot'
                  , cross = folds)
  
  # Tomek link
  data <- ubTomek(X = x[, -22], Y = x[, 22]) 
  res$tom <- ksvm(x = as.matrix(data$X)
                  , y = data$Y
                  , type = "C-svc"
                  , kernel = 'rbfdot'
                  , cross = folds)
  
  return(res)
})

names(binary.svm) <- c('f1.n1', 'f2.n1', 'f3.n1')

## Comparison

## Cross-validation error
## SMOTE (lowest mean of cross-validation error)
cross.comp <- matrix(
  unlist(lapply(binary.svm, function(x) 
    unlist(lapply(x, function(y) 
      y@cross))))
  , ncol = length(binary.svm))
colnames(cross.comp) <- c("f1.n1", "f2.n1", "f3.n1")
rownames(cross.comp) <- names(binary.svm[[1]])
which(min(rowMeans(cross.comp)) == rowMeans(cross.comp))

## F-Measure
## From single confusion matrices
## Original, OSS, ENN, and Tomek are tied (highest mean of F-Measure)
fm.comp <- sapply(1:3, function(x) {
  unlist(lapply(binary.svm[[x]], function(y) {
    m <- table(predict(y, as.matrix(binary.data[[x]][, -22])), binary.data[[x]][, 22])
    p <- m[1, 1]/sum(m[1, ])
    r <- m[1, 1]/sum(m[, 1])
    2*(p*r)/(p+r)
  }))
})
colnames(fm.comp) <- c("f1.n1", "f2.n1", "f3.n1")
rownames(fm.comp) <- names(binary.svm[[1]])
which(max(rowMeans(fm.comp)) == rowMeans(fm.comp))

## Part 3----
## Fit SVM with default settings on all classes, then balance/weight
## data and compare results (choose best balance technique)
##

all.list <- list(f.1[, -c(1,2)], f.2[, -c(1,2)], f.3[, -c(1,2)], n.1[, -c(1,2)], n.2[, -c(1,2)], n.3[, -c(1,2)])

## not balanced fits
fit.notbalanced <- ksvm(x=as.matrix(faults[, -c(1, 2, 24)])
                        , y=faults$fault
                        , type="C-svc"
                        , kernel='rbfdot'
                        , cross = folds)

## SMean resampling
f.smean <- balance.multi(data = all.list, method = "smean", technique = list('under' = rus, 'over' = ros))
f.smean <- do.call(rbind.data.frame, f.smean)
fit.smean <- ksvm(x=as.matrix(f.smean[, -22])
                  , y=f.smean$fault
                  , type="C-svc"
                  , kernel='rbfdot'
                  , cross = folds)

## SMedian resampling
f.smedian <- balance.multi(data = all.list, method = "smedian", technique = list('under' = rus, 'over' = ros))
f.smedian <- do.call(rbind.data.frame, f.smedian)
fit.smedian <- ksvm(x=as.matrix(f.smedian[, -22])
                    , y=f.smedian$fault
                    , type="C-svc"
                    , kernel='rbfdot'
                    , cross = folds)

## SMOTE
f.smote <- SMOTE(form = fault~., faults[, -c(1,2)])
fit.smote <- ksvm(x=as.matrix(f.smote[, -22])
                    , y=f.smote$fault
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
multi.svm <- list(fit.notbalanced, fit.smean, fit.smedian, fit.smote, fit.weighted)
names(multi.svm) <- c('notbalanced', 'smean', 'smedian', 'smote', 'weighted')

## Cross-validation error
## SMean resampling (lowest mean of cross-validation error)
multi.cross.comp <- unlist(lapply(multi.svm, function(x) x@cross))
which(multi.cross.comp == min(multi.cross.comp))