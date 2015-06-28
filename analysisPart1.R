library(unbalanced)
library(ggplot2)
library(mlr)
library(kernlab)
library(pso)
library(DMwR)
library(PMCMR)
library(reshape2)

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

## Set data----
f1.n1 <- rbind(f.1, n.1)
levels(f1.n1$fault) <- c(1, 0)
f2.n1 <- rbind(f.2, n.1)
levels(f2.n1$fault) <- c(1, 0)
f3.n1 <- rbind(f.3, n.1)
levels(f3.n1$fault) <- c(1, 0)

f1.n2 <- rbind(f.1, n.2)
levels(f1.n2$fault) <- c(1, 0)
f2.n2 <- rbind(f.2, n.2)
levels(f2.n2$fault) <- c(1, 0)
f3.n2 <- rbind(f.3, n.2)
levels(f3.n2$fault) <- c(1, 0)

f1.n3 <- rbind(f.1, n.3)
levels(f1.n3$fault) <- c(1, 0)
f2.n3 <- rbind(f.2, n.3)
levels(f2.n3$fault) <- c(1, 0)
f3.n3 <- rbind(f.3, n.3)
levels(f3.n3$fault) <- c(1, 0)

binary.data <- list(f1.n1[, -c(1, 2)], f2.n1[, -c(1, 2)], f3.n1[, -c(1, 2)]
                    ,f1.n2[, -c(1, 2)], f2.n2[, -c(1, 2)], f3.n2[, -c(1, 2)]
                    ,f1.n3[, -c(1, 2)], f2.n3[, -c(1, 2)], f3.n3[, -c(1, 2)])
names(binary.data) <- c('f1.n1', 'f2.n1', 'f3.n1'
                        , 'f1.n2', 'f2.n2', 'f3.n2'
                        , 'f1.n3', 'f2.n3', 'f3.n3')

## Train classifiers----
binary.set <- lapply(binary.data, function(x) {
  res <- list()
  
  # None
  res$none$svm <- ksvm(x = as.matrix(x[, -22])
                       , y = x[, 22]
                       , type = "C-svc"
                       , kernel = 'rbfdot'
                       , cross = folds)
  res$none$min <- length(which(x[, 22] == 1))
  res$none$maj <- length(which(x[, 22] == 0))
  
  # Random oversampling
  data <- ubOver(X = x[, -22], Y = x[, 22], k = 0)
  res$ros$svm <- ksvm(x = as.matrix(data$X)
                      , y = data$Y
                      , type = "C-svc"
                      , kernel = 'rbfdot'
                      , cross = folds)
  res$ros$min <- length(which(data$Y == 1))
  res$ros$maj <- length(which(data$Y == 0))
  
  # Synthetic minority over-sampling technique (50:50 ratio)
  data <- ubSMOTE(X = x[, -22], Y = x[, 22], k = 5, perc.over = 100, perc.under = 200) 
  res$smo50$svm <- ksvm(x = as.matrix(data$X)
                        , y = data$Y
                        , type = "C-svc"
                        , kernel = 'rbfdot'
                        , cross = folds)
  res$smo50$min <- length(which(data$Y == 1))
  res$smo50$maj <- length(which(data$Y == 0))
  
  # Synthetic minority over-sampling technique (default settings)
  data <- ubSMOTE(X = x[, -22], Y = x[, 22], k = 5, perc.over = 200, perc.under = 200) 
  res$smoD$svm <- ksvm(x = as.matrix(data$X)
                       , y = data$Y
                       , type = "C-svc"
                       , kernel = 'rbfdot'
                       , cross = folds)
  res$smoD$min <- length(which(data$Y == 1))
  res$smoD$maj <- length(which(data$Y == 0))
  
  # Random undersampling
  data <- ubUnder(X = x[, -22], Y = x[, 22], propMinClass = 50) 
  res$rus$svm <- ksvm(x = as.matrix(data$X)
                      , y = data$Y
                      , type = "C-svc"
                      , kernel = 'rbfdot'
                      , cross = folds)
  res$rus$min <- length(which(data$Y == 1))
  res$rus$maj <- length(which(data$Y == 0))
  
  # One-sided sampling
  data <- ubOSS(X = x[, -22], Y = x[, 22]) 
  res$oss$svm <- ksvm(x = as.matrix(data$X)
                      , y = data$Y
                      , type = "C-svc"
                      , kernel = 'rbfdot'
                      , cross = folds)
  res$oss$min <- length(which(data$Y == 1))
  res$oss$maj <- length(which(data$Y == 0))
  
  # Condensed nearest neighbours
  data <- ubCNN(X = x[, -22], Y = x[, 22], k = 1) 
  res$cnn$svm <- ksvm(x = as.matrix(data$X)
                      , y = data$Y
                      , type = "C-svc"
                      , kernel = 'rbfdot'
                      , cross = folds)
  res$cnn$min <- length(which(data$Y == 1))
  res$cnn$maj <- length(which(data$Y == 0))
  
  # Edited nearest neighbours
  data <- ubENN(X = x[, -22], Y = x[, 22], k = 3) 
  res$enn$svm <- ksvm(x = as.matrix(data$X)
                      , y = data$Y
                      , type = "C-svc"
                      , kernel = 'rbfdot'
                      , cross = folds)
  res$enn$min <- length(which(data$Y == 1))
  res$enn$maj <- length(which(data$Y == 0))
  
  # Neighbourhood cleaning rule
  data <- ubNCL(X = x[, -22], Y = x[, 22], k = 3) 
  res$ncl$svm <- ksvm(x = as.matrix(data$X)
                      , y = data$Y
                      , type = "C-svc"
                      , kernel = 'rbfdot'
                      , cross = folds)
  res$ncl$min <- length(which(data$Y == 1))
  res$ncl$maj <- length(which(data$Y == 0))
  
  # Tomek link
  data <- ubTomek(X = x[, -22], Y = x[, 22]) 
  res$tom$svm <- ksvm(x = as.matrix(data$X)
                      , y = data$Y
                      , type = "C-svc"
                      , kernel = 'rbfdot'
                      , cross = folds)
  res$tom$min <- length(which(data$Y == 1))
  res$tom$maj <- length(which(data$Y == 0))
  
  return(res)
})

names(binary.set) <- names(binary.data)
binary.svm <- lapply(binary.set, function(x)
  lapply(x, function(y) y$svm))
names(binary.svm) <- names(binary.data)

## Class distribution
binary.min <- lapply(binary.set, function(x)
  lapply(x, function(y) y$min))
names(binary.min) <- names(binary.data)

instances.min <- matrix(
  unlist(lapply(binary.min, function(x)
    unlist(x)))
  , ncol = (length(binary.min)))
colnames(instances.min) <- names(binary.min)
rownames(instances.min) <- names(binary.min[[1]])

binary.maj <- lapply(binary.set, function(x)
  lapply(x, function(y) y$maj))
names(binary.maj) <- names(binary.data)

instances.maj <- matrix(
  unlist(lapply(binary.maj, function(x)
    unlist(x)))
  , ncol = (length(binary.maj)))
colnames(instances.maj) <- names(binary.maj)
rownames(instances.maj) <- names(binary.maj[[1]])

## Comparison----
## Cross-validation error
## SMOTE (lowest mean of cross-validation error)
cross.comp <- matrix(
  unlist(lapply(binary.svm, function(x) 
    unlist(lapply(x, function(y) 
      y@cross))))
  , ncol = length(binary.svm))
colnames(cross.comp) <- names(binary.svm)
rownames(cross.comp) <- names(binary.svm[[1]])
which(min(rowMeans(cross.comp)) == rowMeans(cross.comp))
posthoc.friedman.nemenyi.test(t(cross.comp))
cross.melt <- melt(cross.comp)
colnames(cross.melt) <- c('method', 'data', 'value')
p <- ggplot(cross.melt, aes(x = method, y = value)) + geom_boxplot(aes(fill = method))
p <- p + xlab("Method") + ylab("Cross-Validation Error") + ggtitle("Sampling Methods Comparison for Binary SVMs")
p <- p + theme(plot.title=element_text(size=28, face="bold"), text=element_text(size=28))
p
cross.comp

cross.rank <- apply(cross.comp, 2, min_rank)
which(min(rowMeans(cross.rank)) == rowMeans(cross.rank))

## Data difficulty
posthoc.friedman.nemenyi.test(cross.comp)
q <- ggplot(cross.melt, aes(x = data, y = value)) + geom_boxplot(aes(fill = data))
q <- q + xlab("Dataset") + ylab("Cross-Validation Error") + ggtitle("Data Difficulty for Binary Cases")
q

## F-Measure
## From single confusion matrices
## Original, OSS, ENN, and Tomek are tied (highest mean of F-Measure)
fm.comp <- sapply(1:9, function(x) {
  unlist(lapply(binary.svm[[x]], function(y) {
    m <- table(predict(y, as.matrix(binary.data[[x]][, -22])), binary.data[[x]][, 22])
    p <- m[1, 1]/sum(m[1, ])
    r <- m[1, 1]/sum(m[, 1])
    2*(p*r)/(p+r)
  }))
})
colnames(fm.comp) <- names(binary.svm)
rownames(fm.comp) <- names(binary.svm[[1]])
which(max(rowMeans(fm.comp)) == rowMeans(fm.comp))

fm.rank <- apply(fm.comp, 2, min_rank)

## Part 3----
## Fit SVM with default settings on all classes, then balance/weight
## data and compare results (choose best balance technique)
##

set.seed(20150626)

all.list <- list(f.1[, -c(1,2)], f.2[, -c(1,2)], f.3[, -c(1,2)], n.1[, -c(1,2)], n.2[, -c(1,2)], n.3[, -c(1,2)])

multi.svm <- sapply(1:100, function(x) {
  res <- list()
  
  ## not balanced fits
  res$none <- ksvm(x=as.matrix(faults[, -c(1, 2, 24)])
                   , y=faults$fault
                   , type="C-svc"
                   , kernel='rbfdot'
                   , cross = folds)
  
  ## SMean resampling
  f.smean <- balance.multi(data = all.list, method = "smean", technique = list('under' = rus, 'over' = ros))
  f.smean <- do.call(rbind.data.frame, f.smean)
  res$smean <- ksvm(x=as.matrix(f.smean[, -22])
                    , y=f.smean$fault
                    , type="C-svc"
                    , kernel='rbfdot'
                    , cross = folds)
  
  ## SMedian resampling
  f.smedian <- balance.multi(data = all.list, method = "smedian", technique = list('under' = rus, 'over' = ros))
  f.smedian <- do.call(rbind.data.frame, f.smedian)
  res$smedian <- ksvm(x=as.matrix(f.smedian[, -22])
                      , y=f.smedian$fault
                      , type="C-svc"
                      , kernel='rbfdot'
                      , cross = folds)
  
  ## SMOTE
  f.smote <- SMOTE(form = fault~., faults[, -c(1,2)])
  res$smote <- ksvm(x=as.matrix(f.smote[, -22])
                    , y=f.smote$fault
                    , type="C-svc"
                    , kernel='rbfdot'
                    , cross = folds)
  
  ## Naive weights
  weight <- sum(sapply(all.list, nrow))/sapply(all.list, nrow)
  res$weighted <- ksvm(x=as.matrix(faults[, -c(1, 2, 24)])
                       , y=faults$fault
                       , type="C-svc"
                       , kernel='rbfdot'
                       , cross = folds
                       , class.weight = weight)
  
  return(res)
})

## Comparison
## Cross-validation error
## SMean resampling (lowest mean of cross-validation error)
multi.cross.comp <- apply(multi.svm, 2, function(x)
  unlist(lapply(x, function(y) y@cross)))
which(min(rowMeans(multi.cross.comp)) == rowMeans(multi.cross.comp))
posthoc.friedman.nemenyi.test(t(multi.cross.comp))
multi.cross.melt <- melt(multi.cross.comp)
colnames(multi.cross.melt) <- c('method', 'iteration', 'value')
z <- ggplot(multi.cross.melt, aes(x = method, y = value)) + geom_boxplot(aes(fill = method))
z <- z + xlab("Method") + ylab("Cross-Validation Error") + ggtitle("Sampling Methods Comparison for K-class SVMs")
z <- z + theme(plot.title=element_text(size=28, face="bold"), text=element_text(size=28))
z

## without SMOTE
wSmote <- filter(multi.cross.melt, method != 'smote')
y <- ggplot(wSmote, aes(x = method, y = value)) + geom_boxplot(aes(fill = method))
y <- y + xlab("Method") + ylab("Cross-Validation Error") + ggtitle("Sampling Methods Comparison for K-class SVMs")
y <- y + theme(plot.title=element_text(size=28, face="bold"), text=element_text(size=28))
y
