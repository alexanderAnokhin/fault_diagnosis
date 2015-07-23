library(unbalanced)
library(ggplot2)
library(mlr)
library(kernlab)
library(pso)
library(DMwR)
library(PMCMR)
library(reshape2)
library(dunn.test)

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
  m <- sapply(1:10, function(f) {
    svm <- ksvm(x = as.matrix(x[, -22])
                , y = x[, 22]
                , type = "C-svc"
                , kernel = 'rbfdot'
                , cross = folds)
    cbind(svm@cross, length(which(x[, 22] == 1)), length(which(x[, 22] == 1)))
  })

  res$none$svm <- m[1, ]
  res$none$min <- mean(m[2, ])
  res$none$maj <- mean(m[3, ])
  
  # Random oversampling
  m <- sapply(1:10, function(f) {
    data <- ubOver(X = x[, -22], Y = x[, 22], k = 0)
    svm <- ksvm(x = as.matrix(data$X)
                , y = data$Y
                , type = "C-svc"
                , kernel = 'rbfdot'
                , cross = folds)
    cbind(svm@cross, length(which(data$Y == 1)), length(which(data$Y == 0)))
  })
   
  res$ros$svm <- m[1, ]
  res$ros$min <- mean(m[2, ])
  res$ros$maj <- mean(m[3, ])  
  
  # Synthetic minority over-sampling technique (50:50 ratio)  
  m <- sapply(1:10, function(f) {
    data <- ubSMOTE(X = x[, -22], Y = x[, 22], k = 5, perc.over = 100, perc.under = 200) 
    svm <- ksvm(x = as.matrix(data$X)
                , y = data$Y
                , type = "C-svc"
                , kernel = 'rbfdot'
                , cross = folds)
    cbind(svm@cross, length(which(data$Y == 1)), length(which(data$Y == 0)))
  })
  
  res$smo50$svm <- m[1, ]
  res$smo50$min <- mean(m[2, ])
  res$smo50$maj <- mean(m[3, ])
  
  # Synthetic minority over-sampling technique (default settings)
  m <- sapply(1:10, function(f) {
    data <- ubSMOTE(X = x[, -22], Y = x[, 22], k = 5, perc.over = 200, perc.under = 200) 
    svm <- ksvm(x = as.matrix(data$X)
                , y = data$Y
                , type = "C-svc"
                , kernel = 'rbfdot'
                , cross = folds)
    cbind(svm@cross, length(which(data$Y == 1)), length(which(data$Y == 0)))
  })
  
  res$smoD$svm <- m[1, ]
  res$smoD$min <- mean(m[2, ])
  res$smoD$maj <- mean(m[3, ])
  
  # Random undersampling
  m <- sapply(1:10, function(f) {
    data <- ubUnder(X = x[, -22], Y = x[, 22], propMinClass = 50) 
    svm <- ksvm(x = as.matrix(data$X)
                , y = data$Y
                , type = "C-svc"
                , kernel = 'rbfdot'
                , cross = folds)
    cbind(svm@cross, length(which(data$Y == 1)), length(which(data$Y == 0)))
  })
  
  res$rus$svm <- m[1, ]
  res$rus$min <- mean(m[2, ])
  res$rus$maj <- mean(m[3, ])
  
  # One-sided sampling
  m <- sapply(1:10, function(f) {
    data <- ubOSSx(X = x[, -22], Y = x[, 22]) 
    svm <- ksvm(x = as.matrix(data$X)
                , y = data$Y
                , type = "C-svc"
                , kernel = 'rbfdot'
                , cross = folds)
    cbind(svm@cross, length(which(data$Y == 1)), length(which(data$Y == 0)))
  })
  
  res$oss$svm <- m[1, ]
  res$oss$min <- mean(m[2, ])
  res$oss$maj <- mean(m[3, ])
  
  # Condensed nearest neighbours  
  m <- sapply(1:10, function(f) {
    data <- ubCNN(X = x[, -22], Y = x[, 22], k = 1) 
    svm <- ksvm(x = as.matrix(data$X)
                , y = data$Y
                , type = "C-svc"
                , kernel = 'rbfdot'
                , cross = folds)
    cbind(svm@cross, length(which(data$Y == 1)), length(which(data$Y == 0)))
  })
  
  res$cnn$svm <- m[1, ]
  res$cnn$min <- mean(m[2, ])
  res$cnn$maj <- mean(m[3, ])
  
  # Edited nearest neighbours
  m <- sapply(1:10, function(f) {
    data <- ubENN(X = x[, -22], Y = x[, 22], k = 3) 
    svm <- ksvm(x = as.matrix(data$X)
                , y = data$Y
                , type = "C-svc"
                , kernel = 'rbfdot'
                , cross = folds)
    cbind(svm@cross, length(which(data$Y == 1)), length(which(data$Y == 0)))
  })
  
  res$enn$svm <- m[1, ]
  res$enn$min <- mean(m[2, ])
  res$enn$maj <- mean(m[3, ])
  
  # Neighbourhood cleaning rule
  m <- sapply(1:10, function(f) {
    data <- ubNCL(X = x[, -22], Y = x[, 22], k = 3) 
    svm <- ksvm(x = as.matrix(data$X)
                , y = data$Y
                , type = "C-svc"
                , kernel = 'rbfdot'
                , cross = folds)
    cbind(svm@cross, length(which(data$Y == 1)), length(which(data$Y == 0)))
  })
  
  res$ncl$svm <- m[1, ]
  res$ncl$min <- mean(m[2, ])
  res$ncl$maj <- mean(m[3, ])
    
  # Tomek link
  m <- sapply(1:10, function(f) {
    data <- ubTomek(X = x[, -22], Y = x[, 22]) 
    svm <- ksvm(x = as.matrix(data$X)
                , y = data$Y
                , type = "C-svc"
                , kernel = 'rbfdot'
                , cross = folds)
    cbind(svm@cross, length(which(data$Y == 1)), length(which(data$Y == 0)))
  })
  
  res$tom$svm <- m[1, ]
  res$tom$min <- mean(m[2, ])
  res$tom$maj <- mean(m[3, ])
  
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
cross.comp <- do.call(rbind, lapply(binary.svm, function(x) do.call(cbind, x)))

which(min(colMeans(cross.comp)) == colMeans(cross.comp))
cross.melt <- melt(cross.comp)
colnames(cross.melt) <- c('i', 'method', 'value')

kruskal.test(cross.melt$value, cross.melt$method)
dunn.test(cross.melt$value, cross.melt$method, method = 'bonferroni')
posthoc.friedman.nemenyi.test(cross.comp)
p <- ggplot(cross.melt, aes(x = method, y = value)) + geom_boxplot(aes(fill = method))
p <- p + xlab("Method") + ylab("Cross-Validation Error") + ggtitle("Sampling Methods Comparison for Binary SVMs")
p <- p + theme(plot.title=element_text(size=28, face="bold"), text=element_text(size=28))
p
cross.comp

cross.rank <- apply(cross.comp, 1, min_rank)
rank.melt <- melt(t(cross.rank))
colnames(rank.melt) <- c('i', 'method', 'rank')
q <- ggplot(rank.melt, aes(x = method, y = rank)) + geom_boxplot(aes(fill = method))
q <- q + xlab("Method") + ylab("Rank") + ggtitle("Sampling Methods Comparison for Binary SVMs")
q <- q + theme(plot.title=element_text(size=28, face="bold"), text=element_text(size=28))
q

## Data difficulty
#posthoc.friedman.nemenyi.test(cross.comp)
#q <- ggplot(cross.melt, aes(x = data, y = value)) + geom_boxplot(aes(fill = data))
#q <- q + xlab("Dataset") + ylab("Cross-Validation Error") + ggtitle("Data Difficulty for Binary Cases")
#q

## F-Measure
## From single confusion matrices
## Original, OSS, ENN, and Tomek are tied (highest mean of F-Measure)
#fm.comp <- sapply(1:9, function(x) {
#  unlist(lapply(binary.svm[[x]], function(y) {
#    m <- table(predict(y, as.matrix(binary.data[[x]][, -22])), binary.data[[x]][, 22])
#    p <- m[1, 1]/sum(m[1, ])
#   r <- m[1, 1]/sum(m[, 1])
#    2*(p*r)/(p+r)
#  }))
#})
#colnames(fm.comp) <- names(binary.svm)
#rownames(fm.comp) <- names(binary.svm[[1]])
#which(max(rowMeans(fm.comp)) == rowMeans(fm.comp))

#fm.rank <- apply(fm.comp, 2, min_rank)

## Part 3----
## Fit SVM with default settings on all classes, then balance/weight
## data and compare results (choose best balance technique)
##

set.seed(20150626)

all.list <- list(f.1[, -c(1,2)], f.2[, -c(1,2)], f.3[, -c(1,2)], n.1[, -c(1,2)], n.2[, -c(1,2)], n.3[, -c(1,2)])
raw.weight <- sum(sapply(all.list, nrow))/sapply(all.list, nrow)
weight <- raw.weight/sum(raw.weight)

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
multi.cross.melt <- melt(multi.cross.comp)
colnames(multi.cross.melt) <- c('method', 'iteration', 'value')
kruskal.test(multi.cross.melt$value, multi.cross.melt$method)
dunn.test(multi.cross.melt$value, multi.cross.melt$method, method = 'bonferroni')
posthoc.friedman.nemenyi.test(t(multi.cross.comp[-4, ]))
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

# rank
multi.rank <- apply(multi.cross.comp[-4, ], 2, min_rank)
multi.rank.melt <- melt(t(multi.rank))
colnames(multi.rank.melt) <- c('i', 'method', 'rank')
w <- ggplot(multi.rank.melt, aes(x = method, y = rank)) + geom_boxplot(aes(fill = method))
w <- w + xlab("Method") + ylab("Rank") + ggtitle("Sampling Methods Comparison for K-class SVMs")
w <- w + theme(plot.title=element_text(size=28, face="bold"), text=element_text(size=28))
w
