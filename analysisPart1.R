## Set random seed
set.seed(20150626)

## How many seconds in one split
seconds <- 5

## Load transformed data
source("resources/load.R")

## Load resampling functions
source("resources/balance.R")
source("resources/helper.R")

## Set number of folds for cross-validation
folds <- 20

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
f1.n1 <- balance(data = list(f.1[, -c(1,2)], n.1[, -c(1,2)]), method = 'o', technique = list('under' = NULL, 'over' = ros))
f1.n1 <- rbind(f1.n1[[1]], f1.n1[[2]])
f1.n1.ros <- ksvm(x=as.matrix(f1.n1[, -22])
                  , y=f1.n1$fault
                  , type="C-svc"
                  , kernel='rbfdot'
                  , cross = folds)

f2.n1 <- balance(data = list(f.2[, -c(1,2)], n.1[, -c(1,2)]), method = 'o', technique = list('under' = NULL, 'over' = ros))
f2.n1 <- rbind(f2.n1[[1]], f2.n1[[2]])
f2.n1.ros <- ksvm(x=as.matrix(f2.n1[, -22])
                  , y=f2.n1$fault
                  , type="C-svc"
                  , kernel='rbfdot'
                  , cross = folds)

f3.n1 <- balance(data = list(f.3[, -c(1,2)], n.1[, -c(1,2)]), method = 'o', technique = list('under' = NULL, 'over' = ros))
f3.n1 <- rbind(f3.n1[[1]], f3.n1[[2]])
f3.n1.ros <- ksvm(x=as.matrix(f3.n1[, -22])
                  , y=f3.n1$fault
                  , type="C-svc"
                  , kernel='rbfdot'
                  , cross = folds)

rm(f1.n1); rm(f2.n1); rm(f3.n1)

## Random Undersampling
f1.n1 <- balance(data = list(f.1[, -c(1,2)], n.1[, -c(1,2)]), method = 'u', technique = list('under' = rus, 'over' = NULL))
f1.n1 <- rbind(f1.n1[[1]], f1.n1[[2]])
f1.n1.rus <- ksvm(x=as.matrix(f1.n1[, -22])
                  , y=f1.n1$fault
                  , type="C-svc"
                  , kernel='rbfdot'
                  , cross = folds)

f2.n1 <- balance(data = list(f.2[, -c(1,2)], n.1[, -c(1,2)]), method = 'u', technique = list('under' = rus, 'over' = NULL))
f2.n1 <- rbind(f2.n1[[1]], f2.n1[[2]])
f2.n1.rus <- ksvm(x=as.matrix(f2.n1[, -22])
                  , y=f2.n1$fault
                  , type="C-svc"
                  , kernel='rbfdot'
                  , cross = folds)

f3.n1 <- balance(data = list(f.3[, -c(1,2)], n.1[, -c(1,2)]), method = 'u', technique = list('under' = rus, 'over' = NULL))
f3.n1 <- rbind(f3.n1[[1]], f3.n1[[2]])
f3.n1.rus <- ksvm(x=as.matrix(f3.n1[, -22])
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
f1.n1 <- balance(data = list(f.1[, -c(1,2)], n.1[, -c(1,2)]), method = 'o', technique = list('under' = NULL, 'over' = ros))
f1.n1 <- rbind(f1.n1[[1]], f1.n1[[2]])
p11.ros <- cv.ksvm(x=as.matrix(f1.n1[, -22])
                   , y=f1.n1$fault
                   , folds = folds
                   , type="C-svc"
                   , kernel='rbfdot')

f2.n1 <- balance(data = list(f.2[, -c(1,2)], n.1[, -c(1,2)]), method = 'o', technique = list('under' = NULL, 'over' = ros))
f2.n1 <- rbind(f2.n1[[1]], f2.n1[[2]])
p21.ros <- cv.ksvm(x=as.matrix(f2.n1[, -22])
                   , y=f2.n1$fault
                   , folds = folds
                   , type="C-svc"
                   , kernel='rbfdot')

f3.n1 <- balance(data = list(f.3[, -c(1,2)], n.1[, -c(1,2)]), method = 'o', technique = list('under' = NULL, 'over' = ros))

f3.n1 <- rbind(f3.n1[[1]], f3.n1[[2]])
p31.ros <- cv.ksvm(x=as.matrix(f3.n1[, -22])
                   , y=f3.n1$fault
                   , folds = folds                
                   , type="C-svc"
                   , kernel='rbfdot')

rm(f1.n1); rm(f2.n1); rm(f3.n1)

## Random Undersampling
f1.n1 <- balance(data = list(f.1[, -c(1,2)], n.1[, -c(1,2)]), method = 'u', technique = list('under' = rus, 'over' = NULL))
f1.n1 <- rbind(f1.n1[[1]], f1.n1[[2]])
p11.rus <- cv.ksvm(x=as.matrix(f1.n1[, -22])
                   , y=f1.n1$fault
                   , folds = folds
                   , type="C-svc"
                   , kernel='rbfdot')

f2.n1 <- balance(data = list(f.2[, -c(1,2)], n.1[, -c(1,2)]), method = 'u', technique = list('under' = rus, 'over' = NULL))
f2.n1 <- rbind(f2.n1[[1]], f2.n1[[2]])
p21.rus <- cv.ksvm(x=as.matrix(f2.n1[, -22])
                   , y=f2.n1$fault
                   , folds = folds
                   , type="C-svc"
                   , kernel='rbfdot')

f3.n1 <- balance(data = list(f.3[, -c(1,2)], n.1[, -c(1,2)]), method = 'u', technique = list('under' = rus, 'over' = NULL))
f3.n1 <- rbind(f3.n1[[1]], f3.n1[[2]])
p31.rus <- cv.ksvm(x=as.matrix(f3.n1[, -22])
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