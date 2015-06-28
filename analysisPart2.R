library(ggplot2)
library(mlr)
library(kernlab)
library(pso)

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

## Get the balanced datasets
all.list <- list(f.1[, -c(1,2)], f.2[, -c(1,2)], f.3[, -c(1,2)], n.1[, -c(1,2)], n.2[, -c(1,2)], n.3[, -c(1,2)])
b.faults <- balance.multi(data = all.list, method = "smean", technique = list('under' = rus, 'over' = ros))
b.faults <- do.call(rbind.data.frame, b.faults)

## Part 4
## Fit SVM with different kernels (maybe implementations) 
## and compare results (choose best kernel function)
##

errors <- sapply(c("vanilladot", "polydot", "rbfdot", "laplacedot", "anovadot")
                 , function(kernel) { 
                    fit <- ksvm(x=as.matrix(b.faults[, -22])
                         , y=b.faults$fault
                         , type="C-svc"
                         , kernel=kernel
                         , cross = folds)
                    attributes(fit)$cross*100 } )
names(errors) <- c("Linear", "Polynomial", "Radial Basis", "Laplacian", "ANOVA RBF")

png(filename="images/kernels.png", width = 1000, height = 600)
par(mfcol=c(1, 1), cex=2)
barplot(errors, xlab="Kernel", ylab="Cross Validation Error Rate (%)")
dev.off()

## Part 5
## Explore how feature extraction affects result (PCA analysis, maybe ICA)
##

## Extract features
pca.fit <- princomp(faults[, -c(1, 2, 24)], cor=TRUE)

## Get datasets
all.list <- list(data.frame(pca.fit$scores[faults$fault == "Failure 1", ], factor("Failure 1"))
                 , data.frame(pca.fit$scores[faults$fault == "Failure 2", ], factor("Failure 2"))
                 , data.frame(pca.fit$scores[faults$fault == "Failure 3", ], factor("Failure 3"))
                 , data.frame(pca.fit$scores[faults$fault == "Normal 1", ], factor("Normal 1"))
                 , data.frame(pca.fit$scores[faults$fault == "Normal 2", ], factor("Normal 2"))
                 , data.frame(pca.fit$scores[faults$fault == "Normal 3", ], factor("Normal 3")))
pca.faults <- data.frame(pca.fit$scores, faults$fault)
b.pca.faults <- balance.multi(data = all.list, method = "smean", technique = list('under' = rus, 'over' = ros))
b.pca.faults <- do.call(rbind.data.frame, b.pca.faults)

## Visualise results, 11 components explaine about 99% of variance
png(filename="images/pcas.png", width = 800, height = 600)
cumsum(pca.fit$sdev^2)/sum(pca.fit$sdev^2)
par(cex=2)
plot(x=1:21, cumsum(pca.fit$sdev^2)/sum(pca.fit$sdev^2)*100
     , xlab="Number of Principal Components"
     , ylab="Variance Explained (%)"
     , type="l"
     , lwd=2)
dev.off()

## Repeat procedure and find optimal number of components
errors <- sapply(c(9:21)
                 , function(x) { 
                   fit.pca <- ksvm(x=as.matrix(b.pca.faults[, c(1:x)])
                               , y=b.pca.faults$fault
                               , type="C-svc"
                               , kernel="anovadot"
                               , cross = folds)
                   fit <- ksvm(x=as.matrix(b.faults[, c(1:x)])
                                   , y=b.faults$fault
                                   , type="C-svc"
                                   , kernel="anovadot"
                                   , cross = folds)
                   c(attributes(fit)$cross*100
                     , attributes(fit.pca)$cross*100) } )

## Visualise results, minumal error about 1.2% with 18 PCs
png(filename="images/pcaEffect.png", width = 800, height = 800)
par(cex=2)
plot(x=9:21, errors[1, ]
     , xlab="Number of Principal Components / Features"
     , ylab="Cross Validation Error Rate (%)"
     , type="l", lwd=2, col=1, ylim=c(0, 5.8))
points(x=9:21, errors[2, ]
       , xlab="Number of Principal Components / Features"
       , ylab="Cross Validation Error Rate (%)"
       , type="l", lwd=2, col=2)
legend("topright", lty=1, legend=c("Features", "PCs"), lwd=2, col=c(1, 2))
abline(h=min(errors[2, ]), lty=2, lwd=2)
dev.off()

## Part 6
## Fit k-class SVM and compare results with standard
## "One-Against-One" approach
##

errors <- sapply(c("C-svc", "spoc-svc", "kbb-svc")
                 , function(method) { 
                   fit <- ksvm(x=as.matrix(b.pca.faults[, 1:18])
                               , y=b.pca.faults$fault
                               , type=method
                               , kernel="anovadot"
                               , cross = folds)
                   attributes(fit)$cross*100 } )

alg.time <- sapply(c("C-svc", "spoc-svc", "kbb-svc")
                 , function(method) { 
                   system.time(ksvm(x=as.matrix(b.pca.faults[, 1:18])
                               , y=b.pca.faults$fault
                               , type=method
                               , kernel="anovadot"
                               , cross = folds))[3] } )

names(errors) <- c("One-Against-One", "Crammer multi-class", "Weston, Watkins multi-class")
alg.results <- rbind(errors, alg.time)

## Show results 
alg.results

## Part 7
## Find best parametrization for SVN using PSO
##

results <- c()
## Best parametrization
fc <- function(x) {
  fit <- ksvm(x=as.matrix(b.pca.faults[, 1:18])
              , y=b.pca.faults$fault
              , type="C-svc"
              , kpar=list(sigma=x[1], degree=x[2])
              , C=x[3]
              , kernel="anovadot"
              , cross = folds)
  results <<- c(results, attributes(fit)$cross*100)
  attributes(fit)$cross*100
}
system.time(
  optim.c <- psoptim(c(1, 1, 1), fc, lower = c(.5, .5, .001), upper = c(2, 2, 100), control = list(maxit = 25, trace=1, trace.stats=TRUE)))

## elapsed time ~ 72.5 min. for 25 iterations
## fitness = 0.8719807, sigma = 0.7657325, degree = 1.0310232, C=20.6512650 

## Visualize results
png(filename="images/optimization.png", width = 800, height = 800)
par(cex=2)
res <- data.frame(call=1:length(results[results < 3]), fitness=results[results < 3])
plot(res, pch=19
     , xlab= "Evaluate Call"
     , ylab= "Cross Validation Error Rate (%)"
     , ylim=c(.5, 2.7))
abline(lm(fitness ~., res), lwd=4, col=2, lty=2)
points(which(min(res$fitness) == res$fitness), min(res$fitness), pch=19, col="red")
legend("topright", legend="Optimal Value (0.87)", pch=19, col="red")
dev.off()

##
## Part 8. Predict final results
##

## Fit final model 
fit <- ksvm(x=as.matrix(b.pca.faults[, 1:18])
            , y=b.pca.faults$fault
            , type="C-svc"
            , kpar=list(sigma=optim.c$par[1], degree=optim.c$par[2])
            , C=optim.c$par[3]
            , kernel="anovadot")

## Print final results
results.acc <- table(predict(fit, pca.faults[, 1:18]), pca.faults$fault) 
100 - sum(diag(results.acc))/sum(results.acc)*100
results.acc

##
## Part 9. decrease interval 
##

## Set random seed
set.seed(20150626)

d.windows <- sapply(c(1/2, 1, 3, 5, 10, 15, 20), function(x) {
  time <- system.time( {
    ## set time window
    seconds <<- x
    
    ## Load transformed data
    source("resources/load.R")
    
    ## Extract features
    pca.fit <- princomp(faults[, -c(1, 2, 24)], cor=TRUE)
    
    ## Get datasets
    all.list <- list(data.frame(pca.fit$scores[faults$fault == "Failure 1", ], factor("Failure 1"))
                     , data.frame(pca.fit$scores[faults$fault == "Failure 2", ], factor("Failure 2"))
                     , data.frame(pca.fit$scores[faults$fault == "Failure 3", ], factor("Failure 3"))
                     , data.frame(pca.fit$scores[faults$fault == "Normal 1", ], factor("Normal 1"))
                     , data.frame(pca.fit$scores[faults$fault == "Normal 2", ], factor("Normal 2"))
                     , data.frame(pca.fit$scores[faults$fault == "Normal 3", ], factor("Normal 3")))
    pca.faults <- data.frame(pca.fit$scores, faults$fault)
    b.pca.faults <- balance.multi(data = all.list, method = "smean", technique = list('under' = rus, 'over' = ros))
    b.pca.faults <- do.call(rbind.data.frame, b.pca.faults)
    
    ## Built classifier an return error rate
    fit <- ksvm(x=as.matrix(b.pca.faults[, 1:18])
                , y=b.pca.faults$fault
                , type="C-svc"
                , kpar=list(sigma=optim.c$par[1], degree=optim.c$par[2])
                , C=optim.c$par[3]
                , kernel="anovadot")
    
    ## Print final results
    results.acc <- table(predict(fit, pca.faults[, 1:18]), pca.faults$fault) 
    results.acc <- 100 - sum(diag(results.acc))/sum(results.acc)*100 })[3]
  c(time, results.acc)
  
})

##
## Part 10. Explore one-class svm
##

## Set random seed
set.seed(20150626)

## Make train and test samples
n.obs <- dim(n.1)[1]
intrain <- sample(1:n.obs, n.obs*.8) 

## Make test/train samples
train <- n.1[intrain, 3:24]
test <- rbind(n.1[-intrain, ], f.1, f.2, f.3)[, 3:24]

## Fit final model 
fit <- ksvm(x=as.matrix(train[, -22])
            , type="one-svc"
            , kpar=list(sigma=1, degree=1)
            , kernel="anovadot")

table(predict(fit, test[, -22]), test[, 22])
