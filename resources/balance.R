# perform random undersampling
# x input
# y response
# n number of instances to remove
# rate undersampling rate

rus <- function(x, y, n, rate = 0){
  data = cbind(x, 'fault' = y)
  if(missing(n)) {n = nrow(data)*rate}
  data = data[-sample(nrow(data), n, replace = F),]
  
  return(data)
}

# perform random oversampling
# x input
# y response
# n number of instances to add
# rate oversampling rate

ros <- function(x, y, n, rate = 0){
  data = cbind(x, 'fault' = y)
  if(missing(n)) {n = nrow(data)*rate}
  data = rbind(data, data[sample(nrow(data), n, replace = T),])
    
  return(data)
}

# balance multi-class data
# reference: Seliya, N., Xu, Z., and Khoshgoftaar, T. M. Addressing Class Imbalance in Non-Binary Classification Problems. 2008
# data a list of dataframes
# method a selection of methods
# technique a list of undersampling and oversampling techniques

balance.multi <- function(data, method = c("smean", "smedian"), technique = list('under' = rus, 'over' = ros)){
  method = match.arg(method)
  under = technique$under
  over = technique$over
  
  switch(method,
         smean = {
           m = ceiling(mean(sapply(data, nrow)))
           data = lapply(data, function(x){
             d = nrow(x) - m
             if(d > 0){
               ind <- ncol(x)
               under(x[, -ind], x[, ind], n = abs(d))
             } else {
               ind <- ncol(x)
               over(x[, -ind], x[, ind], n = abs(d))
             }
           })
         },
         smedian = {
           m = median(unlist(lapply(data, nrow)))
           data = lapply(data, function(x){
             d = nrow(x) - m
             if(d > 0){
               ind <- ncol(x)
               under(x[, -ind], x[, ind], n = abs(d))
             } else {
               ind <- ncol(x)
               over(x[, -ind], x[, ind], n = abs(d))
             }
           })
         })
  
  return(data)
}

### corrected ubOSS function from unbalanced package 
ubOSSx <- function (X, Y, verbose = T) 
{
  is.not.num <- which(sapply(X, is.numeric) == FALSE)
  if (length(is.not.num) > 0) 
    stop("only numeric features are allowed to compute nearest neighbors")
  S.X <- X
  S.Y <- Y
  i.1 <- which(Y == 1)
  N.1 <- length(i.1)
  i.0 <- which(Y == 0)
  N.0 <- length(i.0)
  if (N.1 == 0 | N.0 == 0) {
    cat("all instances of the same class \n")
    return(list(X = X, Y = Y))
  }
  id.C <- c(i.1, sample(i.0, 1))
  C.X <- X[id.C, ]
  C.Y <- Y[id.C]
  Y.knn <- knn(C.X, S.X, C.Y, k = 1)
  id.miss <- which(S.Y != Y.knn)
  id.C <- c(id.C, id.miss)
  id.C <- sample(id.C)
  C.X <- X[id.C, ]
  C.Y <- Y[id.C]
  ## original line: data <- ubTomek(X, Y, verbose)
  ## the revised version feeds the new dataset C.X and C.Y into ubTomek instead of the original one
  data <- ubTomek(C.X, C.Y, verbose)
  X <- data$X
  Y <- data$Y
  return(list(X = X, Y = Y))
}