## Alternative ksvm function with manual cross-validation to accomodate other performance measures
cv.ksvm <- function(x, y, folds, ...) {
  k.rows <- split(sample(nrow(x)), rep(1:folds, length=nrow(x)))
  temp <- lapply(k.rows, function(a) {
    k.svm <- ksvm(x=x[-a, ]
                  , y=y[-a]
                  , ...)
    cbind(predict(k.svm, x[a, ,drop = F]), y[a])    
  })
  res <- list(lapply(temp, function(x) x[, 1]), lapply(temp, function(x) x[, 2]))
  names(res) <- c('predictions', 'labels')
  
  return(res)
}

f.cv <- function(x) {
  pred <- x[[1]]
  lab <- x[[2]]
  res <- sapply(1:length(pred), function(i) {
    m <- table(pred[[i]], lab[[i]])
    if(all(dim(m) != 2)){
      ifelse(all(pred[[i]] == lab[[i]]) & all(lab[[i]] == 1), 1, 0)
    } else {
      p <- m[1, 1]/sum(m[1, ])
      r <- m[1, 1]/sum(m[, 1])
      2*(p*r)/(p+r)
    }
  })
  
  return(res)
}

gmean.cv <- function(x) {
  pred <- x[[1]]
  lab <- x[[2]]
  res <- sapply(1:length(pred), function(i) {
    m <- table(pred[[i]], lab[[i]])
    if(all(dim(m) != 2)){
      ifelse(all(pred[[i]] == lab[[i]]), 1, 0)
    } else {
      tpr <- m[1, 1]/sum(m[1, ])
      tnr <- m[2, 2]/sum(m[2, ])
      sqrt(tpr*tnr)
    }
  })
  
  return(res)
}

ksearch <- function(data, nc=15){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  diff2 <- diff(abs(diff(wss)))
  res <- which(abs(diff2) == max(abs(diff2))) + 1
  
  return(res)
  }