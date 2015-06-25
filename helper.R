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
    p <- m[1, 1]/sum(m[1, ])
    r <- m[1, 1]/sum(m[, 1])
    2*(p*r)/(p+r)
  })
  
  return(mean(res))
}