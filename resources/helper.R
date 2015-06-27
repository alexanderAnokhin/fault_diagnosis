ksearch <- function(data, nc=15){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  diff2 <- diff(abs(diff(wss)))
  res <- which(abs(diff2) == max(abs(diff2))) + 1
  
  return(res)
}