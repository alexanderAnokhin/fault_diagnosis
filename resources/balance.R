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

# perform cluster-based undersampling
# reference: 2013 Hu, Zhang - Clustering-Based Subset Ensemble Learning Method for Imbalanced Data
# x input
# y response
# n number of instances to remove
# rate undesampling rate

cbus <- function(x, y, n, rate = 0) {
  if(missing(n)) {n = nrow(x)*rate}
  target.size = nrow(x) - n
  k = nrow(x)/target.size
  if(k < 2) {
    data = rus(x, y, n)
  } else {
    clust = kmeans(x, k)
    ind = ncol(x)
    temp = cbind(x, 'cluster' = clust$cluster, 'fault' = y)
    data = data.frame(matrix(, ncol = ind + 1))
    names(data) = names(temp)[-(ind+1)]
    for(i in 1:length(clust$size)) {
      rem = nrow(x) - target.size*clust$size[i]/nrow(x)
      df = filter(temp, cluster == i)
      data = rbind(data, rus(x = df[, 1:ind], y = df$fault, n = rem)) 
    } 
  }
  
  return(data)
}

# perform cluster-based oversampling
# adapted from: 2013 Hu, Zhang - Clustering-Based Subset Ensemble Learning Method for Imbalanced Data
# x input
# y response
# n number of instances to add
# rate oversampling rate

cbos <- function(x, y, n, rate = 0) {
  if(missing(n)) {n = nrow(x)*rate}
  target.size = nrow(x) + n
  k = target.size/nrow(x)
  if(k < 2) {
    data = ros(x, y, n)
  } else {
    clust = kmeans(x, k)
    ind = ncol(x)
    temp = cbind(x, 'cluster' = clust$cluster, 'fault' = y)
    data = data.frame(matrix(, ncol = ind + 1))
    names(data) = names(temp)[-(ind+1)]
    for(i in 1:length(clust$size)) {
      add = n*clust$size[i]/nrow(x)
      df = filter(temp, cluster == i)
      data = rbind(data, ros(x = df[, 1:ind], y = df$fault, n = add)) 
    } 
  }
  
  return(data)
}

# balance the distributions between two classes
# data a list of dataframes
# method a selection of methods
# technique a list of undersampling and oversampling techniques
# uRate undersampling rate
# oRate oversampling rate
# ratio ratio of majority:minority

balance <- function(data, method = c("u", "o", "b"), technique = list('under' = rus, 'over' = ros), uRate = 0, oRate = 0, ratio = 1){
  method = match.arg(method)
  under = technique$under
  over = technique$over
  
  # set majority and minority classes
  if(nrow(data[[1]]) >= nrow(data[[2]])){
    maj = data[[1]]
    min = data[[2]]
  } else {
    maj = data[[2]]
    min = data[[1]]
  }
  
  rem = nrow(maj)*uRate
  add = nrow(min)*oRate
    
  switch(method,
         u = {
           if(uRate == 0) {rem = nrow(maj) - (ratio*nrow(min))}
           ind <- ncol(maj)
           maj = under(maj[, -ind], maj[, ind], rem)
         },
         o = {
           if(oRate == 0) {add = nrow(maj)/ratio - nrow(min)}
           ind <- ncol(min)
           min = over(min[, -ind], min[, ind], add)
         },
         b = {
           if(oRate == 0 || uRate == 0) break
           
           ind <- ncol(maj)
           maj = under(maj[, -ind], maj[, ind], rate = uRate)
           
           ind <- ncol(min)
           min = over(min[, -ind], min[, ind], rate = oRate)                       
         })
  
  data = list(min, maj)
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