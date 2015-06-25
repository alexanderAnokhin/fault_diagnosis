# perform random undersampling
# data a dataframe to undersample
# n number of instances to remove
# rate undersampling rate

rus <- function(data, n, rate = 0){
  if(missing(n)) {n = nrow(data)*rate}
  data = data[-sample(nrow(data), n, replace = F),]
  
  return(data)
}

# perform random oversampling
# data a dataframe to oversample
# n number of instances to add
# rate oversampling rate

ros <- function(data, n, rate = 0){
  if(missing(n)) {n = nrow(data)*rate}
  data = rbind(data, data[sample(nrow(data), n, replace = T),])
  
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
           maj = under(maj, rem)
         },
         o = {
           if(oRate == 0) {add = nrow(maj)/ratio - nrow(min)}
           min = over(min, add)
         },
         b = {
           if(oRate == 0 || uRate == 0) break
           maj = under(maj, rate = uRate)
           min = over(min, rate = oRate)                       
         })
  
  data = list(min, maj)
  return(data)
}

# balance multi-class data
# references: Seliya, N., Xu, Z., and Khoshgoftaar, T. M. Addressing Class Imbalance in Non-Binary Classification Problems. 2008
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
               under(x, n = abs(d))
             } else {
               over(x, n = abs(d))
             }
           })
         },
         smedian = {
           m = median(unlist(lapply(data, nrow)))
           data = lapply(data, function(x){
             d = nrow(x) - m
             if(d > 0){
               under(x, n = abs(d))
             } else {
               over(x, n = abs(d))
             }
           })
         })
  
  return(data)
}