# balance the distributions between two classes
# data a list of dataframes
# method a selection of methods
# uRate undersampling rate
# oRate oversampling rate
# ratio ratio of majority:minority

balance <- function(data, method = c("rus", "ros", "b"), uRate = 0, oRate = 0, ratio = 1) {
  method = match.arg(method)
  rem = nrow(maj)*uRate
  add = nrow(min)*oRate
  
  # set majority and minority classes
  if(nrow(data[[1]]) >= nrow(data[[2]])){
    maj = data[[1]]
    min = data[[2]]
  }
  else {
    maj = data[[2]]
    min = data[[1]]
  }
    
  switch(method,
         rus = {
           if(uRate == 0) {rem = nrow(maj) - (ratio*nrow(min))}
           maj = maj[-sample(nrow(maj), rem, replace = F),]
         },
         ros = {
           if(oRate == 0) {add = nrow(maj) - (ratio*nrow(min))}
           min = rbind(min, min[sample(nrow(min), add, replace = T),])
         },
         b = {
           if(oRate == 0 || uRate == 0) break
           rem = nrow(maj)*uRate
           add = nrow(min)*oRate
           maj = maj[-sample(nrow(maj), rem, replace = F),]
           min = rbind(min, min[sample(nrow(min), add, replace = T),])                       
         },
         stop("Invalid method"))
  
  data = list(maj, min)
  return(data)
}