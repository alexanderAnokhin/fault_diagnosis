setwd("~/Documents/Uni_Munster/DA in Spare Parts Management/fault_diagnosis")

library(ggplot2)

## Load transformed data
source("load.R")

data <- rbind(f.1, f.2, f.3, n.1, n.2, n.3)
plot(data$s1.sd, data$s2.sd, pch=".", col=data$fault)
