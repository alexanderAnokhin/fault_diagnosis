setwd("~/Documents/Uni_Munster/DA in Spare Parts Management/Failure Pattern Recognition")

library(stringr)

getData <- function(path) {
  sensor.data <- read.table(path
                     , sep = "\t", dec = ","
                     , colClasses = c("numeric")
                     , col.names = c("measure"))
  pattern <- "(.+_[1-3])\\/.+([1-3])\\/[^0-9]+([0-9]+.+)\\.txt"
  groups <- str_match(string = path, pattern = pattern)
  sensor.data$cycle <- factor(groups[1, 4])
  sensor.data$sensor <- factor(groups[1, 3])
  sensor.data
}

getDataFrame <- function(path) {
  files <- list.files(path = path, recursive = TRUE, pattern = "*.txt")
  tmp.data <- data.frame()
  for(file in files) {
    tmp.data <- rbind(tmp.data, getData(paste0(path, file)))
  }
  tmp.data
}
    
# get all paths
failure_1 <- getDataFrame("Failure_1/")
save(failure_1, file = "failure_1.RData")
rm(failure_1)

failure_2 <- getDataFrame("Failure_2/")
save(failure_2, file = "failure_2.RData")
rm(failure_2)

failure_3 <- getDataFrame("Failure_3/")
save(failure_3, file = "failure_3.RData")
rm(failure_3)

normal_1 <- getDataFrame("Normal_1/")
save(normal_1, file = "normal_1.RData")
rm(normal_1)

normal_2 <- getDataFrame("Normal_2/")
save(normal_2, file = "normal_2.RData")
rm(normal_2)

normal_3 <- getDataFrame("Normal_3/")
save(normal_3, file = "normal_3.RData")
rm(normal_3)

# transform data and save them one more time
load("failure_1.RData")
failure_1 <- cbind(subset(failure_1, sensor == 1, select = 1)
                  , subset(failure_1, sensor == 2, select = 1)
                  , subset(failure_1, sensor == 3, select = c(1, 2)))
names(failure_1) <- c("sensor_1", "sensor_2", "sensor_3", "cycle")
save(failure_1, file = "failure_1_merged.RData")
rm(failure_1)

load("failure_2.RData")
failure_2 <- cbind(subset(failure_2, sensor == 1, select = 1)
                   , subset(failure_2, sensor == 2, select = 1)
                   , subset(failure_2, sensor == 3, select = c(1, 2)))
names(failure_2) <- c("sensor_1", "sensor_2", "sensor_3", "cycle")
save(failure_2, file = "failure_2_merged.RData")
rm(failure_2)

load("failure_3.RData")
failure_3 <- cbind(subset(failure_3, sensor == 1, select = 1)
                   , subset(failure_3, sensor == 2, select = 1)
                   , subset(failure_3, sensor == 3, select = c(1, 2)))
names(failure_3) <- c("sensor_1", "sensor_2", "sensor_3", "cycle")
save(failure_3, file = "failure_3_merged.RData")
rm(failure_3)

load("normal_1.RData")
normal_1 <- cbind(subset(normal_1, sensor == 1, select = 1)
                   , subset(normal_1, sensor == 2, select = 1)
                   , subset(normal_1, sensor == 3, select = c(1, 2)))
names(normal_1) <- c("sensor_1", "sensor_2", "sensor_3", "cycle")
save(normal_1, file = "normal_1_merged.RData")
rm(normal_1)

load("normal_2.RData")
normal_2 <- cbind(subset(normal_2, sensor == 1, select = 1)
                  , subset(normal_2, sensor == 2, select = 1)
                  , subset(normal_2, sensor == 3, select = c(1, 2)))
names(normal_2) <- c("sensor_1", "sensor_2", "sensor_3", "cycle")
save(normal_2, file = "normal_2_merged.RData")
rm(normal_2)

load("normal_3.RData")
normal_3 <- cbind(subset(normal_3, sensor == 1, select = 1)
                  , subset(normal_3, sensor == 2, select = 1)
                  , subset(normal_3, sensor == 3, select = c(1, 2)))
names(normal_3) <- c("sensor_1", "sensor_2", "sensor_3", "cycle")
save(normal_3, file = "normal_3_merged.RData")
rm(normal_3)
