library(dplyr)

## Read data
load("data/failure_1.RData")
load("data/failure_2.RData")
load("data/failure_3.RData")

load("data/normal_1.RData")
load("data/normal_2.RData")
load("data/normal_3.RData")

## How many seconds in one split
seconds <- 5

## Transform data
f.1 <- tbl_df(failure_1) %>%
  group_by(cycle) %>%
  mutate(split = 1:n() %/% 2048*seconds) %>%
  group_by(cycle, split) %>%
  summarise(s1.mean = mean(sensor_1)
            , s1.sd = sd(sensor_1)
            , s1.range = max(sensor_1) - min(sensor_1)
            , s2.mean = mean(sensor_2)
            , s2.sd = sd(sensor_2)
            , s2.range = max(sensor_2) - min(sensor_2)
            , s3.mean = mean(sensor_3)
            , s3.sd = sd(sensor_3)
            , s3.range = max(sensor_3) - min(sensor_3)) %>%
  filter(complete.cases(s1.sd, s2.sd, s3.sd)) %>%
  mutate(fault = as.factor("Failure 1"))

f.2 <- tbl_df(failure_2) %>%
  group_by(cycle) %>%
  mutate(split = 1:n() %/% 2048*seconds) %>%
  group_by(cycle, split) %>%
  summarise(s1.mean = mean(sensor_1)
            , s1.sd = sd(sensor_1)
            , s1.range = max(sensor_1) - min(sensor_1)
            , s2.mean = mean(sensor_2)
            , s2.sd = sd(sensor_2)
            , s2.range = max(sensor_2) - min(sensor_2)
            , s3.mean = mean(sensor_3)
            , s3.sd = sd(sensor_3)
            , s3.range = max(sensor_3) - min(sensor_3)) %>%
  filter(complete.cases(s1.sd, s2.sd, s3.sd)) %>%
  mutate(fault = as.factor("Failure 2"))

f.3 <- tbl_df(failure_3) %>%
  group_by(cycle) %>%
  mutate(split = 1:n() %/% 2048*seconds) %>%
  group_by(cycle, split) %>%
  summarise(s1.mean = mean(sensor_1)
            , s1.sd = sd(sensor_1)
            , s1.range = max(sensor_1) - min(sensor_1)
            , s2.mean = mean(sensor_2)
            , s2.sd = sd(sensor_2)
            , s2.range = max(sensor_2) - min(sensor_2)
            , s3.mean = mean(sensor_3)
            , s3.sd = sd(sensor_3)
            , s3.range = max(sensor_3) - min(sensor_3)) %>%
  filter(complete.cases(s1.sd, s2.sd, s3.sd)) %>%
  mutate(fault = as.factor("Failure 3"))

n.1 <- tbl_df(normal_1) %>%
  group_by(cycle) %>%
  mutate(split = 1:n() %/% 2048*seconds) %>%
  group_by(cycle, split) %>%
  summarise(s1.mean = mean(sensor_1)
            , s1.sd = sd(sensor_1)
            , s1.range = max(sensor_1) - min(sensor_1)
            , s2.mean = mean(sensor_2)
            , s2.sd = sd(sensor_2)
            , s2.range = max(sensor_2) - min(sensor_2)
            , s3.mean = mean(sensor_3)
            , s3.sd = sd(sensor_3)
            , s3.range = max(sensor_3) - min(sensor_3)) %>%
  filter(complete.cases(s1.sd, s2.sd, s3.sd)) %>%
  mutate(fault = as.factor("Normal 1"))

n.2 <- tbl_df(normal_2) %>%
  group_by(cycle) %>%
  mutate(split = 1:n() %/% 2048*seconds) %>%
  group_by(cycle, split) %>%
  summarise(s1.mean = mean(sensor_1)
            , s1.sd = sd(sensor_1)
            , s1.range = max(sensor_1) - min(sensor_1)
            , s2.mean = mean(sensor_2)
            , s2.sd = sd(sensor_2)
            , s2.range = max(sensor_2) - min(sensor_2)
            , s3.mean = mean(sensor_3)
            , s3.sd = sd(sensor_3)
            , s3.range = max(sensor_3) - min(sensor_3)) %>%
  filter(complete.cases(s1.sd, s2.sd, s3.sd)) %>%
  mutate(fault = as.factor("Normal 2"))

n.3 <- tbl_df(normal_3) %>%
  group_by(cycle) %>%
  mutate(split = 1:n() %/% 2048*seconds) %>%
  group_by(cycle, split) %>%
  summarise(s1.mean = mean(sensor_1)
            , s1.sd = sd(sensor_1)
            , s1.range = max(sensor_1) - min(sensor_1)
            , s2.mean = mean(sensor_2)
            , s2.sd = sd(sensor_2)
            , s2.range = max(sensor_2) - min(sensor_2)
            , s3.mean = mean(sensor_3)
            , s3.sd = sd(sensor_3)
            , s3.range = max(sensor_3) - min(sensor_3)) %>%
  filter(complete.cases(s1.sd, s2.sd, s3.sd)) %>%
  mutate(fault = as.factor("Normal 3"))

rm(failure_1)
rm(failure_2)
rm(failure_3)
rm(normal_1)
rm(normal_2)
rm(normal_3)