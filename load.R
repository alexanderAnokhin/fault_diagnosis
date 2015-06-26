library(dplyr)
library(moments)

## Read data
load("data/failure_1.RData")
load("data/failure_2.RData")
load("data/failure_3.RData")

load("data/normal_1.RData")
load("data/normal_2.RData")
load("data/normal_3.RData")

## How many seconds in one split
seconds <- 5

## Persentage to be considered
q <- .01

## Transform data
f.1 <- tbl_df(failure_1) %>%
  group_by(cycle) %>%
  mutate(split = 1:n() %/% (2048*seconds)) %>%
  group_by(cycle, split) %>%
  summarise(s1.mean = mean(sensor_1)
            , s1.sd = sd(sensor_1)
            , s1.peak = max(sensor_1)
            , s1.rms = sqrt(mean(sensor_1^2))
            , s1.cf = max(sensor_1)/sqrt(mean(sensor_1^2))
            , s1.sk = skewness(sensor_1)
            , s1.ku = kurtosis(sensor_1)
            , s2.mean = mean(sensor_2)
            , s2.sd = sd(sensor_2)
            , s2.peak = max(sensor_2)
            , s2.rms = sqrt(mean(sensor_2^2))
            , s2.cf = max(sensor_2)/sqrt(mean(sensor_2^2))
            , s2.sk = skewness(sensor_2)
            , s2.ku = kurtosis(sensor_2)
            , s3.mean = mean(sensor_3)
            , s3.sd = sd(sensor_3)
            , s3.peak = max(sensor_3)
            , s3.rms = sqrt(mean(sensor_3^2))
            , s3.cf = max(sensor_3)/sqrt(mean(sensor_3^2))
            , s3.sk = skewness(sensor_3)
            , s3.ku = kurtosis(sensor_3)) %>%
  filter(complete.cases(s1.sd, s2.sd, s3.sd)) %>%
  mutate(fault = as.factor("Failure 1")) %>%
  group_by(cycle) %>%
  ## filter splits with stable signal
  filter(s1.sd >= quantile(s1.sd, q)) %>%
  data.frame()

f.2 <- tbl_df(failure_2) %>%
  group_by(cycle) %>%
  mutate(split = 1:n() %/% (2048*seconds)) %>%
  group_by(cycle, split) %>%
  summarise(s1.mean = mean(sensor_1)
            , s1.sd = sd(sensor_1)
            , s1.peak = max(sensor_1)
            , s1.rms = sqrt(mean(sensor_1^2))
            , s1.cf = max(sensor_1)/sqrt(mean(sensor_1^2))
            , s1.sk = skewness(sensor_1)
            , s1.ku = kurtosis(sensor_1)
            , s2.mean = mean(sensor_2)
            , s2.sd = sd(sensor_2)
            , s2.peak = max(sensor_2)
            , s2.rms = sqrt(mean(sensor_2^2))
            , s2.cf = max(sensor_2)/sqrt(mean(sensor_2^2))
            , s2.sk = skewness(sensor_2)
            , s2.ku = kurtosis(sensor_2)
            , s3.mean = mean(sensor_3)
            , s3.sd = sd(sensor_3)
            , s3.peak = max(sensor_3)
            , s3.rms = sqrt(mean(sensor_3^2))
            , s3.cf = max(sensor_3)/sqrt(mean(sensor_3^2))
            , s3.sk = skewness(sensor_3)
            , s3.ku = kurtosis(sensor_3)) %>%
  filter(complete.cases(s1.sd, s2.sd, s3.sd)) %>%
  mutate(fault = as.factor("Failure 2")) %>%
  group_by(cycle) %>%
  ## filter splits with stable signal
  filter(s1.sd >= quantile(s1.sd, q)) %>%
  data.frame()

f.3 <- tbl_df(failure_3) %>%
  group_by(cycle) %>%
  mutate(split = 1:n() %/% (2048*seconds)) %>%
  group_by(cycle, split) %>%
  summarise(s1.mean = mean(sensor_1)
            , s1.sd = sd(sensor_1)
            , s1.peak = max(sensor_1)
            , s1.rms = sqrt(mean(sensor_1^2))
            , s1.cf = max(sensor_1)/sqrt(mean(sensor_1^2))
            , s1.sk = skewness(sensor_1)
            , s1.ku = kurtosis(sensor_1)
            , s2.mean = mean(sensor_2)
            , s2.sd = sd(sensor_2)
            , s2.peak = max(sensor_2)
            , s2.rms = sqrt(mean(sensor_2^2))
            , s2.cf = max(sensor_2)/sqrt(mean(sensor_2^2))
            , s2.sk = skewness(sensor_2)
            , s2.ku = kurtosis(sensor_2)
            , s3.mean = mean(sensor_3)
            , s3.sd = sd(sensor_3)
            , s3.peak = max(sensor_3)
            , s3.rms = sqrt(mean(sensor_3^2))
            , s3.cf = max(sensor_3)/sqrt(mean(sensor_3^2))
            , s3.sk = skewness(sensor_3)
            , s3.ku = kurtosis(sensor_3)) %>%
  filter(complete.cases(s1.sd, s2.sd, s3.sd)) %>%
  mutate(fault = as.factor("Failure 3")) %>%
  group_by(cycle) %>%
  ## filter splits with stable signal
  filter(s1.sd >= quantile(s1.sd, q)) %>%
  data.frame()

n.1 <- tbl_df(normal_1) %>%
  group_by(cycle) %>%
  mutate(split = 1:n() %/% (2048*seconds)) %>%
  group_by(cycle, split) %>%
  summarise(s1.mean = mean(sensor_1)
            , s1.sd = sd(sensor_1)
            , s1.peak = max(sensor_1)
            , s1.rms = sqrt(mean(sensor_1^2))
            , s1.cf = max(sensor_1)/sqrt(mean(sensor_1^2))
            , s1.sk = skewness(sensor_1)
            , s1.ku = kurtosis(sensor_1)
            , s2.mean = mean(sensor_2)
            , s2.sd = sd(sensor_2)
            , s2.peak = max(sensor_2)
            , s2.rms = sqrt(mean(sensor_2^2))
            , s2.cf = max(sensor_2)/sqrt(mean(sensor_2^2))
            , s2.sk = skewness(sensor_2)
            , s2.ku = kurtosis(sensor_2)
            , s3.mean = mean(sensor_3)
            , s3.sd = sd(sensor_3)
            , s3.peak = max(sensor_3)
            , s3.rms = sqrt(mean(sensor_3^2))
            , s3.cf = max(sensor_3)/sqrt(mean(sensor_3^2))
            , s3.sk = skewness(sensor_3)
            , s3.ku = kurtosis(sensor_3)) %>%
  filter(complete.cases(s1.sd, s2.sd, s3.sd)) %>%
  mutate(fault = as.factor("Normal 1")) %>%
  group_by(cycle) %>%
  ## filter splits with stable signal
  filter(s1.sd >= quantile(s1.sd, q)) %>%
  data.frame()

n.2 <- tbl_df(normal_2) %>%
  group_by(cycle) %>%
  mutate(split = 1:n() %/% (2048*seconds)) %>%
  group_by(cycle, split) %>%
  summarise(s1.mean = mean(sensor_1)
            , s1.sd = sd(sensor_1)
            , s1.peak = max(sensor_1)
            , s1.rms = sqrt(mean(sensor_1^2))
            , s1.cf = max(sensor_1)/sqrt(mean(sensor_1^2))
            , s1.sk = skewness(sensor_1)
            , s1.ku = kurtosis(sensor_1)
            , s2.mean = mean(sensor_2)
            , s2.sd = sd(sensor_2)
            , s2.peak = max(sensor_2)
            , s2.rms = sqrt(mean(sensor_2^2))
            , s2.cf = max(sensor_2)/sqrt(mean(sensor_2^2))
            , s2.sk = skewness(sensor_2)
            , s2.ku = kurtosis(sensor_2)
            , s3.mean = mean(sensor_3)
            , s3.sd = sd(sensor_3)
            , s3.peak = max(sensor_3)
            , s3.rms = sqrt(mean(sensor_3^2))
            , s3.cf = max(sensor_3)/sqrt(mean(sensor_3^2))
            , s3.sk = skewness(sensor_3)
            , s3.ku = kurtosis(sensor_3)) %>%
  filter(complete.cases(s1.sd, s2.sd, s3.sd)) %>%
  mutate(fault = as.factor("Normal 2")) %>%
  group_by(cycle) %>%
  ## filter splits with stable signal
  filter(s1.sd >= quantile(s1.sd, q)) %>%
  data.frame()

n.3 <- tbl_df(normal_3) %>%
  group_by(cycle) %>%
  mutate(split = 1:n() %/% (2048*seconds)) %>%
  group_by(cycle, split) %>%
  summarise(s1.mean = mean(sensor_1)
            , s1.sd = sd(sensor_1)
            , s1.peak = max(sensor_1)
            , s1.rms = sqrt(mean(sensor_1^2))
            , s1.cf = max(sensor_1)/sqrt(mean(sensor_1^2))
            , s1.sk = skewness(sensor_1)
            , s1.ku = kurtosis(sensor_1)
            , s2.mean = mean(sensor_2)
            , s2.sd = sd(sensor_2)
            , s2.peak = max(sensor_2)
            , s2.rms = sqrt(mean(sensor_2^2))
            , s2.cf = max(sensor_2)/sqrt(mean(sensor_2^2))
            , s2.sk = skewness(sensor_2)
            , s2.ku = kurtosis(sensor_2)
            , s3.mean = mean(sensor_3)
            , s3.sd = sd(sensor_3)
            , s3.peak = max(sensor_3)
            , s3.rms = sqrt(mean(sensor_3^2))
            , s3.cf = max(sensor_3)/sqrt(mean(sensor_3^2))
            , s3.sk = skewness(sensor_3)
            , s3.ku = kurtosis(sensor_3)) %>%
  filter(complete.cases(s1.sd, s2.sd, s3.sd)) %>%
  mutate(fault = as.factor("Normal 3")) %>%
  group_by(cycle) %>%
  ## filter splits with stable signal
  filter(s1.sd >= quantile(s1.sd, q)) %>%
  data.frame()

rm(failure_1); rm(failure_2); rm(failure_3)
rm(normal_1); rm(normal_2); rm(normal_3)

## Combine all data together
faults <- rbind(f.1, f.2, f.3, n.1, n.2, n.3)

## Calulate weights of classes
weights <- table(faults$fault)