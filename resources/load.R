library(dplyr)
library(moments)

## Read data
load("./data/failure_1.RData")
load("./data/failure_2.RData")
load("./data/failure_3.RData")

load("./data/normal_1.RData")
load("./data/normal_2.RData")
load("./data/normal_3.RData")

#### Persentage to be considered
q <- .05

## Transform data
f.1 <- tbl_df(failure_1) %>%
  ## filter data at first
  group_by(cycle) %>%
  ## every split lasts 1/16 of second
  mutate(split = 1:n() %/% 128) %>%
  group_by(cycle, split) %>%
  mutate(s1.sd = sd(sensor_1)) %>%
  group_by(cycle) %>%
  filter(s1.sd >= quantile(s1.sd, q, na.rm=TRUE)) %>%
  ## aggregate data
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
  data.frame()

f.2 <- tbl_df(failure_2) %>%
  ## filter data at first
  group_by(cycle) %>%
  ## every split lasts 1/16 of second
  mutate(split = 1:n() %/% 128) %>%
  group_by(cycle, split) %>%
  mutate(s1.sd = sd(sensor_1)) %>%
  group_by(cycle) %>%
  filter(s1.sd >= quantile(s1.sd, q, na.rm=TRUE)) %>%
  ## aggregate data
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
  data.frame()

f.3 <- tbl_df(failure_3) %>%
  ## filter data at first
  group_by(cycle) %>%
  ## every split lasts 1/16 of second
  mutate(split = 1:n() %/% 128) %>%
  group_by(cycle, split) %>%
  mutate(s1.sd = sd(sensor_1)) %>%
  group_by(cycle) %>%
  filter(s1.sd >= quantile(s1.sd, q, na.rm=TRUE)) %>%
  ## aggregate data
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
  data.frame()

n.1 <- tbl_df(normal_1) %>%
  ## filter data at first
  group_by(cycle) %>%
  ## every split lasts 1/16 of second
  mutate(split = 1:n() %/% 128) %>%
  group_by(cycle, split) %>%
  mutate(s1.sd = sd(sensor_1)) %>%
  group_by(cycle) %>%
  filter(s1.sd >= quantile(s1.sd, q, na.rm=TRUE)) %>%
  ## aggregate data
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
  data.frame()

n.2 <- tbl_df(normal_2) %>%
  ## filter data at first
  group_by(cycle) %>%
  ## every split lasts 1/16 of second
  mutate(split = 1:n() %/% 128) %>%
  group_by(cycle, split) %>%
  mutate(s1.sd = sd(sensor_1)) %>%
  group_by(cycle) %>%
  filter(s1.sd >= quantile(s1.sd, q, na.rm=TRUE)) %>%
  ## aggregate data
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
  data.frame()

n.3 <- tbl_df(normal_3) %>%
  ## filter data at first
  group_by(cycle) %>%
  ## every split lasts 1/16 of second
  mutate(split = 1:n() %/% 128) %>%
  group_by(cycle, split) %>%
  mutate(s1.sd = sd(sensor_1)) %>%
  group_by(cycle) %>%
  filter(s1.sd >= quantile(s1.sd, q, na.rm=TRUE)) %>%
  ## aggregate data
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
  data.frame()

rm(failure_1); rm(failure_2); rm(failure_3)
rm(normal_1); rm(normal_2); rm(normal_3)

## Combine all data together
faults <- rbind(f.1, f.2, f.3, n.1, n.2, n.3)

## Calulate weights of classes
weights <- table(faults$fault)