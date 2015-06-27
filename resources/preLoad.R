library(dplyr)

load("./data/failure_1.RData")
load("./data/normal_3.RData")

pre.f.1 <- tbl_df(failure_1) %>%
  group_by(cycle) %>%
  ## every split lasts 1/16 of second
  mutate(split = 1:n() %/% 128) %>%
  group_by(cycle, split) %>%
  summarise(s1.mean = mean(sensor_1)
            , s1.sd = sd(sensor_1)) %>%
  ## take only one cycle and complete cases
  filter(cycle == "302A" & complete.cases(s1.sd)) %>%
  data.frame()

pre.n.3 <- tbl_df(normal_3) %>%
  group_by(cycle) %>%
  ## every split lasts 1/16 of second
  mutate(split = 1:n() %/% 128) %>%
  group_by(cycle, split) %>%
  summarise(s1.mean = mean(sensor_1)
            , s1.sd = sd(sensor_1)) %>%
  ## take only one cycle and complete cases
  filter(cycle == "302A" & complete.cases(s1.sd)) %>%
  data.frame()

rm(failure_1); rm(normal_3)