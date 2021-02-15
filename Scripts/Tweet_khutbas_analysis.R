library(tidyverse)
library(lubridate)

k <- readRDS("data/khutbas.RDS")
t <- readRDS("data/tws1520_week.RDS")

t <- t %>% 
  pivot_longer(-week, names_to = "group", values_to = "count")
 
t$week <- ymd(t$week)

m <- k %>% 
  left_join(t, by = c("week", "group"))

m <- m %>% 
group_by(week) %>% 
  mutate(all = sum(count)) %>%  
  mutate(ratio = count/all) %>% 
  ungroup() %>% 
  select(-text)

m <- m %>% 
  group_by(group) %>%
  arrange(week) %>% 
  mutate(lag.count = lag(count, n = 1, default = NA)) %>% 
  mutate(lag.ratio = lag(ratio, n = 1, default = NA)) %>% 
  ungroup()
  
m %>% 
  filter(group!="umma") %>% 
  ggplot(aes(beta, ratio, color = group)) + 
  geom_point() +
  geom_smooth(method = "lm", span = .1) +
  facet_wrap(~ group, ncol = 3) + 
  theme(legend.position = "none") 

summary(lm(ratio~beta + lag.ratio + as.factor(group)+ week, data = m))
summary(lm(log(count)~beta + log(lag.count) + as.factor(group) + week, data = m))

m$yweek <-  week(m$date)

summary(lm(ratio~beta + lag.ratio + as.factor(group)+ week + as.factor(yweek), data = m))
