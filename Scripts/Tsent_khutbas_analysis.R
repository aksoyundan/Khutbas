library(tidyverse)
library(lubridate)
library(lme4)
library(texreg)
library(arm)

k <- readRDS("data/khutbas.RDS")
s <- readRDS("data/sent/tws1520_ws.RDS")

s <- s %>% 
  pivot_longer(-c(week,sall), names_to = "topic", values_to = "sent") %>% 
  mutate(week = ymd(week),
         group = str_sub(topic, 2)
         ) %>% 
  group_by(group) %>%
  mutate(lag.sent = lag(sent, n = 1, default = NA)) %>% 
  mutate(lag.sall = lag(sall, n = 1, default = NA)) %>% 
  ungroup
  
  
m <- k %>% 
  left_join(s, by = c("week", "group"))

m %>% 
  filter(group!="umma") %>% 
  ggplot(aes(beta, sent, color = group)) + 
  geom_point() +
  scale_x_continuous(trans='log10') + 
  geom_smooth(method = "lm", span = .1) +
  facet_wrap(~ group, ncol = 3) + theme_classic() +
  theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold")) + 
  xlab("Sermon topic probability (beta)") + 
  ylab("Average senitiment of tweets on topic") + ggtitle("X: Sentiments in tweets versus Friday sermons")

m <- m %>% 
  mutate(bbus = beta*ifelse(group == "business", 1, 0),
         bfam = beta*ifelse(group == "family", 1, 0),
         bhea = beta*ifelse(group == "health", 1, 0),
         bnat = beta*ifelse(group == "nationalism", 1, 0),
         bpat = beta*ifelse(group == "patience", 1, 0),
         btru = beta*ifelse(group == "trust", 1, 0),
         bumm = beta*ifelse(group == "umma", 1, 0),)

summary(sent <- lm(sent~bbus+bfam+bhea+bnat+bpat+btru+bumm + lag.sent +
                      as.factor(group) + as.factor(week) , data=m))

m2 <- m %>% 
  filter(group == "trust") 

summary(fit2 <- lm(sall ~ beta + lag.sall + as.factor(week), data = m2))

#####################################################
#####################################################
###################hourly, only fridays##############

fs <- readRDS("data/sent/tws1520_sfr.RDS")

fs$week <- ymd(fs$week)
fs$after  <- ifelse(fs$hour > 11, 1, 0)
fs$group <- str_sub(fs$group, 2)

fs <- fs %>% 
  filter(group != "all") %>% 
  group_by(week, group, after) %>% 
  summarise(asent = mean(average, na.rm = TRUE)) %>% 
  mutate(lag.ssent = lag(asent, n = 1, default = NA)) %>% 
  ungroup()

fs <- fs %>% 
  left_join(k, by = c("week", "group"))

fs <- fs %>% 
  mutate(bbus = beta*ifelse(group == "business", 1, 0),
         bfam = beta*ifelse(group == "family", 1, 0),
         bhea = beta*ifelse(group == "health", 1, 0),
         bnat = beta*ifelse(group == "nationalism", 1, 0),
         bpat = beta*ifelse(group == "patience", 1, 0),
         btru = beta*ifelse(group == "trust", 1, 0),
         bumm = beta*ifelse(group == "umma", 1, 0),)

summary(fsent <- lm(asent~bbus+bfam+bhea+bnat+bpat+btru+bumm + lag.ssent +
                     as.factor(group) + as.factor(week) , data=fs))

coef <- as_tibble(clubSandwich::coef_test(fsent, vcov = "CR1", 
                                          cluster = fs$week, test = "naive-t")[2:8,])
