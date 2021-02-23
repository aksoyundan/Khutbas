library(tidyverse)
library(lubridate)
library(lme4)
library(texreg)
library(arm)

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
  ungroup() 

#%>% 
#  select(-text)

m <- m %>% 
  group_by(group) %>%
  arrange(week) %>% 
  mutate(lag.count = lag(count, n = 1, default = NA)) %>% 
  mutate(lag.ratio = lag(ratio, n = 1, default = NA)) %>% 
  ungroup()
  
m %>% 
  filter(group!="umma") %>% 
  ggplot(aes(beta, log(count), color = group)) + 
  geom_point() +
  geom_smooth(method = "lm", span = .1) +
  facet_wrap(~ group, ncol = 3) + 
  theme(legend.position = "none") 

summary(lm(ratio~beta + lag.ratio + as.factor(group)+ as.factor(week), data = m[m$group!="umma",]))
summary(lm(count~beta + lag.count + as.factor(group) + as.factor(week), data = m[m$group!="umma",]))

m$yweek <-  week(m$date)

summary(lm(ratio~beta + lag.ratio + as.factor(group)+ week + as.factor(yweek), data = m))


mo <- lm(ratio~beta + lag.ratio + as.factor(group)+ week, data = m)
ml <- lmer(ratio~beta + lag.ratio + as.factor(group) 
           + (1 | week), data = m,  REML = FALSE)

screenreg(list(mo, ml))

ml2 <- lmer(ratio~beta + lag.ratio + (1 + beta| group), data = m,  REML = TRUE)

cml12 <- cbind(fixef(ml2)[2] + ranef(ml2)$group[,2], se.ranef(ml2)$group[,2] )

plot(NA, xlim = c(0, .4), ylim = c(1, 7), xlab = "Slope", ylab = "", yaxt = "n")
title("Regression Results")
axis(2, 1:7, rownames(ranef(ml2)$group), las = 2)
abline(v = 0, col = "gray")
points(cml12[,1], 1:7, pch = 23, col = "black", bg = "black")
segments((cml12[,1] - (qnorm(0.975) * cml12[,2])), 1:7, (cml12[,1] + (qnorm(0.975) * cml12[,2])), 
         1:7, col = "black", lwd = 2)

#####################################################
#####################################################
###################hourly, only fridays##############
f <- readRDS("data/tws1520_fr.RDS")

f$week <- ymd(f$week)

fk <- f %>% 
  left_join(k, by = c("week", "group"))

fk$after  <- ifelse(fk$hour > 11, 1, 0)
fk$bafter <- fk$beta*fk$after
fk$ahour <- fk$hour*fk$after

fk <- fk %>% 
  group_by(week) %>% 
  mutate(all = sum(count)) %>%  
  mutate(ratio = 100*count/all) %>% 
  ungroup() 

summary(lmer(count~beta + after + bafter +  hour + as.factor(group) + (1 | week), data = fk))
summary(lmer(ratio~beta + after + bafter + hour + as.factor(group) + (1 | week), data = fk))

m <- lm(count~beta +  bafter + as.factor(hour) 
        + as.factor(group) + as.factor(week), data = fk)

clubSandwich::coef_test(m, vcov = "CR1", 
          cluster = fk$week, test = "naive-t")[2:10,]

clubSandwich::coef_test(m, vcov = "CR2", 
                        cluster = fk$week, test = "Satterthwaite")[2:10,]

m2 <- lmer(ratio~beta + after + bafter + (1|group), data = fk)


fk_sum <- fk %>% 
  group_by(week, group, after) %>% 
  summarise(mratio = sum(ratio),
            mcount = sum(count)) %>% 
  mutate(lag.mratio = lag(mratio, n = 1, default = NA)) %>% 
  mutate(lag.mcount = lag(mcount, n = 1, default = NA)) %>% 
  mutate(diffmcount = mcount - lag.mcount) %>% 
  mutate(diffmratio = mratio - lag.mratio) %>% 
ungroup()

fk_sum <- fk_sum %>% 
  left_join(k, by = c("week", "group"))

fk_sum %>% 
  ggplot(aes(beta, diffmcount, color = group)) + 
  geom_point() +
  geom_smooth(method = "lm", span = .1) +
  facet_wrap(~ group, ncol = 3) + 
  theme(legend.position = "none") 

fk_sum %>% 
  filter(group!="umma") %>% 
  ggplot(aes(beta, diffmratio)) + 
  geom_smooth(method = "lm", span = .1) +
  geom_point(aes(beta, diffmratio, color = group)) 

fk_sum$bafter <- fk_sum$beta*fk_sum$after

summary(m2 <- lm(mcount~beta + bafter + after+ as.factor(group) + as.factor(week) , data = fk_sum))

summary(m3 <- lm(diffmcount~beta + as.factor(group) + as.factor(week) , data = fk_sum))

summary(m4 <- lm(mcount~beta +lag.mcount + as.factor(group) + as.factor(week) , data = fk_sum))


clubSandwich::coef_test(m3, vcov = "CR1", 
                        cluster = fk_sum$week, test = "naive-t")[2:4,]

#####################################################
#####################################################
################### fridays & Thursdays ##############

k  <- readRDS("data/khutbas.RDS")
tf <- readRDS("data/tws1520_thfr.RDS")

tf$week <- ymd(tf$week) + days(4) #to be able to match with k in which weeks starts by fri


mtf <- tf %>% 
  left_join(k, by = c("week", "group"))

mtf$after <- ifelse(mtf$day=="Fri", 1, 0)

mtf$bafter <- mtf$beta*mtf$after


mtf<- mtf %>% 
  group_by(week, group) %>% 
  mutate(lag.mcount = lag(count, n = 1, default = NA)) %>% 
  ungroup()

mtf$cdiff = mtf$count - mtf$lag.mcount

summary(m3 <- lm(count~beta + lag.mcount + as.factor(group) +as.factor(week) , data = mtf[mtf$day=="Fri",]))

