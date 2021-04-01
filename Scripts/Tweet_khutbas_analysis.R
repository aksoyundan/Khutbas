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
  
C <- m %>% 
  filter(group!="umma") %>% 
  ggplot(aes(beta, count, color = group)) + 
  geom_point() +
  scale_y_continuous(trans='log10') + 
  geom_smooth(method = "lm", span = .1) +
  facet_wrap(~ group, ncol = 3) + theme_classic() +
  theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold")) + 
  xlab("Sermon topic probability (beta)") + 
  ylab("# tweets on topic next week") + ggtitle("C: Tweets versus Friday sermons")

A <- k %>% 
  filter(group!="umma") %>% 
  ggplot(aes(week, beta, color = group)) + geom_smooth(method = "loess", span = .1) +
  scale_y_continuous(trans='log10') + 
  facet_wrap(~ group, ncol = 3) +   
  xlab("Date") + 
  ylab("Topic probability (beta)") +  theme_classic() +
  theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("A: Friday sermon topics by week")

ggpubr::ggarrange(A, B, C, D, ncol = 2, nrow = 2)

m <- m %>% 
  mutate(bbus = beta*ifelse(group == "business", 1, 0),
         bfam = beta*ifelse(group == "family", 1, 0),
         bhea = beta*ifelse(group == "health", 1, 0),
         bnat = beta*ifelse(group == "nationalism", 1, 0),
         bpat = beta*ifelse(group == "patience", 1, 0),
         btru = beta*ifelse(group == "trust", 1, 0),
         bumm = beta*ifelse(group == "umma", 1, 0),)

summary(pois <- glm(count~bbus+bfam+bhea+bnat+bpat+btru+bumm + 
                       as.factor(group) + lag.count +as.factor(week) , 
                     family=poisson(link = "log"), data=m))

summary(pois2 <- glm(count~bbus+bfam+bhea+bnat+bpat+btru+bumm + 
                      as.factor(group) + as.factor(week) , offset(log(lag.count)), 
                    family=poisson(link = "log"), data=m))

summary(pois3 <- glm(count~bbus+bfam+bhea+bnat+bpat+btru+bumm + lag.count +
                       as.factor(group) + as.factor(week), 
                     family=quasipoisson, data=m))

coefw <- as_tibble(clubSandwich::coef_test(pois, vcov = "CR1", 
                                          cluster = m$week, test = "naive-t")[2:7,])
coefw$Model <- "Week after Friday vs before"
coefw$names <- names <- c("business", "family", "health", "nationalism", "patience", "trust")

#####################################################
#####################################################
###################hourly, only fridays##############
f <- readRDS("data/tws1520_fr.RDS")

f$week <- ymd(f$week)

f$after  <- ifelse(f$hour > 11, 1, 0)

fk_sum <- f %>% 
  group_by(week, group, after) %>% 
  summarise(mcount = sum(count)) %>% 
  mutate(lag.mcount = lag(mcount, n = 1, default = NA)) %>% 
  mutate(diffmcount = mcount - lag.mcount) %>% 
  ungroup()

fk_sum <- fk_sum %>% 
  left_join(k, by = c("week", "group"))

fk_sum <- fk_sum %>% 
  mutate(bbus = beta*ifelse(group == "business", 1, 0),
         bfam = beta*ifelse(group == "family", 1, 0),
         bhea = beta*ifelse(group == "health", 1, 0),
         bnat = beta*ifelse(group == "nationalism", 1, 0),
         bpat = beta*ifelse(group == "patience", 1, 0),
         btru = beta*ifelse(group == "trust", 1, 0),
         bumm = beta*ifelse(group == "umma", 1, 0),)

summary(mpois <- glm(mcount~bbus+bfam+bhea+bnat+bpat+btru+bumm + 
                       as.factor(group) + lag.mcount +as.factor(week) , 
                     family=quasipoisson(link = "log"), data=fk_sum))

coef <- as_tibble(clubSandwich::coef_test(mpois, vcov = "CR1", 
                        cluster = fk_sum$week, test = "naive-t")[2:7,])

coef$Model <- "Friday pm vs am"
coef$names <- names <- c("business", "family", "health", "nationalism", "patience", "trust")

coefs <- rbind(coef, coefw)

D <- ggplot(coefs, aes(reorder(names, -beta), beta, color = Model ))+
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  geom_linerange(aes(ymin = beta - SE, ymax = beta + SE), 
                  position = position_dodge(width = 1/2), lwd = 1)+
  geom_pointrange(aes(ymin = beta - SE*1.96, ymax = beta + SE*1.96), 
                  position = position_dodge(width = 1/2), lwd = 1/2)+  theme_classic() +
  labs(title = "D: Coefficients of quasipoisson regressions \n predicting tweet counts by sermon topic") +
  coord_flip() + theme(plot.title = element_text(size = 10, face = "bold")) + 
  xlab("Coef for beta (+95% CI & SE) ") + 
  ylab("")



mpois2 <- glm(mcount~bbus+bfam+bhea+bnat+bpat+btru+bumm + lag.mcount +
                as.factor(group) + as.factor(week) , 
              family=quasipoisson, data=fk_sum)

#vCmpois <- sandwich::vcovCL(mpois, cluster = ~ week)
vCmpois <- sandwich::vcovHC(mpois, type="HC0")

std.err <- sqrt(diag(vCmpois))[1:15]

r.est <- cbind(Estimate= coef(mpois)[1:15], "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(mpois)[1:15]/std.err), lower.tail=FALSE),
               LL = coef(mpois)[1:15] - 1.96 * std.err,
               UL = coef(mpois)[1:15] + 1.96 * std.err)

r.est


summary(mqpois <- glm(mcount~bbus+bfam+bhea+bnat+bpat+btru+bumm + 
                       as.factor(group) + lag.mcount + as.factor(week), family=quasipoisson(link = "log"), data=fk_sum))

clubSandwich::coef_test(mpois, vcov = "CR1", 
                        cluster = fk_sum$week, test = "naive-t")[1:15,]

summary(mpois <- glm(mcount~bbus+bfam+bhea+bnat+bpat+btru+bumm + 
                       as.factor(group) + lag.mcount, family=poisson(link = "log"), data=fk_sum))

########################################################
########################################################
###############Placebo: Fridays the week before########
k <- readRDS("data/khutbas.RDS")
t <- readRDS("data/tws1520_fr.RDS")

t$week <- ymd(t$week) + days(7)  #to be able to match with k in which weeks starts by fri


t$after  <- ifelse(t$hour > 11, 1, 0)

tk_sum <- t %>% 
  group_by(week, group, after) %>% 
  summarise(mcount = sum(count)) %>% 
  mutate(lag.mcount = lag(mcount, n = 1, default = NA)) %>% 
  mutate(diffmcount = mcount - lag.mcount) %>% 
  ungroup()

tk_sum <- tk_sum %>% 
  left_join(k, by = c("week", "group"))

tk_sum <- tk_sum %>% 
  mutate(bbus = beta*ifelse(group == "business", 1, 0),
         bfam = beta*ifelse(group == "family", 1, 0),
         bhea = beta*ifelse(group == "health", 1, 0),
         bnat = beta*ifelse(group == "nationalism", 1, 0),
         bpat = beta*ifelse(group == "patience", 1, 0),
         btru = beta*ifelse(group == "trust", 1, 0),
         bumm = beta*ifelse(group == "umma", 1, 0),)

mpoist <- glm(mcount~bbus+bfam+bhea+bnat+bpat+btru+bumm + 
                       as.factor(group) + lag.mcount + as.factor(week), 
                     family=quasipoisson, data=tk_sum)

coefp <- as_tibble(clubSandwich::coef_test(mpoist, vcov = "CR1", 
                        cluster = tk_sum$week, test = "naive-t")[2:7,])

coefp$Model <- "Placebo: week before Friday"
coefp$names <- names <- c("business", "family", "health", "nationalism", "patience", "trust")

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

mtf <- mtf %>% 
  mutate(bbus = beta*ifelse(group == "business", 1, 0),
         bfam = beta*ifelse(group == "family", 1, 0),
         bhea = beta*ifelse(group == "health", 1, 0),
         bnat = beta*ifelse(group == "nationalism", 1, 0),
         bpat = beta*ifelse(group == "patience", 1, 0),
         btru = beta*ifelse(group == "trust", 1, 0),
         bumm = beta*ifelse(group == "umma", 1, 0),)

summary(m3 <- glm(count~bbus+bfam+bhea+bnat+bpat+btru+bumm
                  + lag.mcount + as.factor(group) 
                  +as.factor(week), family=poisson(link = "log"),
                  data = mtf[mtf$day=="Fri",]))

clubSandwich::coef_test(m3, vcov = "CR1", 
                        cluster = mtf[mtf$day=="Fri",]$week, test = "naive-t")[1:15,]

#####################################################
#####################################################
###################days##############

k  <- readRDS("data/khutbas.RDS")
d <- readRDS("data/tws1520_day.RDS")

d$week <- ymd(d$week)

d$wday <- wday(d$day)

d <- d %>% 
  left_join(k, by = c("week", "group"))

d$sun = ifelse(d$wday==1, 1, 0)
d$mon = ifelse(d$wday==2, 1, 0)
d$tue = ifelse(d$wday==3, 1, 0)
d$wed = ifelse(d$wday==4, 1, 0)
d$thu = ifelse(d$wday==5, 1, 0)
d$fri = ifelse(d$wday==6, 1, 0)
d$sat = ifelse(d$wday==7, 1, 0)

d$bsun = d$sun*d$beta
d$bmon = d$mon*d$beta
d$btue = d$tue*d$beta
d$bwed = d$wed*d$beta
d$bthu = d$thu*d$beta
d$bfri = d$fri*d$beta
d$bsat = d$sat*d$beta

summary(m4 <- lm(count~beta + sat + sun + mon + tue + wed + thu +
                   + bsat + bsun + bmon + btue + bwed + bthu +
                   as.factor(group) + as.factor(week) , data = d))

clubSandwich::coef_test(m4, vcov = "CR1", 
                        cluster = d$week, test = "naive-t")[2:14,]