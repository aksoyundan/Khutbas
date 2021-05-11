library(tidyverse)
library(lubridate)
library(lme4)
library(texreg)
library(arm)

k <- readRDS("data/khutbas.RDS")

k <- k %>% 
  filter(group!="umma") 

events <- tibble(date  = c(dmy(30082015) + years(0:5), 
                           dmy(29102015) + years(0:5),  
                           dmy(18032015) + years(0:5), 
                           dmy(15072016), dmy(20012018)),
                 event = c(rep('Victory day', 6), 
                           rep('Republic day', 6), 
                           rep('Dardanelles vic', 6),
                           'Coup attempt', 'Olive branch')
)

k %>% 
  group_by(group) %>% 
  summarise(mean = 100*mean(beta))

k %>% 
  filter(group == "nationalism") %>% 
ggplot(aes(date, beta)) + geom_line() +
  xlab("Date") + 
  ylab("Nationalism in Friday sermons") +  theme_classic() +
  theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("A: Friday sermon topics by week") +
  geom_vline(data = events, aes(xintercept = date), linetype = "dashed", colour = "gray") +
  geom_text(data = events, mapping = aes(label = event, y = .20), angle = 60, hjust = 0)


   
                              
ggplot(corpus.tr[corpus.tr$date > ymd("2015-01-01"),], aes_string(x="date",y="V27")) +
  geom_line() +
  ylab("Nationalism in Friday sermons") +
  xlab("Time") +
