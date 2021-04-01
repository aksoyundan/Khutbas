Sys.setlocale(category = "LC_ALL", locale = "Turkish")
options(stringsAsFactors=F)

library(rtweet)
library(tidyverse)
library(lubridate)
library(ggplot2)
#library(stringr)

tws1520_s <- readRDS("data/sent/tws1520_s.RDS")
tws1520 <- readRDS("data/tws1520_c.RDS")

tws1520_t <- tws1520 %>% 
  select(com, fam, hlt, nat, pat, tru, uma, id)

tws1520_s <- tws1520_s %>% 
  inner_join(tws1520_t, by ="id")

tws1520_s <- tws1520_s %>% 
  mutate (scom = ifelse(com > 0, com*sentiment, NA)) %>% 
  mutate (sfam = ifelse(fam > 0, fam*sentiment, NA)) %>% 
  mutate (shlt = ifelse(hlt > 0, hlt*sentiment, NA)) %>% 
  mutate (snat = ifelse(nat > 0, nat*sentiment, NA)) %>% 
  mutate (spat = ifelse(pat > 0, pat*sentiment, NA)) %>% 
  mutate (stru = ifelse(tru > 0, tru*sentiment, NA)) %>% 
  mutate (suma = ifelse(uma > 0, uma*sentiment, NA))

saveRDS(tws1520_s, "data/sent/tws1520_s.RDS")

tws1520_s <- tws1520_s %>% 
  mutate(week = floor_date(date_tr, "week", 
                           week_start = getOption("lubridate.week.start", 5))) 


############ weekly data #################################

tws1520_ws <- tws1520_s %>% 
  group_by(week) %>% 
  filter(week > "2015-01-01") %>% 
  summarise(sbusiness    = mean(scom, na.rm = TRUE), 
            sfamily      = mean(sfam, na.rm = TRUE),
            shealth      = mean(shlt, na.rm = TRUE),
            snationalism = mean(snat, na.rm = TRUE),
            spatience    = mean(spat, na.rm = TRUE),
            strust       = mean(stru, na.rm = TRUE),
            summa        = mean(suma, na.rm = TRUE),
            sall         = mean(sentiment, na.rm = TRUE)) %>% 
  ungroup()

saveRDS(tws1520_ws, "data/sent/tws1520_ws.RDS")


########################## friday only #######################################
tws1520_sfr <- tws1520_s %>% 
  filter(wday(date_tr) == 6) %>% 
  mutate(hour = hour(date_tr)) 

tws1520_sfr <- tws1520_sfr %>% 
  group_by(week, hour) %>% 
  filter(week > "2015-01-01") %>% 
  summarise(sbusiness    = mean(scom, na.rm = TRUE), 
            sfamily      = mean(sfam, na.rm = TRUE),
            shealth      = mean(shlt, na.rm = TRUE),
            snationalism = mean(snat, na.rm = TRUE),
            spatience    = mean(spat, na.rm = TRUE),
            strust       = mean(stru, na.rm = TRUE),
            summa        = mean(suma, na.rm = TRUE),
            sall         = mean(sentiment, na.rm = TRUE)) %>% 
  ungroup()

tws1520_sfr <- tws1520_sfr %>% 
  pivot_longer(-c(week, hour), names_to = "group", values_to = "average")

saveRDS(tws1520_sfr, "data/sent/tws1520_sfr.RDS")

################# computing sentiments ##########################

#tws1520 <- readRDS("data/tws1520.RDS")
#tws1520$date <- ymd_hms(tws1520$created_at)
#tws1520$date_tr <- with_tz(tws1520$date, tzone ="Europe/Istanbul")#

#tws1520 <- tws1520 %>%
#  mutate(year = floor_date(date_tr, "year"))

#sent_scores <- function(x) syuzhet::get_sentiment(plain_tweets(x), method = "nrc",
#                                                  language ="turkish") - .5

#system.time({
#tws1520_s <- tws1520 %>%
#         mutate( sentiment = sent_scores(text) ) %>% 
#  select(sentiment, id, date_tr)
#saveRDS(tws1520_s, "data/sent/tws1520_s.RDS")
#})