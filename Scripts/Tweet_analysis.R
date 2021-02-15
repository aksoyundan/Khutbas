Sys.setlocale(category = "LC_ALL", locale = "Turkish")
options(stringsAsFactors=F)

library(rtweet)
library(tidyverse)
library(lubridate)
library(ggplot2)
#library(stringr)

tws1520 <- readRDS("data/tws1520_c.RDS")

tws1520$date <- ymd_hms(tws1520$created_at)
tws1520 <- tws1520 %>% 
  mutate(week = floor_date(date, "week", week_start = getOption("lubridate.week.start", 5))) 

tws1520_week <- tws1520 %>%
  group_by(week) %>% 
  filter(week > "2015-01-01") %>% 
  summarise(business    = sum(com), 
            family     = sum(fam),
            health      = sum(hlt),
            nationalism     = sum(nat),
            patience    = sum(pat),
            trust       = sum(tru),
            umma        = sum(uma)) %>% 
  ungroup()

saveRDS(tws1520_week, "data/tws1520_week.RDS")

tws1520 %>%
  group_by(week) %>% 
  filter(week > "2015-01-01") %>% 
  summarise(business    = sum(com), 
            family     = sum(fam),
            health      = sum(hlt),
            natlsm     = sum(nat),
            patien    = sum(pat),
            trust       = sum(tru),
            umma        = sum(uma)) %>%
  select(-umma, -business) %>% 
  pivot_longer(-week, names_to = "group", values_to = "count") %>% 
  ggplot(aes(week, count, color = group)) + geom_smooth(method = "loess", span = .1) +
  geom_line() +scale_y_continuous(trans='log10') + theme(legend.position = "bottom") +
  scale_color_discrete(name = "Topic:", labels = c("family", "health", "nation", "patience", "trust"))

tws1520 %>%
  group_by(week) %>% 
  filter(week > "2015-01-01") %>% 
  summarise(business    = sum(com), 
            family     = sum(fam),
            health      = sum(hlt),
            nationalism     = sum(nat),
            patience    = sum(pat),
            trust       = sum(tru),
            umma        = sum(uma)) %>%
  select(-umma) %>% 
  pivot_longer(-week, names_to = "group", values_to = "count") %>% 
  ungroup() %>% 
  ggplot(aes(week, count, color = group)) + geom_smooth(method = "loess", span = .1) +
  geom_line() +scale_y_continuous(trans='log10') + 
  facet_wrap(~ group, ncol = 3) + 
  theme(legend.position = "none") 


tws1520 %>%
  group_by(week) %>% 
  filter(week > "2015-01-01") %>% 
  summarise(business    = sum(com), 
            family     = sum(fam),
            health      = sum(hlt),
            nationalism     = sum(nat),
            patience    = sum(pat),
            trust       = sum(tru),
            umma        = sum(uma)) %>% 
  pivot_longer(-week, names_to = "group", values_to = "count") %>% 
  group_by(week) %>% 
  mutate(all = sum(count)) %>%  
  mutate(ratio = count/all) %>% 
  filter(group != "umma") %>% 
  ungroup() %>% 
  ggplot(aes(week, ratio, color = group)) + geom_smooth(method = "loess", span = .1) +
  geom_line() +
  facet_wrap(~ group, ncol = 3) + 
  theme(legend.position = "none") 



################################################
##code used to clean tws1520

# 
# tws1520 <- readRDS("data/tws1520.RDS")
# 
# to.plain <- function(s) {
#   # 1 character substitutions
#   old1 <- "çğşıüöÇĞŞİÖÜîâÂ"
#   new1 <- "cgsiuocgsiouiaA"
#   s1 <- chartr(old1, new1, s)
#   # 2 character substitutions
#   old2 <- c("œ", "ß", "æ", "ø")
#   new2 <- c("oe", "ss", "ae", "oe")
#   s2 <- s1
#   for(i in seq_along(old2)) s2 <- gsub(old2[i], new2[i], s2, fixed = TRUE)
#   s2
# }
# 
# tws1520$text=as.vector(sapply(tws1520$text,to.plain))
# 
# hlt <- c("temiz|sağlık|saglik|saglık|salgın|salgin|dikkat|dıkkat")
# fam <- c("aile|anne|çocuk|cocuk|çoçuk|huzur")
# nat <- c("millet|şehit|sehit|vatan|fitne")
# uma <- c("tevhid|ümmet|ummet|zulüm|zulum|müslüman|musluman")
# com <- c("haram|helal|zarar|mal")
# pat <- c("sabır|sabir|yardım|yardim|imtihan|gayret")
# tru <- c("güven|guven|emin|emın|mübarek|mubarek|ahlak")
# 
# tws1520 <- tws1520 %>% 
#   group_by(id) %>% 
#   mutate (com = length(grep(com, text, ignore.case = TRUE))) %>% 
#   mutate (fam = length(grep(fam, text, ignore.case = TRUE))) %>% 
#   mutate (hlt = length(grep(hlt, text, ignore.case = TRUE))) %>% 
#   mutate (nat = length(grep(nat, text, ignore.case = TRUE))) %>% 
#   mutate (pat = length(grep(pat, text, ignore.case = TRUE))) %>% 
#   mutate (tru = length(grep(tru, text, ignore.case = TRUE))) %>% 
#   mutate (uma = length(grep(uma, text, ignore.case = TRUE))) %>% 
#   ungroup()
# 
# tws1520_miss <- tws1520 %>% 
#   filter(rowSums(tws1520[,c("hlt", "fam", "nat", "uma", "com", "pat", "tru")]) == 0)
# 
# saveRDS(tws1520, "data/tws1520_c.RDS")

################################################## junk code below
#j1 <- max.col(tws1520[, c("hlt", "fam", "nat", "uma", "com", "pat", "tru")], "first")
#j2 <- max.col(tws1520[, c("hlt", "fam", "nat", "uma", "com", "pat", "tru")], "last")

#too slow:
#tws1520$hlt <- lengths(regmatches(tws1520$text, gregexpr(hlt, tws1520$text, ignore.case = TRUE)))

# nchar(tws1520$text[1]) -nchar( gsub(hlt, "", tws1520$text[1]))
# 
# ts_plot(tws1520) +
#   ggplot2::labs(
#     title = "Tweets between 2015 and 2021/Feb",
#     subtitle = "On six topics"
#   )

