library(rtweet)
library(tidyverse)

apn <- "Friday-khutbas"
key <- "QPyPimCLYUBe4bT2PuQnw2era"
sec <- "3zfDThCdF9BGu3Q5vt6BZYjCIeShZy838I6SAvZ62LgLSdWHad"
tok <- "AAAAAAAAAAAAAAAAAAAAALpLMQEAAAAAVK6txwxShISW%2BJkVPxyjWqvt%2Fi0%3DYwgrpWgvZHPiKFZm3CBNS4RPn0Bsgsef1kWKWGSoZ1MdxZDFkz"

# google API: AIzaSyBEzHbvHassrigxQJku5prG3TzdYEUbBiM

twitter_token <- create_token(
  app = apn,
  consumer_key = key,
  consumer_secret = sec)

path_to_token <- file.path(path.expand("~"), ".twitter_token.rds")
saveRDS(twitter_token, path_to_token)

env_var <- paste0("TWITTER_PAT=", path_to_token)
cat(env_var, file = file.path(path.expand("~"), ".Renviron"), 
    fill = TRUE, append = TRUE)
readRenviron("~/.Renviron")



library(lubridate)
library(ggplot2)

tryTolower <- function(x) {
  y = NA
  try_error = tryCatch(tolower(x), error= function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

custom.stopwords <- c(quanteda::stopwords("tr", source = "stopwords-iso"), 'ili', 'genel', 'tarih', '(s.a.s.)', '(a.s.)', 
                      'din', 'hizmetleri', 'services', '(s.a.s)', '(a.s.)', 'ili', 
                      'genel', 'müdürlüğü', 'amp', '&', 'hazırlayan', 'redaksiyon', 'hazırlayan',
                      'İlİ', 'ankara', 'diyanet', 'müftülüğü', 'kardeşlerim')


kwords <- c("temiz", "sağlık", "salgın", "dikkat", 
            "aile", "anne", "çocuk", "huzur", 
            "millet", "şehit", "vatan", "fitne",
            "tevhid", "ümmet", "zulüm", "müslüman",
            "haram", "helal", "zarar", "mal",
            "sabır", "yardım", "imtihan", "gayret",
            "güven", "emin", "mübarek", "ahlak")

kwords <- paste(kwords, collapse = " OR ")

tws <- search_tweets(kwords, lang = "tr", n = 18000, retryonratelimit = TRUE,
                     include_rts = FALSE, geocode = lookup_coords("Turkey"))

tws.corp <- tws %>% 
  select(status_id, text)

names(tws.corp ) <- c("doc_id", "text")

corp.tw <- VCorpus(DataframeSource(tws.corp ))
corp.tw <- clean.corpus(corp.tw)

corp.tw_td <- tidy(corp.tw) %>% 
  select(id, text)

tws_iddate <- tws %>% 
  select(created_at, status_id)
names(tws_iddate) <- c("date", "id")

corp.tw_td <- corp.tw_td %>% 
  inner_join(tws_iddate, by="id")

corp.tw_td <- corp.tw_td %>% 
  unnest_tokens(word, text)

corp.tw_td <- corp.tw_td %>% 
  mutate(day = floor_date(date, "hour"))
  

saveRDS(tws, "tws.rds")
  
  

####
tw_m  %>% 
  group_by(days) %>%
  summarise(sentiment = sum(sentiment, na.rm = TRUE)) %>%
  ggplot(aes(x = days, y = sentiment)) +
  geom_point(aes(colour = sentiment > 0)) + 
  geom_smooth(method = "loess", span = .2) + 
  scale_color_manual(values = c("#dd3333", "#22aa33")) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "#000000cc") + 
  theme_minimal(base_family = "Helvetica Neue") +
  ylab("Aliosun tweetlerinin sentimentleri") +
  xlab("Tarih")
