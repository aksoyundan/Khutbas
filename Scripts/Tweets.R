library(rtweet)
library(tidyverse)
library(lubridate)

source("api-keys.txt")
source("Scripts/get_tweets.R")

twitter_token <- create_token(
  app = apn2,
  consumer_key = key2,
  consumer_secret = sec2)

path_to_token <- file.path(path.expand("~"), ".twitter_token.rds")
saveRDS(twitter_token, path_to_token)
env_var <- paste0("TWITTER_PAT=", path_to_token)
cat(env_var, file = file.path(path.expand("~"), ".Renviron"), 
    fill = TRUE, append = TRUE)
readRenviron("~/.Renviron")


kwords <- c("temiz", "sağlık", "salgın", "dikkat", 
            "aile", "anne", "çocuk", "huzur", 
            "millet", "şehit", "vatan", "fitne",
            "tevhid", "ümmet", "zulüm", "müslüman",
            "haram", "helal", "zarar", "mal",
            "sabır", "yardım", "imtihan", "gayret",
            "güven", "emin", "mübarek", "ahlak")

kwords <- paste(kwords, collapse = " OR ")
kwords <- paste("(", kwords, ")", sep="")
kwords <- paste(kwords, "-is:retweet lang:tr place_country:TR")

kw.nat <- c("millet", "şehit", "vatan", "fitne")
kw.nat <- paste(kw.nat, collapse = " OR ")
kw.nat <- paste("(", kw.nat, ")", sep="")
kw.nat <- paste(kw.nat, " -is:retweet lang:tr place_country:TR -vekil")

# "2021-02-08T00:00:00Z"



#df <- get_tweets(kw.nat, 500, 
#                 "2015-01-01T00:00:00Z",
#                 "2016-01-08T00:00:00Z", tok2, next_token)

#df.tidy <- as_tibble(df$data)
#df.tidy2 <- as_tibble(df$includes$users)

#paginate tweets and download

start_date0 <- "2015-01-01T00:00:01Z" #start date of downloading tweets
int0 <- ((ymd("2015-01-01")%--%ymd(Sys.Date())) /years(1))/300 #time between start date and today in years
end_date0 <- sub(" ","T", paste0(lubridate::ymd_hms(start_date0) + lubridate::dyears(int0), "Z"))
next_token <-""
k <- 0

while(k<15*60){
  int <- int0*(k/3)
  start_date <- sub(" ","T", paste0(lubridate::ymd_hms(start_date0) + lubridate::dyears(int), "Z"))
  end_date   <- sub(" ","T", paste0(lubridate::ymd_hms(end_date0) + lubridate::dyears(int), "Z"))
  
  df <- get_tweets(kwords, 500, 
                   start_date,
                   end_date, tok2, next_token)
  
  jsonlite::write_json(df$data,paste0("data/","data_",df$data$id[nrow(df$data)],".json"))
  jsonlite::write_json(df$includes,paste0("data/","includes_",df$data$id[nrow(df$data)],".json"))
  next_token <- df$meta$next_token #this is NULL if there are no pages left
  Sys.sleep(3.1)
  k <- k+3
  cat(k,": ","(",nrow(df$data),") ",df$data$created_at[nrow(df$data)],"\n",sep = "")
}



### merge downloaded tweets

files <- list.files(path = "data/", pattern = "^data_")
data_file <- file.path("data", files)


tws <- data.frame("created_at" = c(), "author_id" = c(),"id" = c(), "text" = c())

for (i in 1:length(data_file)) {
  data.frame("created_at" = as_tibble(jsonlite::fromJSON(data_file[i]))$created_at , 
             "author_id" = as_tibble(jsonlite::fromJSON(data_file[i]))$author_id , 
             "id" = as_tibble(jsonlite::fromJSON(data_file[i]))$id , 
             "text" = as_tibble(jsonlite::fromJSON(data_file[i])$text) ) -> doc
  colnames(doc) <- c("created_at", "author_id", "id", "text")
  tws  <- rbind(tws,doc) 
}

saveRDS(tws, "data/tws.RDA")
################

# "T00:00:00Z"

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

kw.nat <- c("millet", "şehit", "vatan", "fitne")
kw.nat <- paste(kw.nat, collapse = " OR ")


next_token <-""
k <- 0
while(k<15*60){
  df <- get_tweets(q,n,start_time,end_time,bearer,next_token)
  jsonlite::write_json(df$data,paste0("data/","data_",df$data$id[nrow(df$data)],".json"))
  jsonlite::write_json(df$includes,paste0("data/","includes_",df$data$id[nrow(df$data)],".json"))
  next_token <- df$meta$next_token #this is NULL if there are no pages left
  Sys.sleep(3.1)
  k <- k+3
  cat(k,": ","(",nrow(df$data),") ",df$data$created_at[nrow(df$data)],"\n",sep = "")
}


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
