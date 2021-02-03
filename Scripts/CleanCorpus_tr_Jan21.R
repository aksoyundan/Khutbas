options(stringsAsFactors=F)
Sys.setlocale("LC_ALL", "C")

library(pdftools)
library(tm)
library(magrittr)
library(wordcloud)
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyverse)
library(tidyr)
library(tidytext)

#Khutbas since 2015 (whole country)
corpus.tr.2015 <- data.frame("title" = c(), "date" = c(),"text" = c())

files.pdf2 <- list.files(path = "./2015-sonrasi-hutbeler", pattern = "pdf$")
pdf_file2 <- file.path("./2015-sonrasi-hutbeler", files.pdf2)

for (i in 1:length(pdf_file2)) {
  data.frame("title" = NA , 
             "date" = substring(files.pdf2[i], 1, 10), 
             "text" = pdf_text(pdf_file2[i]) ) -> doc
  colnames(doc) <- c("title", "date", "text")
  corpus.tr.2015  <- rbind(corpus.tr.2015,doc) 
}

corpus.tr.2015 <- aggregate(x=list(title=corpus.tr.2015$title , text=corpus.tr.2015$text), by=list(group=corpus.tr.2015$date), paste, collapse="\f")

names(corpus.tr.2015) <- c("date", "title", "text")
corpus.tr.2015 <- corpus.tr.2015[, c("title", "date", "text")]

corpus.tr.2015$date <- lubridate::ymd(corpus.tr.2015$date)

corpus.tr <- corpus.tr.2015

corpus.tr <- corpus.tr[!is.na(corpus.tr$text),]
corpus.tr <- corpus.tr[corpus.tr$text!="",]
corpus.tr <- corpus.tr[corpus.tr$text!=" ",]

corpus.tr$text <- qdap::clean(corpus.tr$text)

corpus.tr<-data.frame(doc_id=seq(1:nrow(corpus.tr)),corpus.tr$text, corpus.tr$date, corpus.tr$title)
 
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

clean.corpus <- function(corpus) {
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, custom.stopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  return(corpus)
}

names(corpus.tr) <- c("doc_id", "text", "date", "title")

#corpus.tr$text <- gsub("’", "'", corpus.tr$text)
#corpus.tr$text <- gsub("’’", "''", corpus.tr$text)
#corpus.tr$text <- gsub("“", "''", corpus.tr$text)

corp <- VCorpus(DataframeSource(corpus.tr))

corp <-clean.corpus(corp)

ndocs <- length(corpus.tr$doc_id)
minDocFreq <- ndocs * 0.05
maxDocFreq <- ndocs * 0.95

tdm <- TermDocumentMatrix(corp, control = list(weighting=weightTf, bounds = list(global = c(minDocFreq, maxDocFreq))))
dtm <- DocumentTermMatrix(corp, control = list(weighting=weightTf, 
                                                 bounds = list(global = c(minDocFreq, maxDocFreq))))


tdm.m <- as.matrix(tdm)
tdm.q <- quanteda::as.dfm(dtm)
#tdm.q <- quanteda::dfm_wordstem(tdm.q, language = "tr")

quanteda::textplot_wordcloud(tdm.q)

tdm.c <- quanteda::convert(tdm.q, to="stm")


tdm.t <- quanteda::convert(tdm.q, to="topicmodels")


lda_tm <- topicmodels::LDA(tdm.t, method = "Gibbs", k = 50, 
                           control = list(seed = 1234))


# terms(lda_tm,10)

doc_lda <- tidytext::tidy(lda_tm, matrix = "gamma")
wor_lda <- tidytext::tidy(lda_tm, matrix = "beta")

sorted_topics <- doc_lda %>% group_by(topic) %>% summarise(mean(gamma)) %>% arrange(desc(`mean(gamma)`) )

top_terms <- wor_lda  %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()


terms_health <- top_terms %>% 
   filter(topic == top_terms[top_terms$term=="temiz",]$topic) %>% 
  select(term, topic)

terms_nation <- top_terms %>% 
  filter(topic == top_terms[top_terms$term=="vatan",]$topic) %>% 
  select(term, topic)

terms_patience <- top_terms %>% 
  filter(topic == top_terms[top_terms$term=="imtihan",]$topic) %>% 
  select(term, topic)

terms_haram <- top_terms %>% 
  filter(topic == top_terms[top_terms$term=="haram",]$topic) %>% 
  select(term, topic)

terms_umma <- top_terms %>% 
  filter(topic == top_terms[top_terms$term=="tevhid",]$topic) %>% 
  select(term, topic)

terms_trust <- top_terms %>% 
  filter(topic == top_terms[top_terms$term=="emin",]$topic) %>% 
  select(term, topic)

terms_family <- top_terms %>% 
  filter(topic == top_terms[top_terms$term=="aile",]$topic) %>% 
  select(term, topic)

terms_kids <- top_terms %>% 
  filter(topic == top_terms[top_terms$term=="baba",]$topic) %>% 
  select(term, topic)

topics <- c(terms_health$topic[1], terms_nation$topic[1], 
            terms_patience$topic[1], terms_haram$topic[1], 
            terms_umma$topic[1], terms_trust$topic[1], 
            terms_family$topic[1])

topics <- cbind(topics, c("health", "nation", "patience", "haram", "umma", "trust", "family"))

names(topics) <- c("topic", "name")

topics <- as_tibble(topics)

topicProbabilities <- as.data.frame(lda_tm@gamma)

library(ggthemes)

corpus.tr <- cbind(corpus.tr[corpus.tr$text != "",], topicProbabilities)

#corpus.tr$date <-lubridate::ymd(corpus.tr$date)

library(lubridate)
ggplot(corpus.tr, aes(date, V46)) + geom_line() + theme_gdocs()

ggplot(corpus.tr, aes(date, V50)) + geom_line()+theme_gdocs()


events <- data_frame(date = c(dmy(30082000), dmy(30082001), dmy(30082002), dmy(30082015), dmy(30082016), dmy(30082017), dmy(30082018), dmy(30082019),
                              dmy(29102000), dmy(29102001), dmy(29102002), dmy(29102015), dmy(29102016), dmy(29102017), dmy(29102018), dmy(29102019),
                              dmy(18032000), dmy(18032001), dmy(18032002), dmy(18032015), dmy(18032016), dmy(18032017), dmy(18032018), dmy(18032019),
                              dmy(14072016), 
                              dmy(16042017),
                              dmy(20012018),
                              dmy(24062018),
                              dmy(31032019)),
                     event = c(rep('Victory day (30Aug)', 8), rep('Republic day (29Oct', 8), rep('Dardanelles victory', 8),  
                               'Coup attempt',
                               'Presidential referendum',
                               'Olive branch',
                               'General/pres elections',
                               'Mayoral elections'))

events <- data_frame(date = c(dmy(30082000), dmy(30082001), dmy(30082002), dmy(30082015), dmy(30082016), dmy(30082017), dmy(30082018), dmy(30082019),
                              dmy(29102000), dmy(29102001), dmy(29102002), dmy(29102015), dmy(29102016), dmy(29102017), dmy(29102018), dmy(29102019),
                              dmy(18032000), dmy(18032001), dmy(18032002), dmy(18032015), dmy(18032016), dmy(18032017), dmy(18032018), dmy(18032019),
                              dmy(14072016), 
                              dmy(16042017),
                              dmy(20012018),
                              dmy(24062018),
                              dmy(31032019)),
                     event = c(rep('Victory day (30Aug)', 8), rep('Republic day (29Oct)', 8), rep('Dardanelles victory', 8),  
                               'Coup attempt',
                               '',
                               'Olive branch',
                               '',
                               ''))

ggplot(corpus.tr[corpus.tr$date > ymd("2015-01-01"),], aes_string(x="date",y="V46")) +
  geom_line() +
  ylab("Nationalism in Friday sermons") +
  xlab("Time") +
  geom_vline(data = events[events$date > ymd("2015-01-01"),], aes(xintercept = as.numeric(date)), linetype = "dashed", colour = "gray") +
  geom_text(data = events[events$date > ymd("2015-01-01"),], mapping = aes(label = event, y = .20), angle = 60, hjust = 0)

ggplot(doc_lda, aes(gamma)) +
  geom_histogram() +
  facet_wrap(~topic) +
  scale_y_log10()

zzzz

corpus.tr_long <- corpus.tr[, c(-1,-2, -4)]
corpus.tr_long <- reshape2::melt(corpus.tr_long, id="date")

corpus.tr_long <- corpus.tr_long %>%
  group_by(variable) %>%
  mutate(mean.x = mean(value)) 

corpus.tr_long <- corpus.tr_long %>% group_by(variable) %>% mutate( g = group_indices() )

quantile(corpus.tr_long$mean.x, c(.20, .50, .80))

top10 <- quantile(corpus.tr_long$mean.x, c(.80))

top10top <- levels(corpus.tr_long[corpus.tr_long$mean.x > top10,]$variable)

ggplot(corpus.tr_long[corpus.tr_long$mean.x > top10,], aes_string(x="date",y="value", colour="variable")) +
  geom_line() +
  ylab("Topic") +
  xlab("Time") + facet_wrap(~g)

ggplot(corpus.tr_long[corpus.tr_long$g %in% topics,], aes_string(x="date",y="value", colour="variable")) +
  geom_line() +
  ylab("Topic") +
  xlab("Time") + facet_wrap(~g, ncol=1)

library(ggridges)
library(viridis)

library(graphics)
library(ggalt)
library(hrbrthemes)
library(ggthemes)
library(ggplot2)

corpus.tr_long$value2 <- corpus.tr_long$value*100 
corpus.tr_long$mean.x2 <- corpus.tr_long$mean.x*100

corpus.tr_long <- filter(corpus.tr_long, mean.x > top10)
  
p <- ggplot(corpus.tr_long, aes(date, value2)) +
  geom_line() +
  geom_hline(
    data = distinct(corpus.tr_long, g, mean.x2),
    aes(yintercept = 0, colour = mean.x2), size = 0.5 ) +
  scale_colour_viridis_c(option = "inferno", direction = -1, begin = 0.1, end = 0.9) +
  scale_fill_viridis_c(option = "inferno", direction = -1, begin = 0.1, end = 0.9) + 
  scale_y_continuous(expand = c(0,0), limits = c(0, 50)) +
  facet_wrap(~g, ncol = 1, strip.position = "left", dir = "h") +
  theme(strip.text.y = element_text(angle = 180, hjust = 1, vjust = 0)) +
  theme(panel.spacing.y = unit(0, "lines")) +
  theme(axis.text.y = element_blank()) +
  theme(legend.position = "none") +
 scale_y_continuous(expand = c(0,0), limits = c(0, 100)) +
  scale_colour_viridis_c(option = "inferno", direction = -1, begin = 0.1, end = 0.9) +
  scale_fill_viridis_c(option = "inferno", direction = -1, begin = 0.1, end = 0.9) +
  facet_wrap(~variable, ncol = 1, strip.position = "left", dir = "h") 

xxx

+
  scale_x_continuous(
    expand = c(0,0.125), limits = c(1, 51),
    breaks = month_idx, labels = month.abb
  ) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 105)) +
  scale_colour_viridis_c(option = "inferno", direction = -1, begin = 0.1, end = 0.9) +
  scale_fill_viridis_c(option = "inferno", direction = -1, begin = 0.1, end = 0.9) +
  facet_wrap(~name, ncol = 1, strip.position = "left", dir = "h") +
  labs(
    x = NULL, y = NULL, fill = NULL, colour = NULL,
    title = "1 big thing: The insane news cycles of 2019",
    subtitle = "Height is search interest in a given topic, indexed to 100.\nColor is average search interest between Dec. 30, 2018–Dec. 20, 2019",
    caption = "Source: Axios <https://www.axios.com/newsletters/axios-am-1d9cd913-6142-43b8-9186-4197e6da7669.html?chunk=0#story0>\nData: Google News Lab. Orig. Chart: Danielle Alberti/Axios"
  ) +
  theme_ipsum_es(grid="X", axis = "") +
  theme(strip.text.y = element_text(angle = 180, hjust = 1, vjust = 0)) +
  theme(panel.spacing.y = unit(-0.5, "lines")) +
  theme(axis.text.y = element_blank()) +
  theme(legend.position = "none")










corpus.tr_long$prop <- corpus.tr_long$value*100

ggplot(corpus.tr_long, aes_string(x="date",y="g", height="value2", fill="variable")) +
  geom_ridgeline_gradient(scale=2, gradient_lwd = 1.)  + 
  scale_fill_viridis(discrete = TRUE, direction = -1) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  ylab("Topic") +
  xlab("Time") +  theme(legend.position = "none") +
  facet_wrap(~g, ncol = 1, strip.position = "left", dir = "h") 
  theme(panel.spacing.y = unit(-0.05, "lines")) +
    theme(axis.text.y = element_blank())

ggplot(corpus.tr_long[corpus.tr_long$mean.x > 0.025,], aes_string(x="date",y="value", colour="variable")) +
  geom_smooth() +
  ylab("Topic") +
  xlab("Time") 

top_terms[top_terms$topic %in% c(4,10,11,18,21,32, 37, 40, 44, 47),]

top_terms[top_terms$topic %in%  c(4,10,11,18,21,32, 37, 40, 44, 47),] %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()


#Number of topics?

library("ldatuning")

result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 80, by = 2),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1234),
  mc.cores = 4L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)

xxxx

