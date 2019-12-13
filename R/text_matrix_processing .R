library(tidyverse)
library(tidytext) #text mining, unnesting
library(tm) #text mining

rm(list = ls())

options(stringsAsFactors = FALSE)
airplanes = readr::read_csv('https://raw.githubusercontent.com/quankiquanki/skytrax-reviews-dataset/master/data/airline.csv')
ff = c("Economy", "Premium Economy", "Business Class", "First Class")
undesirable_words = c("a","the", "at", "is", "to", "from", "was", "it's", "and", "are", "can't", "as", "has", "this",
                    "by", "if","his", "her", "them", "it", "she", "he", "them")

# modify, extract word -------------------------------------------------
air_tidy = airplanes %>%
  mutate(cabin_flown = factor(cabin_flown, levels = ff), ID = seq(1,dim(airplanes)[1])) %>%
  unnest_tokens(word, content) %>%
  filter(!word %in% undesirable_words)

# merge with bing to get relevant words -------------------------------------------------
bing = get_sentiments("bing")
air_bing = air_tidy %>%
  mutate(outcome = ifelse(recommended == 1, "Recommend", "Not recommend")) %>%
  select(ID, word, outcome, rich, us , recommended) %>%
  unique() %>%
  left_join(bing, by = "word") %>%
  na.omit()

# air_tfidf <- air_bing %>%
#     count(outcome, word) %>%
#     bind_tf_idf(term = word, document = outcome, n = n)

air_dtm_matrix = air_bing %>%
  count(ID, word, sort = TRUE)

air_outcome = unique(air_bing[,c("ID", "recommended", 'us', 'rich')])


air_dtm = air_bing %>%
  #get word count per document to pass to cast_dtm
  count(ID, word, sort = TRUE) %>%
  ungroup() %>%
  #create a DTM with docs as rows and words as columns
  cast_dtm(document = ID, term = word, value = n)
