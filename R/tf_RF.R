library(tidyverse)
library(tidytext) #text mining, unnesting
library(tm) #text mining
library(caret)
library(doParallel)

options(stringsAsFactors = FALSE)
airplanes = readr::read_csv('https://raw.githubusercontent.com/quankiquanki/skytrax-reviews-dataset/master/data/airline.csv')
# ff = c("Economy", "Premium Economy", "Business Class", "First Class")
# undesirable_words = c("a","the", "at", "is", "to", "from", "was", "it's", "and", "are", "can't", "as", "has", "this",
#                      "by", "if","his", "her", "them", "it", "she", "he", "them")

# modify, extract word -------------------------------------------------
air_tidy = airplanes %>%
  mutate(cabin_flown = factor(cabin_flown, levels = ff), ID = seq(1,dim(airplanes)[1])) %>%
  tidytext::unnest_tokens(word, content) %>%
  filter(!word %in% undesirable_words)

# merge with bing to get relevant words -------------------------------------------------
bing = get_sentiments("bing")
air_bing = air_tidy %>%
  #mutate(outcome = ifelse(recommended == 1, "Recommend", "Not recommend")) %>%
  select(ID, word, cabin_flown, recommended) %>%
  unique() %>%
  left_join(bing, by = "word") %>%
  na.omit()

# air_tfidf <- air_bing %>%
#     count(outcome, word) %>%
#     bind_tf_idf(term = word, document = outcome, n = n)

air_train = air_bing %>%
  count(ID, word, sort = TRUE)

air_outcome = unique(air_bing[,c("ID", "recommended")])

air_dtm = air_bing %>%
  #get word count per document to pass to cast_dtm
  count(ID, word, sort = TRUE) %>%
  ungroup() %>%
  #create a DTM with docs as rows and words as columns
  cast_dtm(document = ID, term = word, value = n)

# split to train and test
train_x = as.matrix(air_dtm)[1:10000,]
train_y = factor(air_outcome$recommended)[1:10000]
test_x = as.matrix(air_dtm)[10001:17000,]
test_y = factor(air_outcome$recommended)[10001:17000]


# model fitting -------------------------------------------------
# Find out how many cores are available: 
detectCores()
# Create cluster with desired number of cores: 
cl = makeCluster(4)
# Register cluster: 
registerDoParallel(cl)
# Find out how many cores are being used
getDoParWorkers()
air_rf = caret::train(x = train_x,
               y = train_y,
               method = "ranger", # randon forest
               num.trees = 200,
               trControl = trainControl(method = "oob")) # resampling: out-of-bag
stopCluster(cl)
registerDoSEQ()
# model test data -------------------------------------------------
y_pred = caret::predict(air_rf, test_x)
# confusion matrix -------------------------------------------------
con.matrix = confusionMatrix(y_pred, test_y)

# save the model to disk
saveRDS(air_rf, "./Desktop/final_model.rds")

# load the model
rf_model = readRDS("./Desktop/final_model.rds")
print(rf_model)

# test case -------------------------------------------------

comment = "My experience"
test = data.frame(ID = 1, content = comment) 

test = test %>% 
  unnest_tokens(word, content) %>%
  filter(!word %in% undesirable_words)

  
test_bing = test %>%
  select(ID, word) %>%
  unique() %>%
  left_join(bing, by = "word") %>%
  na.omit()

test_dtm <- test_bing %>%
  #get word count per document to pass to cast_dtm
  count(ID, word, sort = TRUE) %>%
  ungroup() %>%
  #create a DTM with docs as rows and words as columns
  cast_dtm(document = ID, term = word, value = n)

test_input = colnames(as.matrix(test_dtm))
test_matrix = matrix(0, nrow = 1, ncol = dim(train_x)[2])
colnames(test_matrix) = colnames(train_x)
for(i in test_input){
  pos = as.vector(grep(i, colnames(test_matrix)))[1]
  test_matrix[1,pos] = 1
}

test_pred = predict(air_rf, test_matrix)




# identify themes with top words
num_words <- 100 #number of words to visualize

#create function that accepts the lda model and num word to display
top_terms <- function(lda_model, num_words) {
  
  #tidy LDA object to get word, topic, and probability (beta)
  topics_tidy <- tidy(lda_model, matrix = "beta")
  
  top_terms <- topics_tidy %>%
    group_by(topic) %>%
    arrange(topic, desc(beta)) %>%
    slice(seq_len(num_words))
  
  topic1 = top_terms[top_terms$topic ==1,"term"]
  topic2 = top_terms[top_terms$topic ==2,"term"]
  print(paste0("Top ",num_words, " words in Recommended is: ",topic1))
  print(paste0("Top ",num_words, " words in NOT Recommended is: ",topic2))
  
}
#call the function you just built!
top_terms(lda, num_words)

