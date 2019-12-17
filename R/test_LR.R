library(tidyverse)
library(tidytext)
library(tm)
library(glmnet)
options(stringsAsFactors = FALSE)


LR_model = readRDS("./rds/logistic_model.rds")
LR_cov_model = readRDS("./rds/logistic_model_cov.rds")

col_names = readRDS("./rds/col_names.rds")
col_names_cov = readRDS("./rds/col_names_cov.rds")
col_names[which(col_names == "break")] = "brk"
col_names_cov[which(col_names_cov == "break")] = "brk"

# test case 
comment = "MUC-SKG on 17th of Dec. One-way. New A320 with new style of seats quite uncomfortable though. Friendly and nice crew. A hot meal was served in this short flight and drinks. However passengers didn't obey at all to the new cabin baggage rules causing space problems and minor delays and the crew did nothing about it. Rules should be more strictly enforced."
test = data.frame(ID = 1, content = comment) 

test = test %>% 
  unnest_tokens(word, content)
bing = get_sentiments(lexicon = "bing")


test_bing = test %>%
  select(ID, word) %>%
  unique() %>%
  left_join(bing, by = "word") %>%
  na.omit()

# add UNK if there are no words
if (dim(test_bing)[1] == 0){
  UNK = data.frame(ID = 1, word = "UNK", sentiment = NA)
  test_bing = rbind(test_bing, UNK)
} else {test_bing = test_bing}

test_dtm = test_bing %>%
  #get word count per document to pass to cast_dtm
  count(ID, word, sort = TRUE) %>%
  ungroup() %>%
  #create a DTM with docs as rows and words as columns
  cast_dtm(document = ID, term = word, value = n)

# create the big matrix contain all the words (like in training set)
test_input = colnames(as.matrix(test_dtm))
test_matrix = matrix(0, nrow = 1, ncol = length(col_names))
colnames(test_matrix) = col_names
for(i in test_input){
  if (i == "break"){
    i == "brk"
  }
  pos = as.vector(grep(i, colnames(test_matrix)))[1]
  test_matrix[1,pos] = 1
}

# test_pred = predict.glmnet(LR_model, test_matrix,type="response")
test_pred = ifelse(predict.glmnet(LR_model, test_matrix, type="response") > 0.5,1,0)
