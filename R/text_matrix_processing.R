library(tidyverse)
library(tidytext) #text mining, unnesting
library(tm) #text mining


# load data -------------------------------------------------
airplanes = readRDS(file = "airplanes.rds")
air_tidy_cov = airplanes %>%
  mutate(ID = seq(1,dim(airplanes)[1])) %>%
  tidytext::unnest_tokens(word, content) %>%
  select(ID, word, recommended, us, high_income)
# dim(air_tidy_cov) 4880627 x 5

# merge with bing to get relevant words -------------------------------------------------
bing = tidytext::get_sentiments("bing")
air_bing = air_tidy_cov %>%
  select(ID, word,recommended, us, high_income) %>%
  unique() %>%
  left_join(bing, by = "word") %>%
  na.omit()
# dim(air_bing) 294100 x 6 (ID, word, recommended, us, high_income, sentiment)

# word count -------------------------------------------------
air_word_count = air_bing %>%
  count(ID, word, sort = TRUE)

# extract outcome (recommended) -------------------------------------------------
air_outcome = unique(air_bing[,c("ID", "recommended")])  # 41177 x 2

# extract covariates 
air_covariates = unique(air_bing[,c("ID", "us", "high_income")]) # 41177 x 3


# long to wide -------------------------------------------------
air_dtm = air_bing %>%
  #get word count per document to pass to cast_dtm
  count(ID, word, sort = TRUE) %>%
  ungroup() %>%
  #create a DTM with docs as rows and words as columns
  cast_dtm(document = ID, term = word, value = n)

# change DTM to matrix
air_dtm_matrix = as.matrix(air_dtm) # 41178 x 3511

# add UNK -------------------------------------------------
# add UNK indicates unknown word, set to 1
UNK = matrix(1, ncol = 1, nrow = dim(air_dtm_matrix)[1])
colnames(UNK) = "UNK"
air_unk_matrix = cbind(air_dtm_matrix, UNK) # 41178 x 3512
# air_unk_matrix is data for "train without covariates"

# merge with covariates -------------------------------------------------
air_unk_matrix_id = as.data.frame(air_unk_matrix) %>%
  mutate(ID = seq(1,dim(air_unk_matrix)[1]))

air_unk_matrix_cov = cbind(air_unk_matrix_id, air_covariates[,2:3])
air_unk_matrix_cov = air_unk_matrix_cov %>% select(-ID)


air_unk_matrix_cov = as.matrix(air_unk_matrix_cov) #M41177 x 3514
# air_unk_matrix_cov is data for "train with covariates"









