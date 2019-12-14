# read column names, this is to create headers for test matrix
options(stringsAsFactors = FALSE)
col_names = readRDS("col_names.rds")
col_names_cov = readRDS("col_names_cov.rds")

# Model without covariates -------------------------------------------------
# load the model
rf_model = readRDS("rf_model.rds")
print(rf_model)

# test case 
comment = "My experience"
test = data.frame(ID = 1, content = comment) 

test = test %>% 
  unnest_tokens(word, content)


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
  pos = as.vector(grep(i, colnames(test_matrix)))[1]
  test_matrix[1,pos] = 1
}

test_pred = predict(air_rf, test_matrix)

# Model with covariates -------------------------------------------------
# load the model
rf_cov_model = readRDS("rf_cov_model.rds")
print(rf_cov_model)

# create the big matrix contain all the words (like in training set)
# will mark 1 to the position the column is located at 
test_matrix_cov = matrix(0, nrow = 1, ncol = length(col_names_cov))
colnames(test_matrix_cov) = col_names_cov
for(i in test_input){
  pos = as.vector(grep(i, colnames(test_matrix_cov)))[1]
  test_matrix_cov[1,pos] = 1
}

# TODO: need condition to add us, high_income into the matrix
