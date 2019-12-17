library(tidyverse)
library(e1071)
library(doParallel)

#source("text_matrix_processing.R")
# air_unk_matrix = readRDS('~/Dropbox/UMich/Fall2019/Biostat625/Project/rds/air_unk_matrix.rds')
# air_unk_matrix_cov = readRDS('~/Dropbox/UMich/Fall2019/Biostat625/Project/rds/air_unk_matrix_cov.rds')
# air_outcome  = readRDS(file = "~/Dropbox/UMich/Fall2019/Biostat625/Project/rds/air_outcome.rds")

air_unk_matrix = readRDS('../rds/air_unk_matrix.rds')
air_unk_matrix_cov = readRDS('../rds/air_unk_matrix_cov.rds')
air_outcome = readRDS(file = "../rds/air_outcome.rds")

# Train without covariates -------------------------------------------------
# use air_unk_matrix (41177 x 3512)
# split to train and test
# fraction of data for training 

train_frac = 0.75

train_id = sample(1:nrow(air_unk_matrix),floor(train_frac*nrow(air_unk_matrix)), replace = F)
test_id = setdiff(1:nrow(air_unk_matrix), train_id)

X = cbind.data.frame('outcome' = air_outcome$recommended, air_unk_matrix)

X$outcome <- as.factor(X$outcome)

X_train = X[train_id,]
X_test = X[test_id,]


system.time({
  # Find out how many cores are available: 
  # detectCores()
  # Create cluster with desired number of cores: 
  cl = makeCluster(4)
  # Register cluster: 
  registerDoParallel(cl)
  # Find out how many cores are being used
  getDoParWorkers()
  
  # f <- reformulate(setdiff(colnames(X_train), "outcome"), response="outcome")
  
  air_svm = e1071::svm(formula = as.factor(outcome) ~ ., 
                            data = X_train, 
                            type = 'C-classification', 
                            kernel = 'linear')
  
  stopCluster(cl)
  registerDoSEQ()
  
})

# model test data -------------------------------------------------
y_pred = predict(air_svm, X_test[, c(-1)])
# confusion matrix -------------------------------------------------
con.matrix = caret::confusionMatrix(as.factor(y_pred), as.factor(X_test[, 1]))
print(con.matrix)
# save the model to disk
saveRDS(air_svm, "svm_model.rds")


#  Train with covariates -------------------------------------------------
  # use air_unk_matrix_cov (41177 x 3512)
  # split to train and test
  # fraction of data for training 
  
train_frac = 0.75

X_cov = cbind.data.frame('outcome' = air_outcome$recommended, air_unk_matrix_cov)

X_cov$outcome <- as.factor(X_cov$outcome)

X_train_cov = X_cov[train_id,]
X_test_cov = X_cov[test_id,]


system.time({
  # Find out how many cores are available: 
  # detectCores()
  # Create cluster with desired number of cores: 
  cl = makeCluster(4)
  # Register cluster: 
  registerDoParallel(cl)
  # Find out how many cores are being used
  getDoParWorkers()
  
  # f <- reformulate(setdiff(colnames(X_train_cov), "outcome"), response="outcome")
  
  air_svm_cov = e1071::svm(formula = outcome ~ ., 
                       data = X_train_cov, 
                       type = 'C-classification', 
                       kernel = 'linear')
  
  stopCluster(cl)
  registerDoSEQ()
  
})

# model test data -------------------------------------------------
y_pred_cov = predict(air_svm, X_test_cov[,-c(1)])
# confusion matrix -------------------------------------------------
con.matrix = caret::confusionMatrix(y_pred_cov, X_test_cov$outcome)
print(con.matrix)
# save the model to disk
saveRDS(air_svm_cov, "svm_model_cov.rds")
