library(biglm)
library(tidyverse)
library(caret)
library(doParallel)


#source("text_matrix_processing.R")
air_unk_matrix = readRDS('~/Dropbox/UMich/Fall2019/Biostat625/Project/rds/air_unk_matrix.rds')
air_unk_matrix_cov = readRDS('~/Dropbox/UMich/Fall2019/Biostat625/Project/rds/air_unk_matrix_cov.rds')
air_outcome  = readRDS(file = "~/Dropbox/UMich/Fall2019/Biostat625/Project/rds/air_outcome.rds")

# Train without covariates -------------------------------------------------
# use air_unk_matrix (41177 x 3512)
# split to train and test
# fraction of data for training 

train_frac = 0.01

train_id = sample(1:nrow(air_unk_matrix),floor(train_frac*nrow(air_unk_matrix)), replace = F)
test_id = setdiff(1:nrow(air_unk_matrix), train_id)

X = cbind.data.frame('outcome' = air_outcome$recommended, air_unk_matrix)

X_train = X[train_id,]
X_test = X[test_id,]

original_col_names = colnames(X_train) # save original column names 

break_id = which(colnames(X_train) == 'break')  # can't have column names like 'next' or 'break' in glm


# we had break, not next, so i changed this! 
colnames(X_train)[break_id] = 'brk'
colnames(X_test)[break_id] = 'brk'

# model fitting -------------------------------------------------

system.time({
  # Find out how many cores are available: 
  # detectCores()
  # Create cluster with desired number of cores: 
  cl = makeCluster(4)
  # Register cluster: 
  registerDoParallel(cl)
  # Find out how many cores are being used
  getDoParWorkers()
  
  f <- reformulate(setdiff(colnames(X_train), "outcome"), response="outcome")
  
  air_logistic = glm(formula = f, data = X_train, family = 'binomial')
  
  stopCluster(cl)
  registerDoSEQ()
  
})



# model test data -------------------------------------------------
y_pred = predict(air_logistic, X_test)
# confusion matrix -------------------------------------------------
con.matrix = confusionMatrix(y_pred, X_test$outcome)
print(con.matrix)
# save the model to disk
saveRDS(air_logistic, "logistic_model.rds")

# restore the matrices to their original form ----------- 
colnames(X_train) = original_col_names
colnames(X_test) = original_col_names


# Train without covariates -------------------------------------------------
X_cov = cbind.data.frame('outcome' = air_outcome$recommended, air_unk_matrix_cov)

X_cov_train = X_cov[train_id,]
X_cov_test = X_cov[test_id,]

original_col_names = colnames(X_cov_train) # save original column names 

break_cov_id = which(colnames(X_cov_train) == 'break')  # can't have column names like 'next' or 'break' in glm


# we had break, not next, so i changed this! 
colnames(X_cov_train)[break_id] = 'brk'
colnames(X_cov_test)[break_id] = 'brk'

# model fitting -------------------------------------------------

system.time({
  
  # Find out how many cores are available: 
  # detectCores()
  # Create cluster with desired number of cores: 
  cl = makeCluster(4)
  # Register cluster: 
  registerDoParallel(cl)
  # Find out how many cores are being used
  getDoParWorkers()
  
  f <- reformulate(setdiff(colnames(X_cov_train), "outcome"), response="outcome")
  
  air_logistic_cov = glm(formula = f, data = X_cov_train, family = 'binomial')
  
  stopCluster(cl)
  registerDoSEQ()
  
})


# model test data -------------------------------------------------
y_pred_cov = predict(air_logistic_cov, X_cov_test)
# confusion matrix -------------------------------------------------
con_matrix_cov = confusionMatrix(y_pred_cov, X_cov_test$outcome)
print(con_matrix_cov)
# save the model to disk
saveRDS(air_logistic_cov, "logistic_model_cov.rds")


