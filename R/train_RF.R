
library(caret)
library(doParallel)

rds = getwd()
setwd("../R")
source("text_matrix_processing.R")
setwd(rds)
# air_unk_matrix = readRDS('air_unk_matrix.rds')
# air_unk_matrix_cov = readRDS('air_unk_matrix_cov.rds')
# air_outcome = readRDS(file = "air_outcome.rds")

# source("text_matrix_processing.R")
air_unk_matrix = readRDS('air_unk_matrix.rds')
air_unk_matrix_cov = readRDS('air_unk_matrix_cov.rds')
air_outcome = readRDS(file = "air_outcome.rds")


# Train without covariates -------------------------------------------------
# use air_unk_matrix (41177 x 3512)
# split to train and test
# fraction of data for training 
train_frac = 0.75

train_id = sample(1:nrow(air_unk_matrix),floor(train_frac*nrow(air_unk_matrix)), replace = F)
test_id = setdiff(1:nrow(air_unk_matrix), train_id)

# train_x = air_unk_matrix[train_id,]  # 30882 x 3512
# train_y = factor(air_outcome$recommended)[train_id]
# test_x = air_unk_matrix[test_id,]  # 10295 x 3512
# test_y = factor(air_outcome$recommended)[test_id]

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
  #Register cluster: 
  registerDoParallel(cl)
  # Find out how many cores are being used
  getDoParWorkers()
  
  # f <- reformulate(setdiff(colnames(X_train), "outcome"), response="outcome")
  air_rf = caret::train(form = as.factor(outcome) ~ .,
                        data = X_train[1:200,],
                        method = "parRF", # random forest
                        num.trees = 200,
                        trControl = caret::trainControl(method = "oob")) # resampling: out-of-bag
  stopCluster(cl)
  registerDoSEQ()
})


# model test data -------------------------------------------------
y_pred = predict(air_rf, X_test[100:200,c(-1)]) # outcome is at column one
# confusion matrix -------------------------------------------------
con.matrix = confusionMatrix(as.factor(y_pred), as.factor(X_test[100:200,1]))
print(con.matrix)
# save the model to disk
saveRDS(air_rf, "rf_model.rds")



##################################################

# Train with covariates -------------------------------------------------
# use air_unk_matrix_cov
train_x_cov = air_unk_matrix_cov[train_id,]  # 30882 x 3514
test_x_cov = air_unk_matrix_cov[test_id,]  # 10295 x 3514

# model fitting -------------------------------------------------
system.time({
  # Create cluster with desired number of cores: 
  cl = makeCluster(4)
  #Register cluster: 
  registerDoParallel(cl)
  # Find out how many cores are being used
  getDoParWorkers()
  
  f <- reformulate(setdiff(colnames(X_cov_train), "outcome"), response="outcome")
  
  air_rf_cov = caret::train(formula = f,
                            method = "parRF", # randon forest
                            num.trees = 200,
                            trControl = caret::trainControl(method = "oob")) # resampling: out-of-bag
  
  stopCluster(cl)
  registerDoSEQ()
})


# model test data -------------------------------------------------
y_pred_cov = predict(air_rf_cov, test_x_cov)
# confusion matrix -------------------------------------------------
con_matrix_cov = confusionMatrix(y_pred_cov, test_y)
print(con_matrix_cov)
# save the model to disk
saveRDS(air_rf_cov, "rf_cov_model.rds")



# col_names = colnames(air_unk_matrix)
# col_names_cov = colnames(air_unk_matrix_cov)

# saveRDS(col_names, "col_names.rds")
# saveRDS(col_names_cov, "col_names_cov.rds")
