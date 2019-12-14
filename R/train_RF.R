
library(caret)
library(doParallel)

source("text_matrix_processing.R")

# Train without covariates -------------------------------------------------
# use air_unk_matrix (41177 x 3512)
# split to train and test
# fraction of data for training 
train_frac = 0.75
train_id = sample(1:nrow(air_unk_matrix),floor(train_frac*nrow(air_unk_matrix)), replace = F)
test_id = setdiff(1:nrow(air_unk_matrix), train_id)

train_x = air_unk_matrix[train_id,]  # 30882 x 3512
train_y = factor(air_outcome$recommended)[train_id]
test_x = air_unk_matrix[test_id,]  # 10295 x 3512
test_y = factor(air_outcome$recommended)[test_id]


# model fitting -------------------------------------------------
system.time({
  # Find out how many cores are available: 
  # detectCores()
  # Create cluster with desired number of cores: 
  #cl = makeCluster(4)
  # Register cluster: 
  # registerDoParallel(cl)
  # Find out how many cores are being used
  # getDoParWorkers()
  air_rf = caret::train(x = train_x,
                        y = train_y,
                        method = "ranger", # randon forest
                        num.trees = 200,
                        trControl = caret::trainControl(method = "oob")) # resampling: out-of-bag
  # stopCluster(cl)
  # registerDoSEQ()
})


# model test data -------------------------------------------------
y_pred = predict(air_rf, test_x)
# confusion matrix -------------------------------------------------
con.matrix = confusionMatrix(y_pred, test_y)
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
  air_rf_cov = caret::train(x = train_x_cov,
                            y = train_y,
                            method = "ranger", # randon forest
                            num.trees = 200,
                            trControl = caret::trainControl(method = "oob")) # resampling: out-of-bag
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
