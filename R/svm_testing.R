library(tidyverse)
library(Hmisc) # imputation
library(e1071) # SVM
airplanes = readr::read_csv('https://raw.githubusercontent.com/quankiquanki/skytrax-reviews-dataset/master/data/airline.csv')

# set cabin type as a factor 
airplanes$cabin_flown = as.factor(airplanes$cabin_flown)
airplanes$cabin_flown = relevel(airplanes$cabin_flown, "Economy")

# set travel type as a factor 
airplanes$type_traveller = as.factor(airplanes$type_traveller)
airplanes$type_traveller = relevel(airplanes$type_traveller, "Business")

subAirplane = airplanes %>% select(-airline_name, -link, -title, -author, -author_country, -date, -content, -aircraft, -route)

# logistic regression
fit_log = glm(recommended ~ . -1, data = subAirplane, family = 'binomial')
summary(fit_log)
# significant variables
# type_travellerBusiness type_travellerCouple Leisure, type_travellerFamilyLeisure, type_travellerSolo Leisure
# overall_rating, cabin_staff_rating, wifi_connectivity_rating

# change "type_traveller" and "cabin_flown"  to dummy variables         
airplane_dummy = fastDummies::dummy_cols(airplanes, select_columns =c("type_traveller", "cabin_flown"))
sairplane = airplane_dummy %>% 
  mutate(type_traveller_Couple = `type_traveller_Couple Leisure`,
         type_traveller_Solo = `type_traveller_Solo Leisure`,
         type_traveller_Family = `type_traveller_FamilyLeisure`) %>%
  select(recommended, type_traveller_Business, type_traveller_Couple, type_traveller_Family, type_traveller_Solo,
         overall_rating, cabin_staff_rating, wifi_connectivity_rating)



# impute with mean value
sairplane = impute(sairplane, "median")
sum(is.na(sairplane))
# Splitting the data into test and train
set.seed(45)
# fraction of data for training 
train_frac = 0.7
# set up training/testing data
train_id = sample(1:nrow(sairplane),floor(train_frac*nrow(sairplane)), replace = F)
train = sairplane[train_id,] 
test = sairplane[-(train_id),]

# SVM
classifier = svm(formula = recommended ~ ., 
                 data = train, 
                 type = 'C-classification', 
                 kernel = 'linear') 
# Predicting the Test set results 
y_pred = predict(classifier, newdata = test[,-1])

# Making the Confusion Matrix 
cm = table(test[,1], y_pred) 
acry = (cm[1,1]+cm[2,2])/sum(cm)

# test
# test1 = c(0,1,0,0,10,1,0)
# y_pred = predict(classifier, newdata = t(as.matrix(test1)))




