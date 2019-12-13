######################
##### SETUP
##################### 
rm(list = ls())
library(tidyverse)
library(tidytext)
library(e1071) # SVM
# go lobos 
# read this god damn data in 

airplanes = readr::read_csv('https://raw.githubusercontent.com/quankiquanki/skytrax-reviews-dataset/master/data/airline.csv')

##############################
###### GRAPHS AND PREPROCESSING
###############################

# barplot 

airplanes %>% select(airline_name, recommended) %>% group_by(airline_name) %>% 
  summarize(pct = sum(recommended)/length(recommended)) %>% 
  filter(airline_name %in% c('delta-air-lines', 'american-airlines', 'jetblue-airways','southwest-airlines')) %>% 
  ggplot(aes(x = airline_name, y = pct)) + geom_col(width = 0.4) + theme_bw() + 
  xlab('Airline') + ylab('Proportion Recommeding') + ylim(0,1) + 
  scale_x_discrete(labels = c('American','Delta', 'JetBlue','Southwest'))


##################### 
###### Explore Data 
####################


airplanes$year = with(airplanes, format(date,"%Y"))
airplanes = airplanes %>% 
    mutate(year = as.factor(format(date,"%Y"))) %>% 
      filter(! year == '1970') 

airplanes$year = relevel(airplanes$year, ref = '2002')


fit_year = glm(recommended ~ year, data = airplanes, family = 'binomial')

summary(fit_year)

# year does not mean a thing, do not include! 
#

# set cabin type as a factor 

airplanes$cabin_flown = as.factor(airplanes$cabin_flown)

airplanes$cabin_flown = relevel(airplanes$cabin_flown, "Economy")

fit_cabin = glm(recommended ~ cabin_flown, data = airplanes, family = 'binomial')

summary(fit_cabin) # rich people more likely to recommend the flight 

airplanes = airplanes %>% mutate(rich = ifelse(cabin_flown %in% c('Economy', 'Premium Economy'), 0,1))

# can use this ^ as a variable in algos 

# set travel type as a factor 

airplanes$type_traveller = as.factor(airplanes$type_traveller)

airplanes$type_traveller = relevel(airplanes$type_traveller, "Business")

fit_type = glm(recommended ~ type_traveller, data = airplanes, family = 'binomial')

summary(fit_type) # solo people more likely, no other significant difference 

# also hella NA (like 39,000).... so maybe not lol  

airplanes = airplanes %>% mutate(alone = ifelse(type_traveller == "Solo Leisure",1,0))

airplanes$airbus = grepl('[Aa]3',airplanes$aircraft) | grepl('[Aa]irbus', airplanes$aircraft)

airplanes$boeing = grepl('[Bb]oeing',airplanes$aircraft) | grepl('[Bb]7', airplanes$aircraft)

airplanes = airplanes %>% mutate(reported_plane_type  = ifelse(boeing ==1 | airbus == 1 , 1,0))

fit_plane = glm(recommended ~ reported_plane_type + rich, data = airplanes, family = 'binomial')

summary(fit_plane)

# more likely to recommend if you report what type of plane it was 
# this doesn't make sense to use - there is too much missing data 

airplanes = airplanes %>% mutate(us = ifelse(author_country %in% c('United States','United Kingdom','Australia'),1,0))

fit_us = glm(recommended ~ us, data = airplanes, family = 'binomial')

summary(fit_us) # lol wicked less likely to recommend if you are from america 

#################################
######### Train/Test Data
###############################

# fraction of data for training 
train_frac = 0.75
# set up training/testing data

train_id = sample(1:nrow(airplanes),floor(train_frac*nrow(airplanes)), replace = F)

test_id = setdiff(1:nrow(airplanes), train_id)

train = airplanes[train_id,] %>% select(us, cabin_flown, recommended)

test = airplanes[test_id,] %>% select(us,cabin_flown, recommended)


#################################
######### Fitting a Logistic Model
##################################

# models 

system.time(glm(recommended ~ . -1 , data = train, family ='binomial'))

model_logistic = glm(recommended ~ . -1 , data = train, family ='binomial')

summary(model_logistic)

# predictions 

predictions = ifelse(predict.glm(model_logistic,newdata = test) > 0.5, 1,0)

id_na = is.na(predictions) | is.na(test$recommended)

pred = predictions[!id_na]

real = test$recommended[!id_na]

sum(pred == real)/length(real)



#########################
###### Random Forest 
#########################

#########################
###### SVM 
#########################

system.time(svm(e1071::formula = recommended ~ ., 
               data = train, 
               type = 'C-classification', 
               kernel = 'linear'))

classifier = e1071::svm(formula = recommended ~ ., 
                 data = train, 
                 type = 'C-classification', 
                 kernel = 'linear')

y_pred = predict(classifier, newdata = test)

sum(is.na(y_pred))

#########################
###### NLP 
##########################

airplanes1 = airplanes %>% unnest_tokens(word, content) %>% 
  filter(!word %in% not_needed)

airplanes1 %>% select(author,word, recommended) %>% group_by(author) %>% mutate(word_count = n()) %>% 
  distinct(author, word_count, recommended) %>% ungroup() %>% group_by(recommended) %>% 
  summarise(avg_word_count = mean(word_count))

