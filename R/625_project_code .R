######################
##### SETUP
###################
rm(list = ls())
library(tidyverse)
library(tidytest)
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

airplanes1 = airplanes %>% unnest_tokens(word, content)

# set cabin type as a factor 
airplanes$cabin_flown = as.factor(airplanes$cabin_flown)
airplanes$cabin_flown = relevel(airplanes$cabin_flown, "Economy")

# set travel type as a factor 
airplanes$type_traveller = as.factor(airplanes$type_traveller)
airplanes$type_traveller = relevel(airplanes$type_traveller, "Business")

#################################
######### Fitting a Logistic Model
##################################


# fraction of data for training 
train_frac = 0.75
# set up training/testing data

train_id = sample(1:nrow(airplanes),floor(train_frac*nrow(airplanes)), replace = F)
test_id = setdiff(1:nrow(airplanes), train_id)

train = airplanes[train_id,]

test = airplanes[test_id,]

# models 

fit = glm(recommended ~ type_traveller, data = train, family = 'binomial')

summary(fit)


# accuracy 
lin = coefficients(fit)[1] + coefficients(fit)[2]*test$seat_comfort_rating + 

pi = exp(lin)/(1 + exp(lin))

pred = ifelse(pi <= 0.5, 0,1)

sum(test$recommended == pred, na.rm = T)/(nrow(test) - sum(is.na(test$recommended)))


#########################
###### GAUSSIAN PROCESS
#########################

#########################
###### NLP 
##########################


