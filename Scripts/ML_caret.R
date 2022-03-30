library(caret)
library(tidyverse)
library(corrplot)
library(randomForest)
library(lubridate)
library(xgboost)
library(elasticnet)

vegData <- read.csv("./Data/transect_vegetation_data_2022-03-17_cleaned.csv")
bycatchData <- read.csv("./Data/insect_bycatch_2022-03-17_cleaned.csv", fileEncoding="UTF-8-BOM")
full_model_data <- read.csv("./Data/insect_veg_combined_2022-03-17.csv")

#dummy variable is used to create one-hot encoding AKA categorical column as a feature converted to numeric

##### Megan Basic ML Algs ---------------------------------------------------
#Listed in order of most to least transects with occurrence
#Need to change the call in 3 places: bug_data, ml_formula, and the name in md_split, and sometimes confusionMatrix name
insects_interest <- c("ciccadellidae", "miridae", "melyridae",
                      'delphacidae', 'aphididae', 'nabidae', 'coccinellidae')

###
###  1  modeling with all veg, abundance based
###
#Create the model data with the veg and insect, can't keep all the bugs in until we look at specific response variables
##Modifying data as needed
bug_data <- bycatchData %>%
  dplyr::select('site', 'transect', 'sampling_period', insects_interest[1])
model_data <- vegData %>%
  full_join(bug_data, by = c("sampling_period", "site","transect")) %>%
  mutate(
    sampling_period = as.factor(.$sampling_period),
    date = mdy(.$date))

#Formula uses all other columns as predictors
ml_formula <- as.formula(paste0(insects_interest[1],
                                "~", "."))
#Data split
#Need to split data AND do folding. The folding tests on all data so without it the model performs better due to viewing all datapoints
md_split <- createDataPartition(model_data$ciccadellidae, p = 0.7, list = FALSE)
md_train <- model_data[md_split,]
md_test <- model_data[-md_split,]

train_control <- trainControl(method = "cv", number = 10, p = 0.7)

#### RF Model
set.seed(7)
rf_rmod <- train(form = ml_formula,
                  data = md_train,
                  method="rf",
                  trControl=train_control)
rf_rfit <- predict(rf_rmod)

### Ridge Regression
set.seed(7)
rr_fit <- train(form = ml_formula,
                data = md_train,
                method="ridge",
                trControl=train_control)

####Gradient Boosted Model
#I think this runs but my computer doesn't like it so need to try at office
set.seed(7)
boo_rmod <- train(form = ml_formula, data = md_train, method = 'xgbTree', trControl = train_control, tuneLength=5, verbosity = 0)

#Compare models
#model_compare <- resamples(RF = rf_rmod, Boosted = boo_rmod)
#summary(models_compare)

###
###  2  modeling with all veg, pres/abs based ---> all veg p/a or just bug? just the bug right now
###

#Create the model data with the veg and insect, can't keep all the bugs in until we look at specific response variables
##Modifying data as needed
##Need to make the presence/absence a factor to use classification
bug_data <- bycatchData %>%
  select('site', 'transect', 'sampling_period', insects_interest[1]) %>%
  mutate(sampling_period = as.factor(.$sampling_period),
         transect = as.factor(.$transect)) %>%
  mutate_if(is.numeric, ~1 * (. != 0))

model_data <- vegData %>%
  mutate(sampling_period = as.factor(.$sampling_period),
         transect = as.factor(.$transect)) %>%
  full_join(bug_data, by = c("sampling_period", "site","transect")) %>%
  mutate(
    date = mdy(.$date)) %>%
  mutate_if(is.numeric, as.factor)

#Formula uses all other columns as predictors
ml_formula <- as.formula(paste0(insects_interest[1],
                                "~", "."))
#Data split
md_split <- createDataPartition(model_data$ciccadellidae, p = 0.7, list = FALSE)
md_train <- model_data[md_split,]
md_test <- model_data[-md_split,]

train_control <- trainControl(method = "cv", number = 10, p = 0.7)

#####Random Forest model
set.seed(7)
rf_cmod <- train(form = ml_formula,
                 data = md_train,
                 method="rf",
                 trControl=train_control)
rf_cmod

####Gradient Boosted Model
#I think this runs but my computer doesn't like it so need to try at office
set.seed(7)
boo_rmod <- train(form = ml_formula, data = md_train, method = 'xgbTree', trControl = train_control, tuneLength=5, verbosity = 0)


predict_test <- predict(rf_cmod, md_test)
confusionMatrix(reference = md_test$ciccadellidae, data = predict_test, mode = 'everything')
