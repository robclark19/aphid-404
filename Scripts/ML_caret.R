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

#dummy variable is used to create one-hot encoding AKA categorical column as a feature converted to numeric 0 or 1

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
  dplyr::select('site', 'transect', 'sampling_period', insects_interest[2])
model_data <- vegData %>%
  dplyr::full_join(bug_data, by = c("sampling_period", "site","transect")) %>%
  plyr::mutate(
    sampling_period = as.factor(.$sampling_period),
    date = mdy(.$date))


#Formula uses all other columns as predictors
ml_formula <- as.formula(paste0(insects_interest[2],
                                "~", "."))
#Data split
#Need to split data AND do folding. The folding tests on all data so without it the model performs better due to viewing all datapoints
md_split <- createDataPartition(model_data$miridae, p = 0.7, list = FALSE)
md_train <- model_data[md_split, !(names(model_data) %in% c('date', 'site', 'transect', 'sampling_period'))]
md_test <- model_data[-md_split, !(names(model_data) %in% c('date', 'site', 'transect', 'sampling_period'))]

train_control <- trainControl(method = "cv", number = 10, p = 0.7)

#### RF Model
set.seed(7)
rf_rmod <- train(form = ml_formula,
                  data = md_train,
                  method="rf",
                  trControl=train_control)
rf_rfit <- predict(rf_rmod)
predict_test <- predict(rf_rmod, md_test)
plot(md_train$miridae, rf_rfit)

### Ridge Regression
#This won't work, missing RMSE values error
#Glmnet with alpha = 0 is ridge and 1 is lasso
set.seed(7)
rid_rmod <- train(form = ml_formula,
                data = md_train,
                method="ridge",
                trControl=train_control)
set.seed(7)
ridge_rmod <- train(form = ml_formula, data = md_train, method = 'glmnet', tuneGrid = expand.grid(alpha = 0, lambda = 1))
ridge_fit <- predict(ridge_rmod)
ridge_rmse <- RMSE(ridge_fit, md_train$miridae)

####Gradient Boosted Model
#This won't work, missing RMSE values error

set.seed(7)
boo_rmod <- train(form = ml_formula, data=md_train, method = 'xgbTree', trControl = train_control, 
                  nrounds = 20)

##Boosted model via xgboost package. Need to fix using caret
xtrain = as.matrix(md_train[,-64])
ytrain = as.matrix(md_train[,64])
set.seed(7)
xgb_train <- xgb.DMatrix(data = xtrain, label = ytrain)
boo_rmod <- xgboost(data = xgb_train, nrounds = 50)

#Compare models
#model_compare <- resamples(RF = rf_rmod, Boosted = boo_rmod, Ridge = rr_fit)
#summary(models_compare)

###
###  2  modeling with all veg, pres/abs based ---> all veg p/a or just bug? just the bug right now
###

#Create the model data with the veg and insect, can't keep all the bugs in until we look at specific response variables
##Modifying data as needed
##Need to make the presence/absence a factor to use classification
bug_data <- bycatchData %>%
  dplyr::select('site', 'transect', 'sampling_period', insects_interest[2]) %>%
  plyr::mutate(sampling_period = as.factor(.$sampling_period),
         transect = as.factor(.$transect)) %>%
  dplyr::mutate_if(is.numeric, ~1 * (. != 0))

model_data <- vegData %>%
 plyr::mutate(sampling_period = as.factor(.$sampling_period),
         transect = as.factor(.$transect)) %>%
  dplyr::full_join(bug_data, by = c("sampling_period", "site","transect")) %>%
  plyr::mutate(
    date = mdy(.$date)) %>%
  dplyr::mutate_if(is.numeric, as.factor)

#Formula uses all other columns as predictors
ml_formula <- as.formula(paste0(insects_interest[2],
                                "~", "."))
#Data split
md_split <- createDataPartition(model_data$miridae, p = 0.7, list = FALSE)
md_train <- model_data[md_split,!(names(model_data) %in% c('date', 'site', 'transect', 'sampling_period'))]
md_test <- model_data[-md_split,!(names(model_data) %in% c('date', 'site', 'transect', 'sampling_period'))]

train_control <- trainControl(method = "cv", number = 10, p = 0.7)

#####Random Forest model
set.seed(7)
rf_cmod <- train(form = ml_formula,
                 data = md_train,
                 method="rf",
                 trControl=train_control)
rf_cmod


##### Ridge regression
set.seed(7)
ridge_cmod <- train(form = ml_formula, data = md_train, method = 'glmnet', tuneGrid = expand.grid(alpha = 0, lambda = 1))
ridge_cfit <- predict(ridge_cmod)
ridge_rmse <- RMSE(ridge_cfit, md_train$miridae)

####Gradient Boosted Model
#I think this runs but my computer doesn't like it so need to try at office
xtrain = as.numeric(as.matrix(md_train[,-64]))
ytrain = as.factor(as.matrix(md_train[,64]))

set.seed(7)
boo_rmod <- train(form = ml_formula, data = md_train, method = 'xgbTree', 
                  trControl = train_control, nrounds = 20)






predict_test <- predict(rf_cmod, md_test)
confusionMatrix(reference = md_test$miridae, data = predict_test, mode = 'everything')


##Consider this later
lm.test <- predict(lmfit,testing)
robust.test <- predict(robustfit,testing)
rf.test <- predict(rffit,testing)
xgb.test <- predict(xgbfit,testing)
svm.test <- predict(svmfit,testing)
train.results <- rbind(
  "LM"=postResample(pred=lm.test,obs=testing$price),
  "Robust"=postResample(pred=robust.test,obs=testing$price),
  "Random Forest"=postResample(pred=rf.test,obs=testing$price),
  "SVM"=postResample(pred=svm.test,obs=testing$price),
  "xgbTree"=postResample(pred=xgb.test,obs=testing$price)
)
print(train.results)