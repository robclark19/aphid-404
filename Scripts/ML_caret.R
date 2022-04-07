library(caret)
library(tidyverse)
library(corrplot)
library(randomForest)
library(lubridate)
library(xgboost)
library(elasticnet)
library(glmnet)

vegData <- read.csv("./Data/transect_vegetation_data_2022-03-17_cleaned.csv")
bycatchData <- read.csv("./Data/insect_bycatch_2022-03-17_cleaned.csv", fileEncoding="UTF-8-BOM")
full_model_data <- read.csv("./Data/insect_veg_combined_2022-03-17.csv")

insects_interest <- c("ciccadellidae", "miridae", "melyridae",
                      'delphacidae', 'aphididae', 'nabidae', 'coccinellidae')

#dummy variable is used to create one-hot encoding AKA categorical column as a feature converted to numeric 0 or 1

##### Megan Basic ML Algs
#Listed in order of most to least transects with occurrence
#Need to change the call in 3 places: bug_data, ml_formula, and the name in md_split, and sometimes confusionMatrix name


###
###  1  modeling with all veg, abundance based ---------------------------------------------
###
#Create the model data with the veg and insect, can't keep all the bugs in until we look at specific response variables
##Modifying data as needed
rbug_data <- bycatchData %>%
  dplyr::select('site', 'transect', 'sampling_period', insects_interest[2])
rmodel_data <- vegData %>%
  dplyr::full_join(rbug_data, by = c("sampling_period", "site","transect")) %>%
  dplyr::mutate(
    sampling_period = as.factor(.$sampling_period),
    date = mdy(.$date))


#Formula uses all other columns as predictors
ml_formula <- as.formula(paste0(insects_interest[2],
                                "~", "."))
#Data split
#Need to split data AND do folding. The folding tests on all data so without it the model performs better due to viewing all datapoints
rmd_split <- createDataPartition(rmodel_data$miridae, p = 0.7, list = FALSE)
rmd_train <- rmodel_data[rmd_split, !(names(rmodel_data) %in% c('date', 'site', 'transect', 'sampling_period'))]
rmd_train_sub <- rmd_train %>% dplyr::select(which(!colSums(rmd_train, na.rm=TRUE) %in% 0)) 
rmd_test <- rmodel_data[-rmd_split, !(names(rmodel_data) %in% c('date', 'site', 'transect', 'sampling_period'))]



train_control <- trainControl(method = "cv", number = 10, p = 0.7)

#### RF Model
set.seed(7)
rf_rmod <- train(form = ml_formula,
                  data = rmd_train,
                  method="rf",
                  trControl=train_control)
rf_rfit <- predict(rf_rmod)
plot(rmd_train$miridae, rf_rfit)

### Ridge Regression: 2 options
set.seed(7)
rid_rmod <- train(form = ml_formula,
                data = rmd_train_sub,
                method="ridge",
                trControl=train_control)
rid_rfit <- predict(rid_rmod)
plot(rmd_train$miridae, rf_rfit)

#Glmnet with alpha = 0 is ridge and 1 is lasso
# set.seed(7)
# ridge_rmod <- train(form = ml_formula, data = rmd_train, method = 'glmnet', 
#                     tuneGrid = expand.grid(alpha = 0, lambda = 1))
#to find the best lambda later rather than 1 as full penalty: (0, 1, 0.1)
#ridge_rmse <- RMSE(ridge_fit, md_train$miridae)

####Gradient Boosted Model

set.seed(7)
boo_rmod <- train(form = ml_formula, data = rmd_train_sub, method = 'xgbTree', trControl = train_control)
boo_rfit <- predict(boo_rmod)
plot(rmd_train$miridae, boo_rfit)

#Compare models
#model_compare <- resamples(RF = rf_rmod, Boosted = boo_rmod, Ridge = rr_fit)
#summary(models_compare)


###
### 2  modeling with all veg, pres/abs based ---------------------------------------------
### all veg p/a or just bug? just the bug right now

#Create the model data with the veg and insect, can't keep all the bugs in until we look at specific response variables
##Modifying data as needed
##Need to make the presence/absence a factor to use classification
cbug_data <- bycatchData %>%
  dplyr::select('site', 'transect', 'sampling_period', insects_interest[2]) %>%
  dplyr::mutate_if(is.numeric, ~1 * (. != 0))

cmodel_data <- vegData %>%
  dplyr::full_join(cbug_data, by = c("sampling_period", "site","transect")) %>%
  dplyr::mutate(
    date = mdy(.$date))

#Formula uses all other columns as predictors
ml_formula <- as.formula(paste0(insects_interest[2],
                                "~", "."))
#Data split
cmd_split <- createDataPartition(cmodel_data$miridae, p = 0.7, list = FALSE)
cmd_train <- cmodel_data[cmd_split,!(names(cmodel_data) %in% c('date', 'site', 'transect', 'sampling_period'))]
cmd_train_sub <- cmd_train %>% dplyr::select(which(!colSums(cmd_train, na.rm=TRUE) %in% 0)) 
cmd_test <- cmodel_data[-cmd_split,!(names(cmodel_data) %in% c('date', 'site', 'transect', 'sampling_period'))]

train_control <- trainControl(method = "cv", number = 10, p = 0.7)

#####Random Forest model
set.seed(7)
rf_cmod <- train(form = ml_formula,
                 data = cmd_train,
                 method="rf",
                 trControl=train_control)
rf_cmod
rf_cfit <- predict(rf_cmod)
plot(cmd_train$miridae, rf_cfit)


##### Ridge regression
set.seed(7)
rid_cmod <- train(form = ml_formula, data = cmd_train, method = 'glmnet', tuneGrid = expand.grid(alpha = 0, lambda = 1))
rid_cfit <- predict(rid_cmod)
plot(cmd_train$miridae, rid_cfit)

####Gradient Boosted Model

set.seed(7)
boo_cmod <- train(form = ml_formula, data = cmd_train_sub, method = 'xgbTree', 
                  trControl = train_control)
boo_cfit <- predict(boo_cmod)
plot(cmd_train$miridae, boo_cfit)




##Confusion matrix for test data
predict_test <- predict(rf_cmod, cmd_test)
confusionMatrix(reference = cmd_test$miridae, data = predict_test, mode = 'everything')


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