library(caret)
library(tidyverse)
library(tidymodels)
library(corrplot)
library(randomForest)
library(ranger)
library(lubridate)
library(xgboost)


vegData <- read.csv("./Data/transect_vegetation_data_2022-03-17_cleaned.csv")
bycatchData <- read.csv("./Data/insect_bycatch_2022-03-17_cleaned.csv", fileEncoding="UTF-8-BOM")
full_model_data <- read.csv("./Data/insect_veg_combined_2022-03-17.csv")

##Tidymodels help: https://www.tidymodels.org/start/

#Top 10 grasses by abundance
sort(colSums(vegData))
response_vars <- c("bromus.inermis", "thinopyrum.intermedium.ssp.intermedium", "alopecurus.pratensis",
                   "arrhenatherum.elatius", "phalaris.arundinacea", "bromus.tectorum", "pseudoroegneria.spicata",
                   "secale.cereale", "poa.pratensis", "elymus.repens")
#Most common grass for each transect into unique list
most <- unique(colnames(vegData)[max.col(vegData, ties.method = 'first')])
most2 <- unique(colnames(vegData)[max.col(vegData, ties.method = 'last')])
#response_vars <- unique(append(most, most2))

##### Megan Basic ML Algs ---------------------------------------------------
##Need to change interest insect in 3 places: bug_data, md_split, ml_formula
insects_interest <- c("ciccadellidae", "miridae", "melyridae",
                      'delphacidae', 'aphididae', 'nabidae', 'coccinellidae')


### Modeling with tidymodels/parsnip #####################


###
###  1  modeling with all veg, abundance based
###
#Create the model data with the veg and insect, can't keep all the bugs in until we look at specific response variables
##Modifying data as needed
bug_data <- bycatchData %>%
  select('site', 'transect', 'sampling_period', insects_interest[1])
model_data <- vegData %>%
  full_join(bug_data, by = c("sampling_period", "site","transect")) %>%
  mutate(
    sampling_period = as.factor(.$sampling_period),
    date = mdy(.$date))

#Stratified data split
md_split <- initial_split(model_data, strata = insects_interest[1])
md_train <- training(md_split)
md_test <- testing(md_split)
#Cross fold validation, 10 folds
set.seed(7)
folds <- vfold_cv(md_train, v = 10)
#Formula uses all other columns as predictors
ml_formula <- as.formula(paste0(insects_interest[1],
                                "~", "."))

##Make the recipe for the model
#role leaves data in but tells model not to use it
all_veg_recipe <- 
  recipe(ml_formula, data = md_train) %>%
  update_role(transect, site, date, sampling_period, new_role = "ID")

#####Random Forest model
rf_rmod <- rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")
#Make the RF workflow
rf_rwf <- 
  workflow() %>%
  add_model(rf_rmod) %>%
  add_formula(ml_formula)
#Fit the RF
set.seed(7)
rf_rfit <- rf_rwf %>%
  fit_resamples(folds) 
##Show accuracy
collect_metrics(rf_rfit)


####Boosted Tree Model
boo_rmod <- boost_tree(trees = 15) %>%
  set_engine('xgboost') %>%
  set_mode('regression') 
set.seed(7)
boo_rfit <- boo_rmod %>%
  fit(ml_formula, data = md_train)
boo_rfit


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

#Stratified data split
md_split <- initial_split(model_data, strata = insects_interest[1])
md_train <- training(md_split)
md_test <- testing(md_split)
#Cross fold validation, 10 folds
set.seed(7)
folds <- vfold_cv(md_train, v = 10)
#Formula uses all other columns as predictors
ml_formula <- as.formula(paste0(insects_interest[1],
                                "~", "."))

##Make the recipe for the model
#role leaves data in but tells model not to use it
all_veg_recipe <- 
  recipe(ml_formula, data = md_train) %>%
  update_role(transect, site, date, sampling_period, new_role = "ID")

#####Random Forest model
rf_cmod <- rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")
#Make the RF workflow
rf_cwf <- 
  workflow() %>%
  add_model(rf_cmod) %>%
  add_formula(ml_formula)
#Fit the RF
set.seed(7)
rf_cfit <- rf_cwf %>%
  fit_resamples(folds)
rf_cfit
##Show accuracy
collect_metrics(rf_cfit)


####Boosted Tree Model 
boo_cmod <- boost_tree(trees = 15) %>%
  set_engine('xgboost',event_level = 'second') %>%
  set_mode('classification') 
set.seed(7)
boo_cfit <- boo_cmod %>%
  fit(ml_formula, data = md_train)
boo_cfit
bind_cols(
  predict(boo_cfit, md_test),
  predict(boo_cfit, md_test, type = 'prob'))
