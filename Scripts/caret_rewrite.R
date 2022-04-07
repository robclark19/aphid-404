library(caret)
library(tidyverse)
library(corrplot)
library(randomForest)
library(lubridate)
library(xgboost)
library(elasticnet)
library(janitor)


# Load data ---------------------------------------------------------------

vegData <- read.csv("./Data/transect_vegetation_data_2022-03-17_cleaned.csv")
bycatchData <- read.csv("./Data/insect_bycatch_2022-03-17_cleaned.csv",
                        fileEncoding="UTF-8-BOM")
full_model_data <- read.csv("./Data/insect_veg_combined_2022-03-17.csv")

# Insects to model
insects_interest <- c("ciccadellidae", "miridae", "melyridae",
                      'delphacidae', 'aphididae', 'nabidae', 'coccinellidae')


# 1. Regression models ----------------------------------------------------

# Pull out the insect columns needed
rbug_data <- bycatchData %>%
  dplyr::select('site', 'transect', 'sampling_period', all_of(insects_interest))

# Join to the predictors
rmodel_data <- vegData %>%
  full_join(rbug_data, by = c("sampling_period", "site", "transect")) 

# Remove plants with a sum less than 100 (for now; there's sometimes columns
# that are entirely 0 during resampling)
less_than_100 <- colSums(select(vegData, where(is.numeric))) %>%
  data.frame() %>%
  rownames_to_column() %>%
  rename(plant = 1, sum = 2) %>%
  filter(sum <= 100) %>%
  pull(plant)

rmodel_data_100 <- rmodel_data %>%
  select(-all_of(less_than_100))

# Create a training and test dataset
set.seed(7)
train_indices <- sample(x = 1:nrow(rmodel_data_100),
                        size = 0.7*nrow(rmodel_data_100),
                        replace = FALSE)

rmd_train <- rmodel_data_100[train_indices, ] %>%
  select(-c(date, site, transect, sampling_period))

rmd_test <- rmodel_data_100[-train_indices, ] %>%
  select(-c(date, site, transect, sampling_period))

# ML params
train_control <- trainControl(method = "cv", number = 10, p = 0.7)


# 1.1 Random forests ------------------------------------------------------

# Iterate over insects, running random forests
random_forest_outputs <- map(.x = insects_interest,
                             .f = ~ {
                               
                               # Formula using all cols as predictors
                               ml_formula <- as.formula(paste0(.x, "~", "."))
                               
                               # Drop insect cols that aren't being modeled
                               cols_to_drop <- insects_interest[!grepl(pattern = .x, x = insects_interest)]
                               
                               train_selection <- select(rmd_train, -cols_to_drop) 
                               
                               # Random Forest 
                               set.seed(7)
                               rf_rmod <- train(form = ml_formula,
                                                data = train_selection,
                                                method="rf",
                                                trControl=train_control)
                               
                               # RF prediction
                               rf_rfit <- predict(rf_rmod)
                               
                               # Predictions vs observed
                               rf_predict_plot <- ggplot() +
                                 geom_point(aes(x = train_selection[, .x], y = rf_rfit)) +
                                 geom_abline(slope = 1, intercept = 0,
                                             linetype = "dashed") +
                                 xlab(make_clean_names(.x, case = "sentence")) +
                                 ylab("RF Prediction") +
                                 theme_bw()
                               
                               # Return model with plot
                               return(
                                 list(
                                   model = rf_rmod,
                                   plot = rf_predict_plot)
                               )
                               
                             })

# Transpose into a list with two sub-lists: model info and plots
transposed_rf_ouputs <- random_forest_outputs %>%
  set_names(nm = insects_interest) %>%
  transpose()

# Extract plots
rf_plots <- transposed_rf_ouputs$plot

# Create a data frame with model outputs
rf_result_table <- map2_df(.x = transposed_rf_ouputs$model,
                           .y = names(transposed_rf_ouputs$model),
                           .f = ~ .x$results %>%
                             mutate(insect = .y,
                                    model_type = "RF"))


# 1.2 Ridge regression ----------------------------------------------------

# Iterate over insects, running ridge regression
ridge_outputs <- map(.x = insects_interest,
                     .f = ~ {
                       
                       # Formula using all cols as predictors
                       ml_formula <- as.formula(paste0(.x, "~", "."))
                       
                       # Drop insect cols that aren't being modeled
                       cols_to_drop <- insects_interest[!grepl(pattern = .x, x = insects_interest)]
                       
                       train_selection <- select(rmd_train, -cols_to_drop) 
                       
                       ### Ridge Regression
                       set.seed(7)
                       ridge_rmod <- train(form = ml_formula,
                                           data = rmd_train,
                                           method = 'glmnet', 
                                           tuneGrid = expand.grid(alpha = 0,
                                                                  lambda = 1))
                       
                       # Ridge prediction
                       ridge_rfit <- predict(ridge_rmod)
                       
                       # Predictions vs observed
                       ridge_predict_plot <- ggplot() +
                         geom_point(aes(x = train_selection[, .x], y = ridge_rfit)) +
                         geom_abline(slope = 1, intercept = 0,
                                     linetype = "dashed") +
                         xlab(make_clean_names(.x, case = "sentence")) +
                         ylab("Ridge Prediction") +
                         theme_bw()
                       
                       # Return model with plot
                       return(
                         list(
                           model = ridge_rmod,
                           plot = ridge_predict_plot)
                       )
                       
                     })

# Transpose into a list with two sub-lists: model info and plots
transposed_ridge_ouputs <- ridge_outputs %>%
  set_names(nm = insects_interest) %>%
  transpose()

# Extract plots
ridge_plots <- transposed_ridge_ouputs$plot

# Create a data frame with model outputs
ridge_result_table <- map2_df(.x = transposed_ridge_ouputs$model,
                              .y = names(transposed_ridge_ouputs$model),
                              .f = ~ .x$results %>%
                                mutate(insect = .y,
                                       model_type = "ridge"))


# 1.3 GBMs ----------------------------------------------------------------

# Iterate over insects, running Gradient Boosted Model
gbm_outputs <- map(.x = insects_interest,
                   .f = ~ {
                     
                     # Formula using all cols as predictors
                     ml_formula <- as.formula(paste0(.x, "~", "."))
                     
                     # Drop insect cols that aren't being modeled
                     cols_to_drop <- insects_interest[!grepl(pattern = .x, x = insects_interest)]
                     
                     train_selection <- select(rmd_train, -cols_to_drop) 
                     
                     ### GBM
                     set.seed(7)
                     gbm_rmod <- train(form = ml_formula,
                                       data = train_selection,
                                       method="xgbTree",
                                       trControl=train_control)
                     
                     # GBM prediction
                     gbm_rfit <- predict(gbm_rmod)
                     
                     # Predictions vs observed
                     gbm_predict_plot <- ggplot() +
                       geom_point(aes(x = train_selection[, .x], y = gbm_rfit)) +
                       geom_abline(slope = 1, intercept = 0,
                                   linetype = "dashed") +
                       xlab(make_clean_names(.x, case = "sentence")) +
                       ylab("GBM Prediction") +
                       theme_bw()
                     
                     # Return model with plot
                     return(
                       list(
                         model = gbm_rmod,
                         plot = gbm_predict_plot)
                     )
                     
                   })

# Transpose into a list with two sub-lists: model info and plots
transposed_gbm_ouputs <- gbm_outputs %>%
  set_names(nm = insects_interest) %>%
  transpose()

# Extract plots
gbm_plots <- transposed_gbm_ouputs$plot

# Create a data frame with model outputs
gbm_result_table <- map2_df(.x = transposed_gbm_ouputs$model,
                            .y = names(transposed_gbm_ouputs$model),
                            .f = ~ .x$results %>%
                              mutate(insect = .y,
                                     model_type = "GBM"))


# 2. Classification Models-------------------------------------------------------

# Pull out the insect columns needed and change to presence/absence
cbug_data <- bycatchData %>%
  dplyr::select('site', 'transect', 'sampling_period', all_of(insects_interest)) %>%
  dplyr::mutate_at(insects_interest, ~1 * (. != 0)) %>%
  dplyr::mutate_at(insects_interest, as.factor)

# Join to the predictors
cmodel_data <- vegData %>%
  full_join(cbug_data, by = c("sampling_period", "site", "transect")) 

# Remove plants with a sum less than 100 (for now; there's sometimes columns
# that are entirely 0 during resampling)
less_than_100 <- colSums(select(vegData, where(is.numeric))) %>%
  data.frame() %>%
  rownames_to_column() %>%
  rename(plant = 1, sum = 2) %>%
  filter(sum <= 100) %>%
  pull(plant)

cmodel_data_100 <- cmodel_data %>%
  select(-all_of(less_than_100))

# Create a training and test dataset
set.seed(7)
train_indices <- sample(x = 1:nrow(cmodel_data_100),
                        size = 0.7*nrow(cmodel_data_100),
                        replace = FALSE)

cmd_train <- cmodel_data_100[train_indices, ] %>%
  select(-c(date, site, transect, sampling_period))

cmd_test <- cmodel_data_100[-train_indices, ] %>%
  select(-c(date, site, transect, sampling_period))

# ML params
train_control <- trainControl(method = "cv", number = 10, p = 0.7)

# 2.1 Random forests ------------------------------------------------------

# Iterate over insects, running random forests
random_forest_outputs <- map(.x = insects_interest[1],
                             .f = ~ {
                               
                               # Formula using all cols as predictors
                               ml_formula <- as.formula(paste0(.x, "~", "."))
                               
                               # Drop insect cols that aren't being modeled
                               cols_to_drop <- insects_interest[!grepl(pattern = .x, x = insects_interest)]
                               
                               train_selection <- select(cmd_train, -cols_to_drop)
                               
                               # Random Forest 
                               set.seed(7)
                               rf_cmod <- train(form = ml_formula,
                                                data = train_selection,
                                                method="rf",
                                                trControl=train_control)
                               
                               # RF prediction
                               rf_cfit <- predict(rf_cmod)
                               
                               # Predictions vs observed
                               rf_confusion_matrix <- confusionMatrix(reference = cmd_train$.x,
                                                                   data = rf_cfit, mode = 'everything')
                               

                               # Return model with plot
                               return(
                                 list(
                                   model = rf_rmod,
                                   confusion = rf_confusion_matrix)
                               )
                               
                             })

# Transpose into a list with two sub-lists: model info and plots
transposed_rf_ouputs <- random_forest_outputs %>%
  set_names(nm = insects_interest) %>%
  transpose()

# Extract plots
rf_plots <- transposed_rf_ouputs$plot

# Create a data frame with model outputs
rf_result_table <- map2_df(.x = transposed_rf_ouputs$model,
                           .y = names(transposed_rf_ouputs$model),
                           .f = ~ .x$results %>%
                             mutate(insect = .y,
                                    model_type = "RF"))


# 2.2 Ridge regression ----------------------------------------------------

# Iterate over insects, running ridge regression
ridge_outputs <- map(.x = insects_interest,
                     .f = ~ {
                       
                       # Formula using all cols as predictors
                       ml_formula <- as.formula(paste0(.x, "~", "."))
                       
                       # Drop insect cols that aren't being modeled
                       cols_to_drop <- insects_interest[!grepl(pattern = .x, x = insects_interest)]
                       
                       train_selection <- select(cmd_train, -cols_to_drop)
                       
                       ### Ridge Regression
                       set.seed(7)
                       ridge_cmod <- train(form = ml_formula,
                                           data = train_selection,
                                           method = 'glmnet', 
                                           tuneGrid = expand.grid(alpha = 0,
                                                                  lambda = 1))
                       
                       # Ridge prediction
                       ridge_cfit <- predict(ridge_cmod)
                       
                       # Predictions vs observed
                       ridge_confusion_matrix <- confusionMatrix(reference = train_selection$.x, data = ridge_cfit, mode = 'everything')
                       
                       # Return model with plot
                       return(
                         list(
                           model = ridge_cmod,
                           confusion = ridge_confusion_matrix)
                       )
                       
                     })

# Transpose into a list with two sub-lists: model info and plots
transposed_ridge_ouputs <- ridge_outputs %>%
  set_names(nm = insects_interest) %>%
  transpose()

# Extract plots
ridge_plots <- transposed_ridge_ouputs$plot

# Create a data frame with model outputs
ridge_result_table <- map2_df(.x = transposed_ridge_ouputs$model,
                              .y = names(transposed_ridge_ouputs$model),
                              .f = ~ .x$results %>%
                                mutate(insect = .y,
                                       model_type = "ridge"))


# 2.3 GBMs ----------------------------------------------------------------

# Iterate over insects, running Gradient Boosted Model
gbm_outputs <- map(.x = insects_interest,
                   .f = ~ {
                     
                     # Formula using all cols as predictors
                     ml_formula <- as.formula(paste0(.x, "~", "."))
                     
                     # Drop insect cols that aren't being modeled
                     cols_to_drop <- insects_interest[!grepl(pattern = .x, x = insects_interest)]
                     
                     train_selection <- select(cmd_train, -cols_to_drop) 
                     
                     ### GBM
                     set.seed(7)
                     gbm_cmod <- train(form = ml_formula,
                                       data = train_selection,
                                       method="xgbTree",
                                       trControl=train_control)
                     
                     # GBM prediction
                     gbm_cfit <- predict(gbm_cmod)
                     
                     # Predictions vs observed
                   
                     
                     # Return model with plot
                     return(
                       list(
                         model = gbm_cmod)
                     )
                     
                   })

# Transpose into a list with two sub-lists: model info and plots
transposed_gbm_ouputs <- gbm_outputs %>%
  set_names(nm = insects_interest) %>%
  transpose()

# Extract plots
gbm_plots <- transposed_gbm_ouputs$plot

# Create a data frame with model outputs
gbm_result_table <- map2_df(.x = transposed_gbm_ouputs$model,
                            .y = names(transposed_gbm_ouputs$model),
                            .f = ~ .x$results %>%
                              mutate(insect = .y,
                                     model_type = "GBM"))



























