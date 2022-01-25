#Load packages
library(DataExplorer)
library(caret)
library(tidyverse)


#Read in data
vegFile <- "./Data/transect_vegetation_data_2021-11-10_cleaned.csv"
vegData <- read.csv(vegFile)

#Data explorer report saved in Figures folder
#Bromus inermis is only grass with less than 80% NA
# create_report(vegData)

###Condense data into 1 row transects
#works on one column
singlevegData <- vegData %>%
  group_by(date, site, transect) %>% 
  summarise(bromus.inermis = sum(bromus.inermis))

#Applies the above to entire df
singlevegData <- vegData %>%
  group_by(date, site, transect) %>%
  # across() applies a function  across multiple columns
  summarise(across(.cols = bromus.inermis:Dainty.TBD,
                   # na.rm = TRUE is one of the arguments to sum()
                   .fns = sum, na.rm = TRUE))

#Pull out sample period and aphid columns then remove the extra rows
minivegData <- vegData %>%
  select(date, transect, site, sample_period, aphid_presence) %>%
  na_if("") %>%
  na.omit

#combine those 2 columns with the vegetation data
interdf <- left_join(singlevegData, minivegData, by = c('date', 'transect', 'site'))
#move those columns before veg species
vegetData <- relocate(interdf, c('sample_period', 'aphid_presence'), .after = 'transect')


# write_csv(vegetData, 'transect_vegetation_data_2021-11-10_cleaned.csv')


# Preliminary ML algorithm selection #####


##Create a validation + training dataset
#Problems: bringing whole transects into the data rather than partials
#creatTimeSlices can be better for time series data over random

#import data
vegFile <- "./Data/transect_vegetation_data_2021-11-10_cleaned.csv"
vegData <- read.csv(vegFile)

vegetData <- vegData

# we dont want to use these non-plant variables
vegetData$date <- NULL
vegetData$site <- NULL
vegetData$transect <- NULL
vegetData$sample_period <- NULL

# select only the known data


# change to incidence based data rather coverage
vegetData <- vegetData %>% mutate_if(is.numeric, ~1 * (. != 0)) %>% as.data.frame()

vegetData$bromus.inermis <- as.factor(vegetData$bromus.inermis) 


trainIndex <- createDataPartition(vegetData$bromus.inermis, p = 0.7, list = FALSE)
validData <- vegetData[-trainIndex,]
trainData <- vegetData[trainIndex,]



# view dimensions of the dataset
dim(trainData)
dim(validData)
# view the types of attributes
sapply(trainData, class)

# set control and metric settings
control <- trainControl(method="cv", number = 10)
metric <- "Accuracy"

# the goal here is to compare 5 algorithms for accuracy (using 10-fold cross validation)

# linear LDA
# a) linear algorithms. linear algorithms do not run due to highly non-linear structure
# set.seed(7)
# fit.lda <- train(aphid_presence~., data=trainData, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(bromus.inermis~., data=trainData, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(bromus.inermis~., data=trainData, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(bromus.inermis~., data=trainData, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(bromus.inermis~., data=trainData, method="rf", metric=metric, trControl=control)


# evaluate models
# summarize accuracy of models
results <- resamples(list(cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
dotplot(results)

# only cart is worse than the rest, which are all 91%


# estimate skill of knn on the validation dataset by making a predictions set
predictions <- predict(fit.svm, validData)

# compare predictions set to the validation set (make aphid_presence a factor)
confusionMatrix(predictions, as.factor(validData$bromus.inermis))


