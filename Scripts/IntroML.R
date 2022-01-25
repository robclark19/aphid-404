library(caret)
library(tidyverse)


vegFile <- "./Data/transect_vegetation_data_2022_01_25_cleaned.csv"
vegData <- read.csv(vegFile)
complete.cases(vegData) #No NA found


# Preliminary ML algorithm selection #####


##Create a validation + training dataset
#Problems: bringing whole transects into the data rather than partials
#creatTimeSlices can be better for time series data over random

# we dont want to use these non-plant variables
vegetData$date <- NULL
vegetData$site <- NULL
vegetData$transect <- NULL
vegetData$sample_period <- NULL

# select only the known data


# change to incidence based data rather coverage
vegetData <- vegetData %>% mutate_if(is.numeric, ~1 * (. != 0))



trainIndex <- createDataPartition(vegData$aphid_presence, p = 0.8, list = FALSE)
validData <- vegData[-trainIndex,]
trainData <- vegData[trainIndex,]



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
fit.cart <- train(aphid_presence~., data=trainData, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(aphid_presence~., data=trainData, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(aphid_presence~., data=trainData, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(aphid_presence~., data=trainData, method="rf", metric=metric, trControl=control)


# evaluate models
# summarize accuracy of models
results <- resamples(list(cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
dotplot(results)

# only cart is worse than the rest, which are all 91%


# estimate skill of knn on the validation dataset by making a predictions set
predictions <- predict(fit.knn, validData)

# compare predictions set to the validation set (make aphid_presence a factor)
confusionMatrix(predictions, as.factor(validData$aphid_presence))


