library(caret)
library(tidyverse)
library(tidymodels)
library(corrplot)


vegFile <- "./Data/transect_vegetation_data_2022-03-17_cleaned.csv"
vegData <- read.csv(vegFile)
#complete.cases(vegData) #No NA found

bycatchFile <- "./Data/insect_bycatch_2022-03-17_cleaned.csv"
bycatchData <- read.csv(bycatchFile, fileEncoding="UTF-8-BOM")

model_data <- read.csv("./Data/insect_veg_combined_2022-03-17.csv")


# Preliminary ML algorithm selection #####


# we dont want to use these non-plant variables
vegData$date <- NULL
vegData$site <- NULL
vegData$transect <- NULL
vegData$sample_period <- NULL


bycatchData$date <- NULL
bycatchData$site <- NULL
bycatchData$transect <- NULL
bycatchData$sampling_period <- NULL
# select only the known data


sums_bycatch <- colSums(bycatchData)
paSums_bycatch <- colSums(bycatchData)

# change to incidence based data rather coverage
vegData <- vegData %>% mutate_if(is.numeric, ~1 * (. != 0))
bycatchData <- bycatchData %>% mutate_if(is.numeric, ~1 * (. != 0))


##Aphid ML ######
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



## Matt's consult ML stuff -----------------------------------------------------------

insects_interest <- c("ciccadellidae", "miridae", "melyridae",
                      'delphacidae', 'aphididae', 'nabidae', 'coccinellidae')

train_control <- trainControl(method = "cv", number = 10, p = 0.7)

response_vars <- c("pseudoroegneria.spicata",
                   "bromus.tectorum", "alopecurus.pratensis", "bromus.inermis", 
                   'thinopyrum.intermedium.ssp.intermedium', 'arrhenatherum.elatius')


model_data %>%
  dplyr::select(all_of(response_vars)) %>%
  cor() %>%
  corrplot(type = "upper")


ml_formula <- as.formula(paste0(insects_interest[4],
                                "~",
                                paste(response_vars, collapse = " + ")))


training_percentage <- 0.7
set.seed(7)
training_indices <- sample(x = 1:nrow(model_data),
                           size = training_percentage*nrow(model_data),
                           replace = FALSE)

set.seed(7)
rf_model <- train(form = ml_formula,
                  data = model_data,
                  method="rf",
                  trControl=train_control,
                  subset = training_indices)
rf_model


varImp(rf_model)

rf_importance <- varImp(rf_model) %>%
  pluck("importance") %>% 
  rownames_to_column() %>% 
  rename("variable" = rowname) %>% 
  arrange(Overall) %>%
  mutate(variable = forcats::fct_inorder(variable)) %>%
  rename(imp = Overall)


ggplot(rf_importance) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = imp), 
               size = 1, alpha = 0.7) +
  geom_point(aes(x = variable, y = imp), 
             size = 2, show.legend = F, color = "blue") +
  coord_flip() +
  xlab("Variable") +
  ylab("Importance") +
  theme_bw() +
  theme(text = element_text(size = 22),
        axis.text.y = element_text(margin = ggplot2::margin(r = 7)),
        panel.border = element_rect(fill = NA,
                                    colour = "black", 
                                    size = 1)) 

