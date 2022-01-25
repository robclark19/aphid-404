library(caret)
library(tidyverse)


vegFile <- "C:/Users/megbl/OneDrive/Documents/Grad School/MS Research/MIAphids/aphid-404/Data/transect_vegetation_data_2021-11-10_cleaned.csv"
vegData <- read.csv(vegFile)
complete.cases(vegData) #No NA found

#Sad plots
ggplot(data = vegData, aes(bromus.inermis)) +
  geom_histogram()



##Create a validation + training dataset
#creatTimeSlices can be better for time series data over random. 
#kfolds?
trainIndex <- createDataPartition(vegData$aphid_presence, p = 0.7, list = FALSE)
validData <- vegData[-trainIndex,]
trainData <- vegData[trainIndex,]

#Define model tuning parameters


##Train model


##Evaluate model
