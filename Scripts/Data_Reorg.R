#Load packages
library(DataExplorer)
library(caret)
library(tidyverse)


#Read in data
vegFile <- "C:/Users/megbl/OneDrive/Documents/Grad School/MS Research/MIAphids/Data/raw_transect_vegetation_data_2021.csv"
vegData <- read.csv(vegFile)

#Data explorer report saved in Figures folder
#Bromus inermis is only grass with less than 80% NA
create_report(vegData)

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

write_csv(vegetData, 'transect_vegetation_data_2021-11-10_cleaned.csv')


##Create a validation + training dataset
#Problems: bringing whole transects into the data rather than partials
#creatTimeSlices can be better for time series data over random
trainIndex <- createDataPartition(vegData$aphid_presence, p = 0.7, list = FALSE)
validData <- vegData[-trainIndex,]
trainData <- vegData[trainIndex,]

