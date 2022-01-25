
##Use this to take raw data to cleaned
#This file cleans the raw transect data only currently

#Load packages
library(DataExplorer)
library(tidyverse)


#Read in data
vegFile <- "C:/Users/megbl/OneDrive/Documents/Grad School/MS Research/MIAphids/aphid-404/Data/raw_transect_vegetation_data_2022-01-25.csv"
vegData <- read.csv(vegFile)

#Data explorer report saved in Figures folder
#Bromus inermis is only grass with less than 80% NA
# create_report(vegData)


#Turn the data into single row transects
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


#write_csv(vegetData, 'transect_vegetation_data_2022-01-25_cleaned.csv')
