
##Use this to take raw data to cleaned
#First section cleans veg data
#Second merges veg and insect

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

### Merging veg + insect ----------------------

insect <- read_csv(file = "./Data/insect_bycatch_2022-03-17_cleaned.csv")

veg <- read_csv(file = "./Data/transect_vegetation_data_2022-03-17_cleaned.csv")

# Check that things line up
unique(insect$site)[!(unique(insect$site) %in% unique(veg$site))]
unique(veg$site)[!(unique(veg$site) %in% unique(insect$site))]

anti_join(
  x = insect,
  y = veg,
  by = c("date", "site", "transect", "sampling_period")
)

anti_join(
  x = veg,
  y = insect,
  by = c("date", "site", "transect", "sampling_period")
)

sampling_data <- inner_join(
  x = insect,
  y = veg,
  by = c("date", "site", "transect", "sampling_period")
)
#write_csv(sampling_data, "insect_veg_combined_2022-03-17.csv")
