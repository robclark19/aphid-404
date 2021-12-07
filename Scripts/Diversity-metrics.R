library(vegan)
library(tidyverse)

#load data
vegFile <- "C:/Users/megbl/OneDrive/Documents/Grad School/MS Research/MIAphids/aphid-404/Data/transect_vegetation_data_2021-11-10_cleaned.csv"
vegData <- read.csv(vegFile)

bycatchFile <- "C:/Users/megbl/OneDrive/Documents/Grad School/MS Research/MIAphids/Data/raw_insect_bycatch_2021-11-15.csv"
bycatchData <- read.csv(bycatchFile)

#make data vegan friendly
#need to remove all columns except the species + make the row names the transect names
vegan_vegData <- vegData %>%
  unite(col = transect, site, transect, sep = 'T') %>%
  unite(col = transect, transect, sample_period, sep = '')
rownames(vegan_vegData) <- vegan_vegData[,2] #rownames_to_column to go back
vegan_vegData <- select(vegan_vegData, -c('date', 'aphid_presence', 'transect'))

vegan_bycatchData <- bycatchData %>%
  unite(col = transect, site, transect, sep = 'T') %>%
  unite(col = transect, transect, sampling_period, sep = 'S')
vegan_bycatchData[is.na(vegan_bycatchData)] <- 0
rownames(vegan_bycatchData) <- vegan_bycatchData[,2]
vegan_bycatchData <- select(vegan_bycatchData, -c('date', 'transect'))

#richness calculation with merge into 1 df
#Fix problem where none of the bycatch merges in, only NA
richness_veg_df <- as.data.frame(richness_veg <- specnumber(vegan_vegData))
richness_bycatch_df <- as.data.frame(richness_bycatch <- specnumber(vegan_bycatchData))

richness_merge <- merge(richness_bycatch_df, richness_veg_df, by = 'row.names', all.x = TRUE)
#richness_merge <- set_names(richness_merge, c('transect', 'bycatch_richness', 'veg_richness'))

#shannon index with merge into 1 df
shannon_veg_df <- as.data.frame(shannon_veg <- diversity(vegan_vegData))
shannon_bycatch_df <- as.data.frame(shannon_bycatch <- diversity(vegan_bycatchData))
shannon_merge <- merge(shannon_bycatch_df, shannon_veg_df, by = 'row.names', all.x = TRUE)

#Full merge into 1 dataframe
diversity_metrics <- merge(shannon_merge, diversity_metrics, by = 'Row.names')



#terrible. Group entire sites together for better
jaccard_veg <- vegdist(vegan_vegData, 'jaccard')

##Consider: attach(). Attach management practices to link this with the vegan dataset
#http://traits-dgs.nceas.ucsb.edu/workspace/r/r-tutorial-for-measuring-species-diversity/Measuring%20Diversity%20in%20R.pdf/attachment_download/file

