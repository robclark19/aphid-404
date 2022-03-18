
library(vegan)
library(tidyverse)
library(EcoIndR)

#load data

vegFile <- "./Data/transect_vegetation_data_2022-03-17_cleaned.csv"
vegData <- read.csv(vegFile)

bycatchFile <- "./Data/insect_bycatch_2022-03-17_cleaned.csv"
bycatchData <- read.csv(bycatchFile, fileEncoding="UTF-8-BOM")

#make data vegan friendly
#need to remove all columns except the species + make the row names the transect names
vegan_vegData <- vegData %>%
  unite(col = transect, site, transect, sep = 'T') %>%
  unite(col = transect, transect, sampling_period, sep = 'S')
rownames(vegan_vegData) <- vegan_vegData[,2] #rownames_to_column to go back
vegan_vegData <- dplyr::select(vegan_vegData, -c('date', 'transect'))

vegan_bycatchData <- bycatchData %>%
  unite(col = transect, site, transect, sep = 'T') %>%
  unite(col = transect, transect, sampling_period, sep = 'S')
rownames(vegan_bycatchData) <- vegan_bycatchData[,2]
vegan_bycatchData <- dplyr::select(vegan_bycatchData, -c('date', 'transect'))


#richness calculation with merge into 1 df
#Fix problem where none of the bycatch merges in, only NA
richness_veg_df <- as.data.frame(richness_veg <- specnumber(vegan_vegData))
richness_bycatch_df <- as.data.frame(richness_bycatch <- specnumber(vegan_bycatchData))

richness_merge <- merge(richness_bycatch_df, richness_veg_df, by = 'row.names', all.x = TRUE)
richness_merge <- set_names(richness_merge, c('transect', 'bycatch_richness', 'veg_richness'))
richLM <- lm(bycatch_richness~veg_richness, data = richness_merge)
plot(richLM)


#shannon index with merge into 1 df
shannon_veg_df <- as.data.frame(shannon_veg <- diversity(vegan_vegData))
shannon_bycatch_df <- as.data.frame(shannon_bycatch <- diversity(vegan_bycatchData))
shannon_merge <- merge(shannon_bycatch_df, shannon_veg_df, by = 'row.names', all.x = TRUE)

#Full merge into 1 dataframe
diversity_metrics <- merge(shannon_merge, richness_merge, by = 'Row.names')
diversity_metrics <- set_names(diversity_metrics, c('transect', 'shannon_bycatch', 'shannon_veg', 'richness_bycatch', 'richness_veg'))

ggplot(diversity_metrics, aes(x = richness_bycatch)) +
  geom_bar()



##Consider: attach(). Attach management practices to link this with the vegan dataset
#http://traits-dgs.nceas.ucsb.edu/workspace/r/r-tutorial-for-measuring-species-diversity/Measuring%20Diversity%20in%20R.pdf/attachment_download/file


#Evar
##Elinor's via Eli non-looped function then with making it looped
Evarb <- function(x)
{
  #Remove taxa with zero abundance
  x1 <- data.frame(abundance=x[x>0])
  
  #Calculate
  S <- nrow(x1)
  x1$v1 <- log(x1$abundance)
  x1$v2 <- x1$v1/S
  s1 <- sum(x1$v2)
  x1$v3 <- ((x1$v1 - s1)^2)/S
  s2 <- sum(x1$v3)
  if(S>=1) evar <- (1 - 2/pi * atan(s2)) else evar <- NA
  
  return(evar)
}
evarbVVdata <- vegan_vegData
for (i in 1:nrow(evarbVVdata)) {
  evarbVVdata[i,] <- Evarb(evarbVVdata[i,])
  out <- evarbVVdata[,1]
}
evarbVVdata <- out

evar2VVdata <- Evar(vegan_vegData)

plot(evar2VVdata~evarbVVdata) #same


Evar(vegan_vegData)
evar_veg_df <- as.data.frame(evar_veg <- Evar(vegan_vegData))
evar2_veg_df <- as.data.frame(evar2_veg <- Evar2(vegan_vegData))
evar_merge <- merge(evar_veg_df, evar2_veg_df, by = 'row.names', all.x = TRUE)

evar2_bycatch_df <- as.data.frame(evar2_bycatch <- Evar2(vegan_bycatchData))
evar_merge <- merge(evar2_bycatch_df, evar2_veg_df, by = 'row.names', all.x = TRUE)
ggplot(evar_merge, aes(evar_merge[,2], evar_merge[,3])) +
  geom_point(position='jitter')


bycatch_evar <- vegan_bycatchData %>%
  Evar() %>%
  as.data.frame()
rownames(bycatch_evar) <- vegan_bycatchData[,2]
merge_test <- merge(bycatch_evar, vegan_bycatchData)

veg_evar <- Evar(vegan_vegData)



##EcoIndR playing
DER(vegData)

richness_bycatch <- specnumber(vegan_bycatchData)
plot(richness_bycatch~bycatch_evar)
plot(richness_veg~veg_evar)
