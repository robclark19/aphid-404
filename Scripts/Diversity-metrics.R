#Currently calculate richness, shannon, evar

library(vegan)
library(tidyverse)
library(EcoIndR)

#load data

vegFile <- "./Data/transect_vegetation_data_2022-03-17_cleaned.csv"
vegData <- read.csv(vegFile)

bycatchFile <- "./Data/insect_bycatch_2022-03-17_cleaned.csv"
bycatchData <- read.csv(bycatchFile, fileEncoding="UTF-8-BOM")

metrics <- read.csv("./Data/diversity_metrics_2022-03-24.csv")

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
richness_veg_df <- as.data.frame(richness_veg <- specnumber(vegan_vegData))
richness_bycatch_df <- as.data.frame(richness_bycatch <- specnumber(vegan_bycatchData))
richness_merge <- richness_bycatch_df %>%
  merge(richness_veg_df, by = 'row.names', all.x = TRUE) %>%
  set_names(c('transect', 'bycatch_richness', 'veg_richness'))
#richLM <- lm(bycatch_richness~veg_richness, data = richness_merge)
#plot(richLM)


#shannon index with merge into 1 df
shannon_veg_df <- as.data.frame(shannon_veg <- diversity(vegan_vegData))
shannon_bycatch_df <- as.data.frame(shannon_bycatch <- diversity(vegan_bycatchData))
shannon_merge <- shannon_bycatch_df %>%
  merge(shannon_veg_df, by = 'row.names', all.x = TRUE) %>%
  set_names(c('transect', 'bycatch_shannon', 'veg_shannon'))

#Merge shannon and richness
metrics <- merge(shannon_merge, richness_merge, by = 'transect')


#Evar from https://github.com/ibartomeus/fundiv/blob/master/R/Evenness.R
Evar <- function(A){
  v <- rep(NA, nrow(A)) 
  for(k in 1:nrow(A)) {
    a <- rep(NA, ncol(A)) 
    b <- a  
    d <- a
    for(i in 1:ncol(A)) {
      a[i] <- if(A[k,i] != 0) log(A[k,i]) else 0 
    }
    S <- sum(A[k,] > 0)
    for(i in 1:ncol(A)) {
      b[i] <- a[i]/S
    }
    c <- sum(b)
    for(i in 1:ncol(A)) {
      d[i] <- if(A[k,i] !=0) (a[i]-c)^2/S else 0 
    }
    f <- sum(d)
    v[k] <- (1-2/pi*atan(f))   
  }
  v 
}

#calculate evar for each and join to metrics
evar_veg <- as.data.frame(Evar(vegan_vegData))
evar_bycatch <- as.data.frame(Evar(vegan_bycatchData))
evar_merge <- cbind(evar_bycatch, evar_veg)
evar_merge <- set_names(evar_merge, c('bycatch_evar', 'veg_evar'))
metrics <- cbind(metrics, evar_merge)

#write_csv(metrics, "diversity_metrics_2022-03-24.csv")

#Expand the site names once again. Thanks Matt Brousil
metrics_test <- metrics %>%
  mutate(site = str_extract(string = transect,
                            # Pattern: Any number of characters
                            # followed by, but not including, T[number]S[number],
                            # with T and S being either upper or lower case
                            pattern = ".*(?=[tT][1-6][sS][1-3])"),
         transect_num = str_extract(string = transect,
                                    # Upper/lowercase T[number] followed by,
                                    # but not including, upper/lower S[number]
                                    pattern = "[1-6](?=[s,S][1-3])"),
         sampling_period = str_extract(string = transect,
                                       # Upper/lower S[number] at the end of
                                       # the string
                                       pattern = "[0-3]$"))


##EcoIndR playing
DER(vegData)

richness_bycatch <- specnumber(vegan_bycatchData)
plot(richness_bycatch~bycatch_evar)
plot(richness_veg~veg_evar)
