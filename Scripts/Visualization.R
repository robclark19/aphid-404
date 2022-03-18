

library(tidyverse)
library(ggforce)
library(esquisse)


#Bring in data
#For file path: laptop = megbl, office = megan.blance
vegFile <- "C:/Users/megan.blance/OneDrive/Documents/Grad School/MS Research/MIAphids/aphid-404/Data/transect_vegetation_data_2022-01-25_cleaned.csv"
vegData <- read.csv(vegFile)

bycatchFile <- "C:/Users/megan.blance/OneDrive/Documents/Grad School/MS Research/MIAphids/Raw Data/raw_insect_bycatch_2022-02-28.csv"
bycatchData <- read.csv(bycatchFile)
bycatchData[is.na(bycatchData)] <- 0

aphidFile <- "C:/Users/megbl/OneDrive/Documents/Grad School/MS Research/MIAphids/Raw Data/raw_aphid_data_2022-02-26.csv"
aphidData <- read.csv(aphidFile)
aphidData[is.na(aphidData)] <- 0


#add ag or natural
natural <- c('Steffan', 'Rose Creek', 'Smoot Hill', 'Philips', 'Idler', 'Magpie', 'Steptoe', 
             'PCEI', 'Laird', 'Hells Gate', 'Wawawai', 'JW Trail', 'Skinner', 'Asotin', 'Robinson', 'WSU Arboretum')
ag <- c('Clark', 'Kambitsch', 'Petty', 'Erikson', 'HagenZ', 'Wolf', 'Esser', 'Schuster',
        'Meyer', 'Greene', 'Schultheis', 'Schlee', 'Aeschliman', 'Anatone', 'Farmington', 'Spillman')


#Add a richness column & site type to vegData, need to create the richness df in diversity metrics script
richness_veg_df <- set_names(richness_veg_df, 'richness')
bycatchData <- bycatchData %>%
  vegData <- vegData %>%
  cbind(richness = richness_veg_df[1]) %>%
  relocate('richness', .after = 'aphid_presence') %>%
  add_column(site_type = if_else(.$site %in% natural, 'natural', 'agricultural'), .after = 'sample_period')

#Add richness column/site type to bycatchData
richness_bycatch_df <- set_names(richness_bycatch_df, 'richness')
bycatchData <- bycatchData %>%
  cbind(richness = richness_bycatch_df[1]) %>%
  add_column(site_type = if_else(.$site %in% natural, 'natural', 'agricultural'), .after = 'sampling_period')

#Distribution of transect richness according to site type
ggplot(vegData, aes(site_type, richness)) +
  geom_violin(draw_quantiles = TRUE) + 
  geom_boxplot(width = 0.1) +
  labs(title = "Transect Richness")

#Average transect richness by site df
site_richness_group <- vegData %>%
  group_by(site) %>%
  summarise_at(vars(richness), list(avg_richness = mean)) %>%
  add_column(site_type = if_else(.$site %in% natural, 'natural', 'agricultural'))

#Plot avg transect richness
ggplot(site_richness_group, aes(x = reorder(site, avg_richness), y = avg_richness, fill = site_type)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Site Average Transect Richness')

#Summarize full site data, get richness
onevegData <- vegData %>%
  group_by(site) %>%
  # across() applies a function  across multiple columns
  summarise(across(.cols = bromus.inermis:Dainty.TBD,
                   # na.rm = TRUE is one of the arguments to sum()
                   .fns = sum, na.rm = TRUE))
richness_onevegData <- specnumber(onevegData)
richness_onevegData <- richness_onevegData - 1 #need to subtract 1 to account for the first column
onevegData <- add_column(onevegData, richness = richness_onevegData, 
                         site_type = if_else(onevegData$site %in% natural, 'natural', 'agricultural'), .after = 'site')

#Summarize bycatch data into 1 full richness per site
onebycData <- bycatchData %>%
  group_by(site) %>%
  # across() applies a function  across multiple columns
  summarise(across(.cols = Diptera:Psocoptera,
                   # na.rm = TRUE is one of the arguments to sum()
                   .fns = sum, na.rm = TRUE))
richness_onebycData <- specnumber(onebycData)
richness_onebycData <- richness_onebycData - 1
onebycData <- add_column(onebycData, richness = richness_onebycData, 
                         site_type = if_else(onebycData$site %in% natural, 'natural', 'agricultural'), .after = 'site')

#Plot site richness
ggplot(onevegData, aes(x = reorder(site, richness), y = richness, fill = site_type)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Site Richness')

#Plot grass abundances
grass_summary <- onevegData %>%
  summarise(across(.cols = bromus.inermis:Dainty.TBD,
                   .fns = sum)) %>%
  transpose(keep.names = 'species') %>%
  rename(sum = V1)
grass_summary <- grass_summary[-c(60:63),]
grass_summary_truncate20 <- grass_summary[1:20,]

ggplot(grass_summary_truncate20, aes(x = reorder(species, sum), y = sum)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Relative abundance of grass species top 20') +
  coord_flip()

##Plot grass abundances by site type
#Ag sites
grass_ag_summary <- grass_summary %>%
  subset(site_type == 'agricultural') %>%
  summarise(across(.cols = bromus.inermis:Dainty.TBD,
                   .fns = sum)) %>%
  select(-'site_type') %>%
  transpose(keep.names = 'species') %>%
  rename(sum = V1)
grass_ag_summary <- grass_ag_summary[-c(60:63),]
grass_ag_summary_trunc20 <- grass_ag_summary[order(-grass_ag_summary$sum),][1:20,]

ggplot(grass_ag_summary_trunc20, aes(x = reorder(species, sum), y = sum)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Top 20 grass species in agricultural sites') +
  coord_flip()
#Natural sites 
grass_nat_summary <- grass_summary %>%
  subset(site_type == 'natural') %>%
  summarise(across(.cols = bromus.inermis:Dainty.TBD,
                   .fns = sum)) %>%
  select(-'site_type') %>%
  transpose(keep.names = 'species') %>%
  rename(sum = V1)
grass_nat_summary <- grass_nat_summary[-c(60:63),]
grass_nat_summary_trunc20 <- grass_nat_summary[order(-grass_nat_summary$sum),][1:20,]

ggplot(grass_nat_summary_trunc20, aes(x = reorder(species, sum), y = sum)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Top 20 grass species in natural sites') +
  coord_flip()


##Visulize aphid data
singleAphidData <- aphidData %>%
  group_by(site) %>% 
  summarise(count = sum(count)) %>%
  add_column(site_type = if_else(.$site %in% natural, 'natural', 'agricultural'))
n_occurances <- count(aphidData, site)
singleAphidData <- left_join(singleAphidData, n_occurances, by = 'site')

ggplot(singleAphidData, aes(reorder(site, count), count)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Total Aphid Counts by Site & Site Type', subtitle = 'Numbers above bars represent number of transects that contributed to aphid count') +
  geom_text(aes(label = n), vjust = -0.3)

##Summarize bycatch data and plot
onebycatch <- bycatchData %>%
  summarise(across(.cols = Diptera:Psocoptera,
                   .fns = sum, na.rm = TRUE)) %>%
  transpose(keep.names = 'family') %>%
  rename(total = V1)
onebycatch_trunc <- onebycatch[order(-onebycatch$total),][1:35,]
ggplot(onebycatch_trunc, aes(reorder(family, total), total)) +
  geom_col() +
  coord_flip() +
  labs(title = "Data Subset of Bycatch Family Totals")

##Site lm bycatch
site_bycatch <- bycatchData %>%
  group_by(site) %>%
  summarise(across(.cols = Diptera:Psocoptera,
                   .fns = sum, na.rm = TRUE)) %>%
  add_column(site_type = if_else(.$site %in% natural, 'natural', 'agricultural'))
  
lm(Ciccadellidae ~ site_type, data = site_bycatch)

