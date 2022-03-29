
##Landscape Metrics package
##Code adapted(AKA stolen) from Vera


library(tidyverse)
library(sf)
library(terra)
library(dplyr)
library(spData)
library(CropScapeR)
library(raster)
library(landscapemetrics)


# Get lat longs

SiteMeta <- read.csv("C:/Users/megbl/OneDrive/Documents/Grad School/MS Research/MIAphids/aphid-404/Data/Site_centroidXY.csv")
coords <- SiteMeta[,c("X", "Y")]
colnames(coords) <- c("X", "Y")
SiteNames<-SiteMeta$site
SiteCodes<-SiteMeta$site


# Project and supply spatial metadata

sites = coords %>% 
  st_as_sf(coords = 1:2, crs = "EPSG:4326")

st_is_longlat(sites)
st_crs(sites)  # get CRS
st_crs(sites)$IsGeographic #TRUE <- unprojected

target_crs <- "EPSG:32611"

# These coordinates are in the USA Contiguous Albers Equal Area Conic projected CRS 
# and the EPSG code is 102003. It's important to use an equal area projection for area relevant calculations.

SitesProj = sf::st_transform(sites, target_crs)
st_crs(SitesProj)  # get CRS
st_crs(SitesProj)$IsGeographic #FALSE <- now it's projected

st_distance(SitesProj$geometry[1], SitesProj$geometry[2]) #note the projected coordinate system is in meters


# Make Buffers

#SiteBuffer_s2 = st_buffer(sites, dist = 1e4) # silent use of s2, like "projecting on the fly"
SiteBuffer_projected = st_buffer(SitesProj, 1e4) # explicit projected 10km buffer, better option


# Get croplands data
LandSectors <- as.list(rep(1,9,nrow(SiteBuffer_projected$geometry)))

for (i in 1:length(SiteBuffer_projected$geometry)) {
  data <- GetCDLData(aoi = SiteBuffer_projected$geometry[i], year = 2018, type = 'b')
  LandSectors[[i]] = data
  #raster::plot(LandSectors[[i]])
}

# Reclassify raster


plot(LandSectors[[1]])


unique(LandSectors[[1]])

SiteCodes[1]
Erikson = LandSectors[[1]]

check_landscape(Erikson)

CDL<-read.csv("C:/Users/megbl/OneDrive/Documents/Grad School/MS Research/MIAphids/aphid-404/Data/CDLcodes.csv")
RclTable<-CDL[, c("Codes", "NewValues")]

Eriksimp <- reclassify(Erikson, as.matrix(RclTable))
plot(Eriksimp)


# Visualization functions

Eriksimp  %>%
  show_cores() #show cores


# Check options for metrics

list_lsm(level = "patch") #see options
list_lsm(level = "class")   %>%  #see options
  print(n=Inf)
list_lsm(level = "landscape")  %>%  #see options
  print(n=Inf)


# Calculate a single metric
lsm_c_clumpy(Eriksimp)


# Calculate multiple metrics
metrics = calculate_lsm(Eriksimp, 
                        what = c("lsm_c_pland", "lsm_l_ta", "lsm_l_te"))

# Matt's addition ---------------------------------------------------------

ggplot() +
  geom_sf(data = rnaturalearth::ne_states(iso_a2 = "US", returnclass = "sf") %>%
            filter(name == "Washington")) +
  geom_sf(data = SiteBuffer_projected)


# Megan's custom classifications
code_table <- tribble(
  ~code,    ~label,
  0,        "Background",
  1,        "Non-cereal crops",
  2,        "Cereal crops",
  3,        "Forest and tree crops",
  4,        "Tree fruit crops",
  5,        "Development",
  6,        "Wetland/open water",
  7,        "Pasture or barren"
)


reclassify(LandSectors[[1]], as.matrix(RclTable))

classified_sites <- map2_df(.x = LandSectors,
                            .y = SiteMeta$site,
                            .f = ~{
                              
                              classified <- reclassify(.x, as.matrix(RclTable))
                              
                              
                              pland <- lsm_c_pland(classified) %>%
                                left_join(x = .,
                                          y = code_table,
                                          by = c("class" = "code")) %>%
                                mutate(site = .y)
                              
                              return(pland)
                              
                            })

land_percentage <- classified_sites %>%
  dplyr::select(site, label, value) %>%
  pivot_wider(names_from = "label", values_from = "value", values_fill = 0) %>%
  clean_names() %>%
  mutate(site = case_when(
    site == "Smoot" ~ "Smoot Hill",
    site == "JW trail" ~ "JW Trail",
    TRUE ~ site))

#write_csv(x = land_percentage, file = "site_landscape_percentages.csv")



