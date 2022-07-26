#CCAP intersections----------

#read in packages
library(sf)
library(dplyr)
library(CropScapeR)
library(ggspatial)
library(ggrepel)
library(tmaptools)
library(tigris)

##################### create key ###################################
# 1: Unclassified
# 2: High Intensity Developed
# 3: Medium Intensity Developed
# 4: Low Intensity Developed
# 5: Open Space Developed
# 6: Cultivated Land
# 7: Pasture/Hay
# 8: Grassland
# 9: Deciduous Forest
# 10: Evergreen Forest
# 11: Mixed Forest
# 12: Scrub/Shrub
# 13: Palustrine Forested Wetland
# 14: Palustrine Scrub/Shrub Wetland
# 15: Palustrine Emergent Wetland
# 16: Estuarine Forested Wetland
# 17: Estuarine Scrub/Shrub Wetland
# 18: Estuarine Emergent Wetland
# 19: Unconsolidated Shore
# 20: Bare Land
# 21: Water
# 22: Palustrine Aquatic Bed
# 23: Estuarine Aquatic Bed
# 24: Tundra
# 25: Snow/Ice

i <- c(1:25)
use <- c("Unclassified", "High Intensity Developed","Medium Intensity Developed", "Low Intensity Developed", "Open Space Developed",
         "Cultivated Land", "Pasture/Hay", "Grassland", "Deciduous Forest", "Evergreen Forest", "Mixed Forest", "Scrub/Shrub", "Palustrine Forested Wetland",
         "Palustrine Scrub/Shrub Wetland", "Palustrine Emergent Wetland", "Estuarine Forested Wetland", "Estuarine Scrub/Shrub Wetland",
         "Estuarine Emergent Wetland", "Unconsolidated Shore", "Bare Land", "Water", "Palustrine Aquatic Bed", "Estuarine Aquatic Bed", 
         "Tundra", "Snow/Ice")

Key <- data.frame(i, use)
saveRDS(Key, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/LanduseArea/Key.RDS")

##############################
#Method-----------
##############################

#1: sort by one landuse
#2: Run intersection between ESVA and CCAP 2016
#3: Run a st_union to get total area of land use for each parcel
#4: get area of each land use in one specifific parcel
#5: link together total area of parcels


##1: seperate out one landuse##

ccap_2_2016<- ccap2016%>% filter(landcover == 2)

##2: run intersection with ESVA and ccap_2_2016##

ccap_2_2016<- st_transform(ccap_2_2016, crs = st_crs(ESVAparcels_update))

sf::sf_use_s2(FALSE)

ESVA_2<-st_intersection(ccap_2_2016, st_make_valid(ESVAparcels_update))

saveRDS(ESVA_2, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/LanduseArea/ESVA_2.RDS")

##3: Run st_union to get total land use (2) for each parcel it is present in##

merge_ESVA_2<-ESVA_2%>%group_by(PARCELID)%>%summarize(two_geometry = st_union(geometry))

##4:parcel ID, area of parcel for two for each parcel##

two_parcel_area<- st_area(merge_ESVA_2)
merge_ESVA_2<-cbind(merge_ESVA_2, two_parcel_area)

##5: Link parcel area to merge_ESVA_2 data set##

#add area to Parcels_Accomack data and delete irrelevant columns
parcel_area<-st_area(ESVAparcels_update)
ESVAparcels<-cbind(ESVAparcels_update, parcel_area)
ESVAparcels<-ESVAparcels%>%select(LOCALITY, PARCELID, PTM_ID, parcel_area, geometry)

saveRDS(ESVAparcels, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/parceldata/ESVAparcels.RDS")

###Find the parcel area as a data.set and can attach this to the merge_corn_accomack and get percentages###

#make Parcel_Accomack as data.set so it can be joined to merge_corn_accomack
ESVAparcels_area<-st_set_geometry(ESVAparcels, NULL)
merge_ESVA_2<- left_join(merge_ESVA_2,ESVAparcels_area)

#add percentage column
merge_ESVA_2<-merge_ESVA_2%>%mutate(perc_area_two=(two_parcel_area/parcel_area)*100)

saveRDS(merge_ESVA_2, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/LanduseArea/merge_ESVA_2.RDS")


##############################################################################################################################################################

#load Parcels
ESVAparcels<- readRDS("/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/parceldata/ESVAparcels.RDS")

#load in key
Key<- readRDS("/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/LanduseArea/Key.RDS")   

#load in all CCAP data
ccap2016<- readRDS("/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CCAP/ccap2016.RDS")

ccap2010<- readRDS("/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CCAP/ccap2010.RDS")

ccap2006<- readRDS("/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CCAP/ccap2006.RDS")

ccap2001<- readRDS("/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CCAP/ccap2001.RDS")

ccap1996<- readRDS("/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CCAP/ccap1996.RDS")

#load in packages
library(sf)
library(dplyr)

##############################################################################################################################################################

# function -----------  

CCAP_Parcels <- function(CCAP){
  #change crs
  transform <- st_transform(CCAP, crs = st_crs(ESVAparcels))
  #create inputs for the for loop
  landuses <- transform %>% distinct(landcover)
  landuse <- landuses[['landcover']]
  datalist = list()
  for (i in landuse){
    sf::sf_use_s2(FALSE)
    allcover <- transform %>% filter(landcover == i)
    #run intersection for use and parcel
    sf::sf_use_s2(FALSE)
    intersect <- st_intersection(allcover, st_make_valid(ESVAparcels))
    #run union by parcel ID
    union <- intersect%>%group_by(PARCELID) %>% summarize(landcover_geometry= st_union(geometry)) %>% mutate(landcover_area = st_area(landcover_geometry))
    link <- cbind(union, i)
    #add parcel area column
    parcel_area <- st_set_geometry(ESVAparcels, NULL)
    add_area <- left_join(link, parcel_area)
    #create percent area crop cover column
    percent <- add_area %>% mutate(perc_area_landcover=(landcover_area/parcel_area)*100)
    #link crop names
    linklandcover <- left_join(percent, Key, by = ("i"))
    #save as dataframe
    final01 <- st_set_geometry(linklandcover, NULL)
    final02 <-as.data.frame(final01)
    datalist[[i]] <- final02
  }
  #bound all df
  bindall <- do.call(rbind, datalist)
  return(bindall)
}

##############################################################################################################################################################
#input into function---------


landuse2016 <- CCAP_Parcels(CCAP = ccap2016)
saveRDS(landuse2016, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/LanduseArea/landuse2016.RDS")
write.csv(landuse2016, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/LanduseArea/landuse2016.cvs")
#
landuse2010 <- CCAP_Parcels(CCAP = ccap2010)
saveRDS(landuse2010, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/LanduseArea/landuse2010.RDS")
write.csv(landuse2010, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/LanduseArea/landuse2010.cvs")
#
landuse2006 <- CCAP_Parcels(CCAP = ccap2006)
saveRDS(landuse2006, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/LanduseArea/landuse2006.RDS")
write.csv(landuse2006, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/LanduseArea/landuse2006.cvs")
#
landuse2001 <- CCAP_Parcels(CCAP = ccap2001)
saveRDS(landuse2001, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/LanduseArea/landuse2001.RDS")
write.csv(landuse2001, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/LanduseArea/landuse2001.cvs")
#
landuse1996 <- CCAP_Parcels(CCAP = ccap1996)
saveRDS(landuse1996, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/LanduseArea/landuse1996.RDS")
write.csv(landuse1996, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/LanduseArea/landuse1996.cvs")
#













