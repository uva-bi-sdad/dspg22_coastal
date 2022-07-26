#read in packages
library(tidyUSDA)
library(sf)
library(dplyr)
library(ggplot2)
library(CropScapeR)
library(rnaturalearth)
library(ggspatial)
library(rnaturalearthdata)
library(maps)
library(tools)
library(ggrepel)
library(tmaptools)
library(tigris)
library(viridis)
library(RColorBrewer)  

#########################################   Accomack    #################################################

#get Cropscape data as point
(
  cdl_sf_Accomack <- GetCDLData(aoi = 51001, year = 2021, type = "f", format = "sf")
)

# see if there is there is over lap
trial_A<- cdl_sf_Accomack[1:30,]
plot(trial_A)

#deleted all 'No data' rows(makes sf object smaller)
cdl_sf_Accomack<- cdl_sf_Accomack%>%arrange(value)
cdl_sf_Accomack<- cdl_sf_Accomack[-c(1:4082282),]

#make into box (polygons), the point being the centroid of box
cdl_Accomack<- st_buffer(cdl_sf_Accomack, dist = 15, endCapStyle = "SQUARE")

#Check to make sure correct area
st_area(cdl_Accomack)

#no overlap
trial_A<-cdl_Accomack[1:30,]
plot(trial_A)

#add legend for what crop means what
#--- load the crop code reference data ---#
data("linkdata")
#join linked data so we know what the values are
(
  cdl_Accomack <- dplyr::left_join(cdl_Accomack, linkdata, by = c('value' = 'MasterCat'))
)

#orginal cropscape polygon data.set
#wont let me save in Cropland folder on Rivanna
saveRDS(cdl_Accomack, file = "~/cdl_Accomack.RDS")

#add area to Parcels_Accomack data and delete irrelevant columns
parcel_area<-st_area(Parcels_Accomack)
Parcels_Accomack<-cbind(Parcels_Accomack, parcel_area)
Parcels_Accomack<-Parcels_Accomack%>%select(LOCALITY, PARCELID, PTM_ID, parcel_area, geometry)

saveRDS(Parcels_Accomack, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/parceldata/Parcels_Accomack.RDS")


###intersections###
cdl_Accomack<- st_transform(cdl_Accomack, crs = st_crs(Parcels_Accomack))

sf::sf_use_s2(FALSE)

#Try with corn first to create process
corn_accomack<- cdl_Accomack%>%filter(Crop == "Corn")


#intersection with corn and accomack parcels
corn_accomack<-st_intersection(corn_accomack, st_make_valid(Parcels_Accomack))


saveRDS(corn_accomack, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/corn_accomack.RDS")

#merge corn by parcelID and get total area in each parcel for CORN
merge_corn_accomack<-corn_accomack%>%group_by(PARCELID)%>%summarize(corn_geometry = st_union(geometry))
corn_parcel_area<- st_area(merge_corn_accomack)
merge_corn_accomack<-cbind(merge_corn_accomack, corn_parcel_area)


###Find the parcel area as a data.set and can attach this to the merge_corn_accomack and get percentages###

#make Parcel_Accomack as data.set so it can be joined to merge_corn_accomack
Parcel_Area_Accomack<-st_set_geometry(Parcels_Accomack, NULL)
merge_corn_accomack<- left_join(merge_corn_accomack, Parcel_Area_Accomack)
#%>%select(-c(FIPS, LASTUPDATE, VGIN_QPID))

#add percentage column
merge_corn_accomack<-merge_corn_accomack%>%mutate(perc_area_corn=(corn_parcel_area/parcel_area)*100)

saveRDS(merge_corn_accomack, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/merge_corn_accomack.RDS")

#check 
hist(merge_corn_accomack$perc_area_corn)

##########################################   Northampton    #################################################

(
  cdl_sf_Northampton <- GetCDLData(aoi = 51131, year = 2021, type = "f", format = "sf")
)

# see if there is there is over lap in points
trial_NA<- cdl_sf_Northampton[1:30,]
plot(trial_NA)

#deleted all 'No data' rows(makes sf smaller)
cdl_sf_Northampton<- cdl_sf_Northampton%>%arrange(value)
cdl_sf_Northampton<- cdl_sf_Northampton[-c(1:453735),]

#make into box (polygons), the point being the centroid of box
cdl_Northampton<- st_buffer(cdl_sf_Northampton, dist = 15, endCapStyle = "SQUARE")

#Check to make sure correct area
st_area(cdl_Northampton)

#no overlap in polygons
trial_NA<-cdl_Northampton[1:30,]
plot(trial_NA)


#add legend for what crop means what
#--- load the crop code reference data ---#
data("linkdata")
#join linked data so we know what the values are
(
  cdl_Northampton <- dplyr::left_join(cdl_Northampton, linkdata, by = c('value' = 'MasterCat'))
)

#orginal cropscape polygon data.set
#wont let me save in Cropland folder on Rivanna
saveRDS(cdl_Northampton, file = "~/cdl_Northampton.RDS")

#add area to Parcels_Northampton data and delete irrelevant columns
parcel_area<-st_area(Parcels_Northampton)
Parcels_Northampton<-cbind(Parcels_Northampton, parcel_area)
Parcels_Northampton<-Parcels_Northampton%>%select(LOCALITY, PARCELID, PTM_ID, parcel_area, geometry)

saveRDS(Parcels_Northampton, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/parceldata/Parcels_Northampton.RDS")

###intersections###
cdl_Northampton<- st_transform(cdl_Northampton, crs = st_crs(Parcels_Northampton))

sf::sf_use_s2(FALSE)

#Try with corn first
corn_northampton<- cdl_Northampton%>%filter(Crop == "Corn")

#intersection with corn and accomack parcels
corn_northampton<-st_intersection(corn_northampton, st_make_valid(Parcels_Northampton))

####
saveRDS(corn_northampton, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/corn_northampton.RDS")

#merge corn by parcelID and get total area in each parcel for CORN
merge_corn_northampton<-corn_northampton%>%group_by(PARCELID)%>%summarize(corn_geometry = st_union(geometry))
corn_parcel_area<- st_area(merge_corn_northampton)
merge_corn_northampton<-cbind(merge_corn_northampton, corn_parcel_area)

###Find the parcel area as a data.set and can attach this to the merge_corn_northampton and get percentages###

#make Parcel_Northampton as data.set so it can be joined to merge_corn_accomack
Parcel_Area_Northampton<-st_set_geometry(Parcels_Northampton, NULL)
merge_corn_northampton<- left_join(merge_corn_northampton, Parcel_Area_Northampton)

#add percentage column
merge_corn_northampton<-merge_corn_northampton%>%mutate(perc_area_corn=(corn_parcel_area/parcel_area)*100)

saveRDS(merge_corn_northampton, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/merge_corn_northampton.RDS")

