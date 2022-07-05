################CCAP intersections##################

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


##################### CCAP2016- trial ######################

sort( table(ccap2016$landcover) )

#22 land uses total

#get code dictionary for landuse# --> this is in Josh's code

#############Method#################
#1: sort by one landuse
#2: Run intersection between ESVA and CCAP 2016
#3: Run a st_union to get total area of land use for each parcel
#4: get area of each land use in one specifific parcel
#5: link together total area of parcels
#############Method#################

#1: seperate out one landuse
ccap_2_2016<- ccap2016%>% filter(landcover == 2)

#2: run intersection between ESVA and ccap_2_2016
ccap_2_2016<- st_transform(ccap_2_2016, crs = st_crs(ESVAparcels_update))

sf::sf_use_s2(FALSE)

#intersection with 2 and ESVA parcels
ESVA_2<-st_intersection(ccap_2_2016, st_make_valid(ESVAparcels_update))

saveRDS(ESVA_2, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/LanduseArea/ESVA_2.RDS")

#3: Run st_union to get total land use (2) for each parcel it is present in
merge_ESVA_2<-ESVA_2%>%group_by(PARCELID)%>%summarize(two_geometry = st_union(geometry))

#4:parcel ID, area of parcel for two for each parcel
two_parcel_area<- st_area(merge_ESVA_2)
merge_ESVA_2<-cbind(merge_ESVA_2, two_parcel_area)

#5: Link parcel area to merge_ESVA_2 data set
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


#sort largest from smallest


fivenum(merge_ESVA_2$perc_area_two)






