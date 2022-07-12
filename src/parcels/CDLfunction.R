############################# creating a function that can link CDL data to Parcel data######################################
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

#the steps that I did to do this with corn for one year

### FUCTION ONE ###

#code to do each subfunction
#1: call in cdl data
  #cdl_sf_Northampton <- GetCDLData(aoi = 51131, year = 2021, type = "f", format = "sf")
#2: delete no data rows
  #df[!(df$value == "0"), ]
#3: create 30m x 30m tiles for CDL
  #st_buffer(df, dist = 15, endCapStyle = "SQUARE")
#4: link crop to value 
  #data("linkdata")
  #df <- dplyr::left_join(df, linkdata, by = c('value' = 'MasterCat'))
#5: transform CDL crs to Parcel crs
  #df<- st_transform(df, crs = st_crs(Parcels_Northampton))
#6: code to do each subfunction
  #corn_northampton<- cdl_Northampton%>%filter(Crop == "Corn")
#7: intersect crop geometries and parcel geometry
  #corn_northampton<-st_intersection(corn_northampton, st_make_valid(Parcels_Northampton))
#8: merge crop geometry for each parcel
  #merge_corn_northampton<-corn_northampton%>%group_by(PARCELID)%>%summarize(corn_geometry = st_union(geometry))
#9: add crop area for each parcel
  #corn_parcel_area<- st_area(merge_corn_northampton)
  #merge_corn_northampton<-cbind(merge_corn_northampton, corn_parcel_area)
#10: add parcel area column
  #merge_corn_northampton<- left_join(merge_corn_northampton, Parcel_Area_Northampton)
#11: create percent area crop cover column
  #merge_corn_northampton<-merge_corn_northampton%>%mutate(perc_area_corn=(corn_parcel_area/parcel_area)*100)






################################## SEE WHAT CROPS I WILL USE##############################
data("linkdata")
cropvalue<- c(1,2,3,4,5,6,10,11,12,13,14,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,
                 38,39,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,63,64,65,66,67,68,69,
                 70,71,72,74,75,76,77,81,82,83,87,88,92,111,112,121,122,123,124,131,141,142,143,152,176,190,195,204,205,206,
                 207,208,209,210,211,212,213,214,216,217,218,219,220,221,222,223,224,225,226,227,229,230,231,232,233,234,235,236,237,
                 238,239,240,241,242,243,244,245,246,2247,248,249,250,254)

#read in for parcel data
Parcels_Accomack <- readRDS("/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/parceldata/Parcels_Accomack.RDS")
Parcels_Northampton <- readRDS("/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/parceldata/Parcels_Northampton.RDS")

#################################### FUCTION #############################################

CDL_Crop_Parcel<- function(year, value, county, parcel){
  library(CropScapeR)
  aoi=county
  #importing CDL data for year and county
  cdl_info<- GetCDLData(aoi = county, year = year, type = "f", format = "sf")
  #print(cdl_info)--> works
  #deleting no data rows
  nodata<- cdl_info[!(cdl_info$value == "0"), ]
  #print(nodata)--> works
  #create tiles (30m resolution)
  library(sf)
  buffer<-st_buffer(nodata, dist = 15, endCapStyle = "SQUARE")
  #print(buffer)--> works
  library(dplyr)
  data("linkdata")
  link<- left_join(buffer, linkdata, by = c('value' = 'MasterCat'))
  #print(link)--> works
  #transform CDL crs to parcel crs
  transform<-st_transform(link, crs = st_crs(parcel))
  #print(transform)--> works
  #for loop for crops, used value because it is a numeric vector
  for (i in cropvalue){
    # print(i)--> links it to merge
    allcrops<- transform%>%filter(value == i)
    #print(allcrops) --> works
    #intersect crop and parcels
    intersect<-st_intersection(allcrops, st_make_valid(parcel))
    #print(intersect) --> works
    #merge crop geometry for each parcel
    merge<-intersect%>%group_by(PARCELID)%>%summarize(crop_geometry = st_union(geometry))%>%mutate(crop_area = st_area(crop_geometry))
    #print(merge)
    add_name<- merge%>%cbind(merge, i)%>%select(PARCELID, crop_area, crop_geometry, i)
    #print(add_name)
    #add parcel area column
    parcel_area<-st_set_geometry(parcel, NULL)
    add_area_locality<- left_join(add_name, parcel_area)
    #print(add_area_locality) --> works
    #create percent area crop cover column
    percent<-add_area_locality%>%mutate(perc_area_crop=(crop_area/parcel_area)*100)
    print(percent)
    #link crop names
    linkcrop<- linkdata%>%rename(i = MasterCat)
    final<- left_join(percent, linkcrop, by = ("i"))
    print(final)
  }
}

#################################### FUCTION #############################################

CDL_Crop_Parcel(year=2021, value="allcropvalue", county=51131, parcel = Parcels_Northampton)



################################################## experimental function #########################################
CDL_Crop_Parcel_trial<- function(year, value, county, parcel){
  library(CropScapeR)
  for (i in both){
  aoi=county
  #importing CDL data for year and county
  cdl_info<- GetCDLData(aoi = county, year = year, type = "f", format = "sf")
  #print(cdl_info)--> works
  #deleting no data rows
  nodata<- cdl_info[!(cdl_info$value == "0"), ]
  #print(nodata)--> works
  #create tiles (30m resolution)
  library(sf)
  buffer<-st_buffer(nodata, dist = 15, endCapStyle = "SQUARE")
  #print(buffer)--> works
  library(dplyr)
  data("linkdata")
  link<- left_join(buffer, linkdata, by = c('value' = 'MasterCat'))
  #print(link)--> works
  #transform CDL crs to parcel crs
  transform<-st_transform(link, crs = st_crs(parcel))
  #print(transform)--> works
  #for loop for crops, used value because it is a numeric vector
  for (i in cropvalue){
    # print(i)--> links it to merge
    allcrops<- transform%>%filter(value == i)
    #print(allcrops) --> works
    #intersect crop and parcels
    intersect<-st_intersection(allcrops, st_make_valid(parcel))
    #print(intersect) --> works
    #merge crop geometry for each parcel
    merge<-intersect%>%group_by(PARCELID)%>%summarize(crop_geometry = st_union(geometry))%>%mutate(crop_area = st_area(crop_geometry))
    #print(merge)
    add_name<- merge%>%cbind(merge, i)%>%select(PARCELID, crop_area, crop_geometry, i)
    #print(add_name)
    #add parcel area column
    parcel_area<-st_set_geometry(parcel, NULL)
    add_area_locality<- left_join(add_name, parcel_area)
    #print(add_area_locality) --> works
    #create percent area crop cover column
    percent<-add_area_locality%>%mutate(perc_area_crop=(crop_area/parcel_area)*100)
    print(percent)
    #link crop names
    linkcrop<- linkdata%>%rename(i = MasterCat)
    final<- left_join(percent, linkcrop, by = ("i"))
    #print(final) --> works
  }
  }
}

CDL_Crop_Parcel_trial(year=2021, value="cropvalue", county="both", parcel = Parcels_Northampton)
 
