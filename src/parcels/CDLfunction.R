############################# creating a function that can link CDL data to Parcel data ######################################
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


#FUCTION: general idea -------

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

################################## All Crops ##############################
data("linkdata")
#allcropvalue<- c(1,2,3,4,5,6,10,11,12,13,14,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,
           ##     38,39,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,63,64,65,66,67,68,69,
           ##      70,71,72,74,75,76,77,81,82,83,87,88,92,111,112,121,122,123,124,131,141,142,143,152,176,190,195,204,205,206,
           ##      207,208,209,210,211,212,213,214,216,217,218,219,220,221,222,223,224,225,226,227,229,230,231,232,233,234,235,236,237,
           ##        238,239,240,241,242,243,244,245,246,2247,248,249,250,254)


#################################################################################
#FUCTION--------

#read in for parcel data
Parcels_Accomack <- readRDS("/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/parceldata/Parcels_Accomack.RDS")
Parcels_Northampton <- readRDS("/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/parceldata/Parcels_Northampton.RDS")


CDL_Crop_Parcel<- function(year, county, parcel){
  library(CropScapeR)
  aoi = county
  #importing CDL data for year and county
  cdl_info <- GetCDLData(aoi = county, year = year, type = "f", format = "sf")
  #deleting no data rows
  nodata <- cdl_info[!(cdl_info$value == "0"), ]
  #create tiles (30m resolution)
  library(sf)
  buffer <- st_buffer(nodata, dist = 15, endCapStyle = "SQUARE")
  library(dplyr)
  data("linkdata")
  link <- left_join(buffer, linkdata, by = c('value' = 'MasterCat'))
  #transform CDL crs to parcel crs
  transform <- st_transform(link, crs = st_crs(parcel))
  #for loop for crops, used value because it is a numeric vector and create the vector within the function
  distinct <- transform %>% distinct(value, .keep_all = TRUE)
  cropvalue <- distinct[['value']]
  #bind all dataframes from loops
  datalist = list()
  for (i in cropvalue){
    allcrops <- transform %>% filter(value == i)
    #intersect crop and parcels
    intersect <- st_intersection(allcrops, st_make_valid(parcel))
    #merge crop geometry for each parcel
    merge <- intersect %>% group_by(PARCELID) %>% summarize(crop_geometry = st_union(geometry)) %>% mutate(crop_area = st_area(crop_geometry))
    #add crop values
    add_name <- merge %>% cbind(merge, i) %>% select(PARCELID, crop_area, crop_geometry, i)
    #add parcel area column
    parcel_area <- st_set_geometry(parcel, NULL)
    add_area_locality <- left_join(add_name, parcel_area)
    #create percent area crop cover column
    percent <- add_area_locality %>% mutate(perc_area_crop=(crop_area/parcel_area)*100)
    #link crop names
    linkcrop <- linkdata %>% rename(i = MasterCat)
    final <- left_join(percent, linkcrop, by = ("i"))
    #save as dataframe
    final01 <- st_set_geometry(final, NULL)
    final02 <-as.data.frame(final01)
    datalist[[i]] <- final02
  }
  #bound all df
  bindall <- do.call(rbind, datalist)
  return(bindall)
}
#################################################################################
# Function inputs --------
#
Northampton2021 <- CDL_Crop_Parcel(year=2021, county=51131, parcel = Parcels_Northampton)
saveRDS(Northampton2021, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2021.RDS")
write.csv(Northampton2021, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2021.csv")
#
Northampton2020 <- CDL_Crop_Parcel(year=2020, county=51131, parcel = Parcels_Northampton)
saveRDS(Northampton2020, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2020.RDS")
write.csv(Northampton2020, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2020.csv")
#
Northampton2019 <- CDL_Crop_Parcel(year=2019, county=51131, parcel = Parcels_Northampton)
saveRDS(Northampton2019, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2019.RDS")
write.csv(Northampton2019, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2019.csv")
#
Northampton2018 <- CDL_Crop_Parcel(year=2018, county=51131, parcel = Parcels_Northampton)
saveRDS(Northampton2018, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2018.RDS")
write.csv(Northampton2018, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2018.csv")
#
Northampton2017 <- CDL_Crop_Parcel(year=2017, county=51131, parcel = Parcels_Northampton)
saveRDS(Northampton2017, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2017.RDS")
write.csv(Northampton2017, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2017.csv")
#
Northampton2016 <- CDL_Crop_Parcel(year=2016, county=51131, parcel = Parcels_Northampton)
saveRDS(Northampton2016, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2016.RDS")
write.csv(Northampton2016, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2016.csv")
#
Northampton2015 <- CDL_Crop_Parcel(year=2015, county=51131, parcel = Parcels_Northampton)
saveRDS(Northampton2015, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2015.RDS")
write.csv(Northampton2015, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2015.csv")
#
Northampton2014 <- CDL_Crop_Parcel(year=2014, county=51131, parcel = Parcels_Northampton)
saveRDS(Northampton2014, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2014.RDS")
write.csv(Northampton2014, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2014.csv")
#
Northampton2013 <- CDL_Crop_Parcel(year=2013, county=51131, parcel = Parcels_Northampton)
saveRDS(Northampton2013, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2013.RDS")
write.csv(Northampton2013, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2013.csv")
#
Northampton2012 <- CDL_Crop_Parcel(year=2012, county=51131, parcel = Parcels_Northampton)
saveRDS(Northampton2012, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2012.RDS")
write.csv(Northampton2012, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2012.csv")
#
Northampton2011 <- CDL_Crop_Parcel(year=2011, county=51131, parcel = Parcels_Northampton)
saveRDS(Northampton2011, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2011.RDS")
write.csv(Northampton2011, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2011.csv")
#
#
Northampton2010 <- CDL_Crop_Parcel(year=2010, county=51131, parcel = Parcels_Northampton)
saveRDS(Northampton2010, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2010.RDS")
write.csv(Northampton2010, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2010.csv")
#
#
Northampton2009 <- CDL_Crop_Parcel(year=2009, county=51131, parcel = Parcels_Northampton)
saveRDS(Northampton2009, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2009.RDS")
write.csv(Northampton2009, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2009.csv")
#
#
Northampton2008 <- CDL_Crop_Parcel(year=2008, county=51131, parcel = Parcels_Northampton)
saveRDS(Northampton2008, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2008.RDS")
write.csv(Northampton2008, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2008.csv")
#
# no data
Northampton2007 <- CDL_Crop_Parcel(year=2007, county=51131, parcel = Parcels_Northampton)
saveRDS(Northampton2007, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2007.RDS")
write.csv(Northampton2007, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2007.csv")
#
# no data
Northampton2006 <- CDL_Crop_Parcel(year=2006, county=51131, parcel = Parcels_Northampton)
saveRDS(Northampton2006, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2006.RDS")
write.csv(Northampton2006, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2006.csv")
#
# no data
Northampton2005 <- CDL_Crop_Parcel(year=2005, county=51131, parcel = Parcels_Northampton)
saveRDS(Northampton2005, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2005.RDS")
write.csv(Northampton2005, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2005.csv")
#
# no data
Northampton2004 <- CDL_Crop_Parcel(year=2004, county=51131, parcel = Parcels_Northampton)
saveRDS(Northampton2004, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2004.RDS")
write.csv(Northampton2004, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2004.csv")
#
# no data
Northampton2003 <- CDL_Crop_Parcel(year=2003, county=51131, parcel = Parcels_Northampton)
saveRDS(Northampton2003, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2003.RDS")
write.csv(Northampton2003, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2003.csv")
#
#
Northampton2002 <- CDL_Crop_Parcel(year=2002, county=51131, parcel = Parcels_Northampton)
saveRDS(Northampton2002, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2002.RDS")
write.csv(Northampton2002, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Northampton2002.csv")
#




#################################################################################
#
Accomack2021 <- CDL_Crop_Parcel(year=2021, county=51001, parcel = Parcels_Accomack)
saveRDS(Accomack2021, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2021.RDS")
write.csv(Accomack2021, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2021.csv")
#
#
Accomack2020<- CDL_Crop_Parcel(year=2020, county=51001, parcel = Parcels_Accomack)
saveRDS(Accomack2020, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2020.RDS")
write.csv(Accomack2020, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2020.csv")
#
#
Accomack2019<- CDL_Crop_Parcel(year=2019, county=51001, parcel = Parcels_Accomack)
saveRDS(Accomack2019, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2019.RDS")
write.csv(Accomack2019, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2019.csv")
#
#
Accomack2018 <- CDL_Crop_Parcel(year=2018, county=51001, parcel = Parcels_Accomack)
saveRDS(Accomack2018, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2018.RDS")
write.csv(Accomack2018, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2018.csv")
#
#
Accomack2017<- CDL_Crop_Parcel(year=2017, county=51001, parcel = Parcels_Accomack)
saveRDS(Accomack2017, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2017.RDS")
write.csv(Accomack2017, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2017.csv")
#
#
Accomack2016<- CDL_Crop_Parcel(year=2016, county=51001, parcel = Parcels_Accomack)
saveRDS(Accomack2016, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2016.RDS")
write.csv(Accomack2016, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2016.csv")
#
#
Accomack2015<- CDL_Crop_Parcel(year=2015, county=51001, parcel = Parcels_Accomack)
saveRDS(Accomack2015, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2015.RDS")
write.csv(Accomack2015, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2015.csv")
#
#
Accomack2014<- CDL_Crop_Parcel(year=2014, county=51001, parcel = Parcels_Accomack)
saveRDS(Accomack2014, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2014.RDS")
write.csv(Accomack2014, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2014.csv")
#
#
Accomack2013<- CDL_Crop_Parcel(year=2013, county=51001, parcel = Parcels_Accomack)
saveRDS(Accomack2013, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2013.RDS")
write.csv(Accomack2013, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2013.csv")
#
#
Accomack2012<- CDL_Crop_Parcel(year=2012, county=51001, parcel = Parcels_Accomack)
saveRDS(Accomack2012, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2012.RDS")
write.csv(Accomack2012, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2012.csv")
#
#
Accomack2011<- CDL_Crop_Parcel(year=2011, county=51001, parcel = Parcels_Accomack)
saveRDS(Accomack2011, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2011.RDS")
write.csv(Accomack2011, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2011.csv")
#
#
Accomack2010<- CDL_Crop_Parcel(year=2010, county=51001, parcel = Parcels_Accomack)
saveRDS(Accomack2010, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2010.RDS")
write.csv(Accomack2010, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2010.csv")
#
#
Accomack2009<- CDL_Crop_Parcel(year=2009, county=51001, parcel = Parcels_Accomack)
saveRDS(Accomack2009, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2009.RDS")
write.csv(Accomack2009, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2009.csv")
#
#
Accomack2008<- CDL_Crop_Parcel(year=2008, county=51001, parcel = Parcels_Accomack)
saveRDS(Accomack2008, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2008.RDS")
write.csv(Accomack2008, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2008.csv")
#
#
Accomack2007<- CDL_Crop_Parcel(year=2007, county=51001, parcel = Parcels_Accomack)
saveRDS(Accomack2007, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2007.RDS")
write.csv(Accomack2007, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2007.csv")
#
#
Accomack2006<- CDL_Crop_Parcel(year=2006, county=51001, parcel = Parcels_Accomack)
saveRDS(Accomack2006, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2006.RDS")
write.csv(Accomack2006, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2006.csv")
#
#
Accomack2005<- CDL_Crop_Parcel(year=2005, county=51001, parcel = Parcels_Accomack)
saveRDS(Accomack2005, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2005.RDS")
write.csv(Accomack2005, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2005.csv")
#
#
Accomack2004<- CDL_Crop_Parcel(year=2004, county=51001, parcel = Parcels_Accomack)
saveRDS(Accomack2004, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2004.RDS")
write.csv(Accomack2004, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2004.csv")
#
#
Accomack2003<- CDL_Crop_Parcel(year=2003, county=51001, parcel = Parcels_Accomack)
saveRDS(Accomack2003, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2003.RDS")
write.csv(Accomack2003, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2003.csv")
#
#
Accomack2002<- CDL_Crop_Parcel(year=2002, county=51001, parcel = Parcels_Accomack)
saveRDS(Accomack2002, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2002.RDS")
write.csv(Accomack2002, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/Accomack2002.csv")
#







