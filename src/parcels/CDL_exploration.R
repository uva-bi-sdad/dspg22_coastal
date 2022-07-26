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

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  stars, # spatiotemporal data handling
  terra, # raster data handling
  raster, # raster data handling
  stringr, # string manipulation
  lubridate, # dates handling
  data.table, # data wrangling
  tidyr, # reshape
  tidyUSDA, # download USDA NASS data
  keyring, # API key management
  FedData, # download Daymet data
  daymetr, # download Daymet data
  ggplot2, # make maps
  tmap, # make maps
  future.apply, # parallel processing
  CropScapeR, # download CDL data
  prism, # download PRISM data
  exactextractr # extract raster values to sf
)  

############raster image##########

#install API 
readRenviron("~/.Renviron")
Sys.getenv("USDA_API_KEY")

#Get sf for Accomack and Northhampton
VA_2_county <- tigris::counties(state = "51", cb = TRUE) %>% 
  st_as_sf() %>% 
  dplyr::filter(NAME %in% c("Accomack", "Northampton")) 

#51001, 51131 are the counties codes if needed#
#extent from both counties getcdldata: 1723935, 1790565, 1745415, 1862205#

###################EXTRA######################################

#this is the plot that shows the bounds that the CDL layer will follow   
ggplot() +
  geom_sf(data = VA_2_county, fill = "lightblue") +
  theme_void()

#Get CDL data within the bounds of two counties
(
  cdl_VA_2 <- GetCDLData(
    aoi = VA_2_county, 
    year = "2020", 
    type = "f",
    format = "sf"
  )
)


ggplot() +
  geom_sf(data = IL_county) +
  geom_sf(data = st_as_sfc(st_bbox(IL_county_4)), fill = "red", alpha = 0.4) +
  theme_void() 

#the downloaded data is a RasterLayer object. Note that the CDL data uses the Albers equal-area conic projection.
terra::crs(cdl_VA_2) 

####### I wonder if I could manually convert this#######
#EX
##: actual code

##as.data.frame(cdl_Champaign, xy = TRUE) %>% 
#--- to sf consisting of points ---#
##st_as_sf(coords = c("x", "y")) %>% 
  #--- Albert conic projection ---#
  ##st_set_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")  

#plot data over area
plot(cdl_VA_2)

#only show area in sf
cdl_VA_2_masked <- VA_2_county %>% 
  #--- change the CRS first to that of the raster data ---#
  st_transform(., projection(cdl_VA_2)) %>% 
  #--- mask the values outside the sf (turn them into NA) ---#
  raster::mask(cdl_VA_2, .)  

#only the counties information included
plot(cdl_VA_2_masked)

######################################################################################################################

#####################################################################################
#CDL as SF 
#####################################################################################

#--- load the crop code reference data ---#
data("linkdata")

######Accomack#######
(
  cdl_sf_Accomack <- GetCDLData(aoi = 51001, year = 2021, type = "f", format = "sf")
)



#join linked data so we know what the values are
(
  crop_data_001 <- dplyr::left_join(cdl_sf_001, linkdata, by = c('value' = 'MasterCat'))
)

#deleted all 'No data' rows(makes sf smaller)
cdl_Accomack<- cdl_sf_Accomack%>%arrange(value)
CDL_ACCOMACK<- cdl_Accomack[-c(1:4082282),]

#larger county --> experimented with northampton first



CDL_Accomack<- st_buffer(CDL_ACCOMACK, dist = 30, endCapStyle = "SQUARE")


######Northampton######
(
  cdl_sf_002 <- GetCDLData(aoi = 51131, year = 2021, type = 'f', format = "sf")
)

#join linked data so we know what the values are
(
  crop_data_002 <- dplyr::left_join(cdl_sf_002, linkdata, by = c('value' = 'MasterCat'))
)

#deleted all 'No data' rows(makes sf smaller)
crop_data_002_excluded<- crop_data_002%>%arrange(value)
crop_data_002_excluded_v1<- crop_data_002_excluded[-c(1:453736),]

###Plotting###

#basic plot to bulid off of
ggplot(data = VA_2_county) + 
  geom_sf(data = crop_data_002_excluded_v1, aes(fill = Crop, geometry=geometry)) +
  geom_sf(data = crop_data_001_excluded_v1, aes(fill = Crop, geometry=geometry)) 

#Josh's notes

# Accomack
sort( table(crop_data_001_excluded_v1$Crop) )

crop_data_001_excluded_v2<- crop_data_001_excluded_v1%>%filter(Crop %in% c("Corn", "Grassland", "Soybeans", "Potatoes", "Alfalfa", "Grassland/Pasture", "Aquaculture", "Sweet_Corn"))
  
#Northampton
sort( table(crop_data_002_excluded_v1$Crop) )

crop_data_002_excluded_v2<- crop_data_002_excluded_v1%>%filter(Crop %in% c("Corn", "Cotton", "Winter_Wheat", "Potatoes", "Sweet_Corn", "Grassland/Pasture", "Soybeans", "Alfalfa"))


library(RColorBrewer)
n <- 35
colrs <- brewer.pal.info[brewer.pal.info$colorblind == TRUE, ]
col_vec = unlist(mapply(brewer.pal, colrs$maxcolors, rownames(colrs)))
col <- sample(col_vec, n)

#all variables
ggplot(data = VA_2_county) + 
      geom_sf(data = VA_2_county) +
     geom_sf(data = crop_data_001_excluded_v1, aes(color=Crop, geometry=geometry), size=0.01) +
     geom_sf(data = crop_data_002_excluded_v1, aes(color=Crop, geometry=geometry), size=0.01) +
     scale_color_manual(values=col) +
     xlab("longitude") + ylab("latitude") + 
     ggtitle("Cropland Data Layer")+
     theme(axis.text.x = element_text(angle=30)) +
     guides(colour = guide_legend(override.aes = list(size=1)))

#top crops
ggplot(data = VA_2_county) + 
  geom_sf(data = VA_2_county) +
  geom_sf(data = crop_data_001_excluded_v2, aes(color=Crop, geometry=geometry), size=0.01) +
  geom_sf(data = crop_data_002_excluded_v2, aes(color=Crop, geometry=geometry), size=0.01) +
  scale_color_manual(values=col) +
  xlab("longitude") + ylab("latitude") + 
  ggtitle("Cropland Data Layer")+
  theme(axis.text.x = element_text(angle=30)) +
  guides(colour = guide_legend(override.aes = list(size=1)))

#######################################################
#Save data so far#
######################################################

#subset parcels
Parcels_Northampton<-ESVAparcels_update%>% filter(LOCALITY == "Northampton County")

Parcels_Accomack<- ESVAparcels_update%>% filter(LOCALITY == "Accomack County")

#save data
saveRDS(Parcels_Northampton, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/parceldata/Parcels_Northampton.RDS")
#saveRDS(CDL_Northampton, file = "CDL_Northampton.RDS")
saveRDS(Parcels_Accomack, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/parceldata/Parcels_Accomack.RDS")
#saveRDS(CDL_Accomack, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/cropland/CDL_Accomack.RDS")


#######################################################
#intersect CDL with parcels#
#######################################################

library(sf)

###########Northampton intersection#################

head(Parcels_Northampton)

#    

################################################################################################################
#1: calulate each parcel area from geometry column
#2: point is 30 x 30 (30m^2) dimensions
#3: from these two columns I can multiply the frequency of each crop by the dimensions and then divided by parcel area and multiple by 100 to get a proper area for each point
##############################################################################################################


#legend for each parcel
parcel_key_northampton<-intersect_Northampton%>%select(LOCALITY, PARCELID, PTM_ID, geometry)
parcel_area_001<- st_area(parcel_key_northampton)

#point linked to parcel with parcel geometry and area
parcel_key_northampton<- cbind(parcel_key, parcel_area_001)

#add point area
library(units)
point_area_keyy<- set_units(rep(30,453051), m^2)
parcel_key_northampton<- cbind(parcel_key_northampton, point_area_keyy)

#delete extra nows
parcel_key_northampton<-parcel_key_northampton %>%unique()

######Frequency#####
library(plyr)

#frequenecy: total
parcel_freq_northampton<- count(intersect_Northampton, 'PARCELID')

###Look at all the crops####
sort( table(intersect_Northampton$Crop) )

####################CORN##############

#frequency: corn
corn<- intersect_Northampton%>%filter(Crop =="Corn")
#head(corn)
corn_freq_northampton<- count(corn, 'PARCELID')
corn_freq_northampton<- corn_freq_northampton%>%dplyr::rename(corn_freq=freq)
#head(corn_freq_northampton)

#join CORN frequency to parcel_key

#TOTAL POINTS IN PARCEL
parcel_key_northampton<- left_join(parcel_key_northampton, parcel_freq_northampton)
parcel_key_northampton<- parcel_key_northampton%>%dplyr:: rename(point_area = point_area_keyy, parcel_area = parcel_area_001)

###############################################################START HERE TO ADD NEW CROP############################################

#Corn points in parcels
parcel_key_northampton<- left_join(parcel_key_northampton, corn_freq_northampton)

#join CORN frequency to parcel_key
parcel_key_northampton<- left_join(parcel_key_northampton, corn_freq_northampton)

#percent area of CORN for each parcel
parcel_key_corn_N<- parcel_key_northampton%>%na.omit(parcel_key_corn)
parcel_key_corn_N<- parcel_key_corn_N%>%mutate(perc_area_corn = ((corn_freq*point_area)/parcel_area)*100)

#save specific crop
saveRDS(parcel_key_corn_N, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/parcel_key_corn_N.RDS")


############SOYBEAN##########

#frequency: soybeans
soybean<-intersect_Northampton%>%filter(Crop == "Soybeans")
soybean_freq_northampton<- count(soybean, 'PARCELID')
soybean_freq_northampton<- soybean_freq_northampton%>%dplyr:: rename(soybean_freq=freq)

#join SOYBEAN frequency to parcel_key
parcel_key_northampton<- left_join(parcel_key_northampton, soybean_freq_northampton)

#percent area of SOYBEAN for each parcel
parcel_key_soybean_N<- parcel_key_northampton%>% select(-(corn_freq))%>%na.omit(parcel_key_soybean)
parcel_key_soybean_N<- parcel_key_soybean_N%>%mutate(perc_area_soybean = ((soybean_freq*point_area)/parcel_area)*100)

#save specific crop
saveRDS(parcel_key_soybean_N, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/CropArea/parcel_key_soybean_N.RDS")


#Always resave when crop is added!

saveRDS(parcel_key_northampton, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/parceldata/parcel_key_northampton.RDS")

############Accomack################

head(Parcels_Accomack)

#match crs with PARCEL crs
CDL_Accomack<- st_transform(CDL_Accomack, crs = st_crs(Parcels_Accomack))

#make sure crs matches
head(Parcels_Accomack)
head(CDL_Accomack)

#wont work without this
sf::sf_use_s2(FALSE)

intersect_Accomack<- st_join(Parcels_Accomack, CDL_Accomack, join = st_intersects)
intersect_Accomack<- intersect_Accomack%>% filter(PARCELID != "character(0)")
intersect_Accomack<- intersect_Accomack%>% select(LOCALITY, PARCELID, PTM_ID, value, Crop, geometry)

saveRDS(intersect_Accomack, file = "intersect_Accomack.RDS")


################################################################################################################
#1: calulate each parcel area from geometry column
#2: point is 30 x 30 (30m^2) dimensions
#3: from these two columns I can multiply the frequency of each crop by the dimensions and then divided by parcel area and multiple by 100 to get a proper area for each point
##############################################################################################################


#legend for each parcel
parcel_key_accomack<-intersect_Accomack%>%select(LOCALITY, PARCELID, PTM_ID, geometry, value, Crop)
parcel_area<- st_area(parcel_key_accomack)

#point linked to parcel with parcel geometry and area
parcel_key_accomack<- cbind(parcel_key_accomack, parcel_area)

#add point area
library(units)
point_area_key<- set_units(rep(30,1163634), m^2)
parcel_key_accomack<- cbind(parcel_key_accomack, point_area_key)
head(parcel_key_accomack)

#delete extra nows
parcel_key_accomack<-parcel_key_accomack %>%unique()

######Frequency#####
library(plyr)

#frequenecy: total
parcel_freq_Accomack<- count(intersect_Accomack, 'PARCELID')

###Look at all the crops####
sort( table(intersect_Accomack$Crop) )

####################CORN##############

#frequency: corn
corn<- intersect_Accomack%>%filter(Crop =="Corn")
#head(corn)
corn_freq_accomack<- count(corn, 'PARCELID')
corn_freq_accomack<- corn_freq_accomack%>%dplyr::rename(corn_freq=freq)
#head(corn_freq_northampton)

#join CORN frequency to parcel_key

#TOTAL POINTS IN PARCEL
parcel_key_Accomack<- left_join(parcel_key_accomack, parcel_freq_accomack)





