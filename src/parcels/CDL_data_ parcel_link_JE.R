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
    year = "2021", 
    type = "b" 
  )
)

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
  cdl_sf_001 <- GetCDLData(aoi = 51001, year = 2021, type = 'f', format = "sf")
)

#join linked data so we know what the values are
(
  crop_data_001 <- dplyr::left_join(cdl_sf_001, linkdata, by = c('value' = 'MasterCat'))
)

#deleted all 'No data' rows(makes sf smaller)
crop_data_001_excluded<- crop_data_001%>%arrange(value)
crop_data_001_excluded_v1<- crop_data_001_excluded[-c(1:4082282),]

#larger county --> experimented with northampton first

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
saveRDS(CDL_Northampton, file = "CDL_Northampton.RDS")
#saveRDS(crop_data_001_excluded_v1, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/cropland/CDL_Accomack.RDS")
saveRDS(Parcels_Accomack, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/parceldata/Parcels_Accomack.RDS")


#######################################################
#intersect CDL with parcels#
#######################################################

library(sf)

###########Northampton#############

head(Parcels_Northampton)

# Match the point and polygon CRS#
#CDL_Northampton<- st_transform(CDL_Northampton, crs = 4326)
#Parcels_Northampton<- st_transform(Parcels_Northampton, crs = 4326)

#match with PARCEL crs#
CDL_Northampton<- st_transform(CDL_Northampton, crs = st_crs(Parcels_Northampton))

#make sure crs match
head(Parcels_Northampton)
head(CDL_Northampton)

#must run  
sf::sf_use_s2(FALSE)

#create column on orginal data set
CDL_Northampton$PAR_ID<-apply(st_intersects(Parcels_Northampton, CDL_Northampton, sparse = FALSE), 2,
                 function(col) {Parcels_Northampton[which(col), ]$VGIN_QPID})


############Accomack###########

head(Parcels_Accomack)

# Match the point and polygon CRS#
#CDL_Accomack<- st_transform(CDL_Accomack, crs = 4326)
#Parcels_Accomack<- st_transform(Parcels_Accomack, crs = 4326)

#match crs with PARCEL crs
CDL_Accomack<- st_transform(CDL_Accomack, crs = st_crs(Parcels_Accomack))

#make sure crs matches
head(Parcels_Accomack)
head(CDL_Accomack)

#wont work without this
sf::sf_use_s2(FALSE)

#create parcel ID in original data set
CDL_Accomack$PAR_ID<-apply(st_intersects(Parcels_Accomack, CDL_Accomack, sparse = FALSE), 2,
                              function(col) {Parcels_Accomack[which(col), ]$VGIN_QPID})



