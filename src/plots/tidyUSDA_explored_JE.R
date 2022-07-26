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


#####################################################################################
#Next method: sf#
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

#plot 2:tried adding color with one palette
ggplot(data = VA_2_county) + 
  +   geom_sf(data = crop_data_002_excluded_v1, aes(fill = Crop, size = .2, geometry=geometry)) +
  +   geom_sf(data = crop_data_001_excluded_v1, aes(fill = Crop, size = .2, geometry=geometry)) +
  +   scale_color_brewer(palette = "Spectral") +
  +   xlab("longitude") + ylab("latitude") + 
  +   ggtitle("Cropland Data Layer") +
  +   theme(axis.text.x = element_text(angle=30))

#Added two plaettes and made points smaller
ggplot(data = VA_2_county) + 
  +   geom_sf(data = crop_data_001_excluded_v1, aes(color=Crop, size = 0.1, geometry=geometry)) +
  +   geom_sf(data = crop_data_002_excluded_v1, aes(color=Crop, size = 0.1, geometry=geometry)) +
  +   scale_color_brewer(palette = c("set3", "paired")) +
  +   xlab("longitude") + ylab("latitude") + 
  +   ggtitle("Cropland Data Layer")+
  +   theme(axis.text.x = element_text(angle=30))



