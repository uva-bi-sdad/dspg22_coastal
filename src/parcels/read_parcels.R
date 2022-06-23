# read in VA parcel data and subset to Accomack + Northampton
# intersect with BlackKnight housing data

library(sf)
library(tigris)
library(dplyr)
library(data.table)

path <- "~/../../../../project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/"

# read in VA parcels subset to Accomack + Northampton counties
ESVAparcels <- readRDS(paste0(path,"ESVAparcels.RDS"))

# -------------------------------------------------------

# read in lat+lons of BlackKnight housing data
# convert to sf object with the same projection as parcels
BKnorthampton <- fread(paste0(path,"BlackKnight/northampton_BK_asmt_data.csv")) %>%
  filter(!is.na(property_address_latitiude))

# projections:
# Albers (m): 5070
# WGS84 (deg): 4326
coords_BK <- st_as_sf( data.frame(latitude=BKnorthampton$property_address_latitiude,
                                  longitude=BKnorthampton$property_address_longitude),
                       coords=1:2,crs=4326
)
coords_BK <- st_transform(coords_CL, st_crs(ESVAparcels))

# get parcel ID (pid) of intersection between ESVAparcels and housing data using st_intersects(coords_BK, ESVAparcels)
# repeat for Accomack county

