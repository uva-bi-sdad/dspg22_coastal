# read in housing data

library(sf)
library(data.table)

setwd("~/git/dspg22_coastal")

path <- "~/../../../../project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/"

BKaccomack <- fread(paste0(path,"BlackKnight/accomack_BK_asmt_data.csv"))

BKnorthampton <- fread(paste0(path,"BlackKnight/northampton_BK_asmt_data.csv"))
