#load in packages
library(dplyr)
library(data.table)
library(mipfp)
library(tidycensus)
library(tigris)
library(acs)

#get census tracts
Northampton_tracts <- tracts(state = "VA",county = "Northampton")
Accomack_tracts <- tracts(state = "VA", county = "Accomack")

#API ACS key
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

########################################################################

# Function-----------------

IPF <- function(County, geoid){
  # marginal controls
  M_age_income_race <- array(NA,dim=c(4,16,7))
  M_tenure_age <- array(NA,dim=c(2,4))
  M_tenure_income <- array(NA,dim=c(2,16))
  M_tenure_race <- array(NA,dim=c(2,7))
  # B19037A-G: Age of Householder by Household Income by Race
  #   (A=White, B=Black, C=AIAN, D=Asian, E=NHPI, F=Other, G=Two or more)
  age_income_race <- list(
    get_acs(geography="tract",county=County,state="VA",year=2020,
            table="B19037A",cache_table=TRUE) %>% filter(GEOID==geoid),
    get_acs(geography="tract",county=County,state="VA",year=2020,
            table="B19037B",cache_table=TRUE) %>% filter(GEOID==geoid),
    get_acs(geography="tract",county=County,state="VA",year=2020,
            table="B19037C",cache_table=TRUE) %>% filter(GEOID==geoid),
    get_acs(geography="tract",county=County,state="VA",year=2020,
            table="B19037D",cache_table=TRUE) %>% filter(GEOID==geoid),
    get_acs(geography="tract",county=County,state="VA",year=2020,
            table="B19037E",cache_table=TRUE) %>% filter(GEOID==geoid),
    get_acs(geography="tract",county=County,state="VA",year=2020,
            table="B19037F",cache_table=TRUE) %>% filter(GEOID== geoid),
    get_acs(geography="tract",county=County,state="VA",year=2020,
            table="B19037G",cache_table=TRUE) %>% filter(GEOID==geoid)
  )
     for(i in 1:length(age_income_race)){
         M_age_income_race[,,i] <- age_income_race[[i]] %>% select(estimate) %>%
        slice(c(3:18, 20:35, 37:52, 54:69)) %>% unlist() %>% as.numeric() %>% matrix(nrow=4, byrow=TRUE)
  }
  tenure_age <- get_acs(geography="tract",county=County,state="VA",year=2020,
                        table="B25007",cache_table=TRUE) %>% filter(GEOID==geoid)
  M_tenure_age[1,1] <- tenure_age$estimate[3]
  M_tenure_age[1,2] <- sum(tenure_age$estimate[4:5])
  M_tenure_age[1,3] <- sum(tenure_age$estimate[6:8])
  M_tenure_age[1,4] <- sum(tenure_age$estimate[9:11])
  M_tenure_age[2,1] <- tenure_age$estimate[13]
  M_tenure_age[2,2] <- sum(tenure_age$estimate[14:15])
  M_tenure_age[2,3] <- sum(tenure_age$estimate[16:18])
  M_tenure_age[2,4] <- sum(tenure_age$estimate[19:21])
  # B25003A-G: Tenure by Race
  # acs.lookup(endyear=2020,span=5,dataset="acs",table.number="B25003A")
  tenure_race <- list(
    get_acs(geography="tract",county=County,state="VA",year=2020,
            table="B25003A",cache_table=TRUE) %>% filter(GEOID==geoid),
    get_acs(geography="tract",county=County,state="VA",year=2020,
            table="B25003B",cache_table=TRUE) %>% filter(GEOID==geoid),
    get_acs(geography="tract",county=County,state="VA",year=2020,
            table="B25003C",cache_table=TRUE) %>% filter(GEOID==geoid),
    get_acs(geography="tract",county=County,state="VA",year=2020,
            table="B25003D",cache_table=TRUE) %>% filter(GEOID==geoid),
    get_acs(geography="tract",county=County,state="VA",year=2020,
            table="B25003E",cache_table=TRUE) %>% filter(GEOID==geoid),
    get_acs(geography="tract",county=County,state="VA",year=2020,
            table="B25003F",cache_table=TRUE) %>% filter(GEOID==geoid),
    get_acs(geography="tract",county=County,state="VA",year=2020,
            table="B25003G",cache_table=TRUE) %>% filter(GEOID==geoid)
  )
  for(i in 1:length(tenure_race)){
    M_tenure_race[,i] <- tenure_race[[i]]$estimate[2:3]
  }
  # run the IPF procedure using these marginals
  # target.list <- list( c(1,2,3), c(4,1), c(4,3) )
  tol <- 0.1 # add to zero counts
  seed <- microdata_table + tol
  target.list <- list( c(1,2,3), c(4,1), c(4,3) )
  target.data <- list(
    M_age_income_race + tol/prod(dim(M_age_income_race)),
    M_tenure_age + tol/prod(dim(M_tenure_age)),
    M_tenure_race + tol/prod(dim(M_tenure_race))
  )
  ipf_out <- Ipfp(seed=seed,
                  target.list=target.list,
                  target.data=target.data)
  round <- round(ipf_out$x.hat) # final cell counts rounded to the nearest household
  IPF_out<- as.data.frame(round)
  no_zero_IPF<- IPF_out %>% filter(Freq != 0)
  return(no_zero_IPF)
}
########################################################################

Northampton930100 <- IPF(County = "Northampton", geoid = "51131930100")
saveRDS(Northampton930100, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/IPFoutput/Northampton930100.RDS")

Northampton930200 <- IPF(County = "Northampton", geoid = "51131930200")
saveRDS(Northampton930200, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/IPFoutput/Northampton930200.RDS")

Northampton930302 <- IPF(County = "Northampton", geoid = "51131930302")
saveRDS(Northampton930302, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/IPFoutput/Northampton930302.RDS")

Northampton930301 <- IPF(County = "Northampton", geoid = "51131930301")
saveRDS(Northampton930301, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/IPFoutput/Northampton930301.RDS")

Northampton990100 <- IPF(County = "Northampton", geoid = "51131990100") #NOT WORKING
saveRDS(Northampton990100, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/IPFoutput/Northampton990100.RDS")

########################################################################
Accomack090300 <- IPF(County = "Accomack", geoid = "51001090300")
saveRDS(Accomack090300, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/IPFoutput/Accomack090300.RDS")

Accomack090800 <- IPF(County = "Accomack", geoid = "51001090800")
saveRDS(Accomack090800, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/IPFoutput/Accomack090800.RDS")

Accomack990200 <- IPF(County = "Accomack", geoid = "51001990200") #NOT WORKING
saveRDS(Accomack990200, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/IPFoutput/Accomack990200.RDS")

Accomack090600 <- IPF(County = "Accomack", geoid = "51001090600") 
saveRDS(Accomack090600, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/IPFoutput/Accomack090600.RDS")

Accomack990100 <- IPF(County = "Accomack", geoid = "51001990100") #NOT WORKING
saveRDS(Accomack090600, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/IPFoutput/Accomack090600.RDS")

Accomack090500 <- IPF(County = "Accomack", geoid = "51001090500")
saveRDS(Accomack090500, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/IPFoutput/Accomack090500.RDS")

Accomack090700 <- IPF(County = "Accomack", geoid = "51001090700")
saveRDS(Accomack090700, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/IPFoutput/Accomack090700.RDS")

Accomack980200 <- IPF(County = "Accomack", geoid = "51001980200") #NOT WORKING
saveRDS(Accomack980200, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/IPFoutput/Accomack980200.RDS")

Accomack980100 <- IPF(County = "Accomack", geoid = "51001980100") #NOT WOKRING (1)
saveRDS(Accomack980100, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/IPFoutput/Accomack980100.RDS")

Accomack090202 <- IPF(County = "Accomack", geoid = "51001090202")
saveRDS(Accomack090202, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/IPFoutput/Accomack090202.RDS")

Accomack090402 <- IPF(County = "Accomack", geoid = "51001090402")
saveRDS(Accomack090402, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/IPFoutput/Accomack090402.RDS")

Accomack090401 <- IPF(County = "Accomack", geoid = "51001090401")
saveRDS(Accomack090401, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/IPFoutput/Accomack090401.RDS")

Accomack090201 <- IPF(County = "Accomack", geoid = "51001090201")
saveRDS(Accomack090201, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/IPFoutput/Accomack090201.RDS")

Accomack090102 <- IPF(County = "Accomack", geoid = "51001090102")
saveRDS(Accomack090102, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/IPFoutput/Accomack090102.RDS")

Accomack090101 <- IPF(County = "Accomack", geoid = "51001090101")
saveRDS(Accomack090102, file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/IPFoutput/Accomack090102.RDS")

# FIX #
#Accomack980100 has 1 observation
#Accomack980200 has 0 observations
#Accomack990100 has 0 observations
#Accomack990200 has 0 observations
#Northampton990100 has 0 observations
