# Exaxmple of IPF for one Census tract

library(dplyr)
library(data.table)
library(mipfp)
library(tidycensus)
library(tigris)

# get Census tracts for Northampton County
Northampton_tracts <- tracts(state="VA",county="Northampton")

# read in ACS PUMS
path <- "~/../../../../project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/"

hva_pums <- fread(paste0(path,"ACS_PUMS/psam_h51.csv")) %>% filter(PUMA=="51125")
pva_pums <- fread(paste0(path,"ACS_PUMS/psam_p51.csv")) %>% filter(PUMA=="51125")
householder_pums <- pva_pums %>% filter(RELSHIPP==20) # subset to the reference person in each household
joined_pums <- householder_pums %>% left_join(hva_pums,by=c("SERIALNO")) # join back to households

# joint tables for income, race, age using the microdata
# bin these as they appear in ACS tract-level tables
# HINCP household income
# RAC1P race
# AGEP age
breaks_age <- c(-1,24,44,64,Inf)
labels_age <- c("0-24","25-44","45-64","65+")
breaks_income <- c(-Inf,9999,14999,19999,24999,29999,34999,39999,44999,49999,
                   59999,74999,99999,124999,149999,199999,Inf)
labels_income <- c("Less than 10,000","10,000-14,999","15,000-19,999","20,000-24,999",
                   "25,000-29,999","30,000-34,999","35,000-39,999","40,000-44,999","45,000-49,999",
                   "50,000-59,999","60,000-74,999","75,000-99,999","100,000-124,999",
                   "125,000-149,999","150,000-199,999","200,000 or more")
breaks_race <- c(0,1,2,5,6,7,8,9)
labels_race <- c("White","Black","AIAN","Asian","NHPI","Other","Two or more races")
breaks_tenure <- c(-1,2,4)
labels_tenure <- c("Owned","Rented")

cleaned_pums <- data.frame(SERIALNO=joined_pums$SERIALNO,
                           age=cut(joined_pums$AGEP, breaks=breaks_age, labels=labels_age),
                           income=cut(joined_pums$HINCP, breaks=breaks_income, labels=labels_income),
                           race=cut(joined_pums$RAC1P, breaks=breaks_race, labels=labels_race),
                           tenure=cut(joined_pums$TEN, breaks=breaks_tenure, labels=labels_tenure))

microdata_table <- table(cleaned_pums$age, cleaned_pums$income, cleaned_pums$race, cleaned_pums$tenure)

# read in ACS Census Tract tables:
# request a key at https://api.census.gov/data/key_signup.html
census_api_key("853b2a1e71aa0aff0f3db966545e8898c73f0772")

# marginal controls
M_age_income_race <- array(NA,dim=c(4,16,7))
M_tenure_age <- array(NA,dim=c(2,4))
M_tenure_income <- array(NA,dim=c(2,16))
M_tenure_race <- array(NA,dim=c(2,7))

library(acs)
#acs.lookup(endyear=2020,span=5,dataset="acs",table.number="B19037A")

# B19037A-G: Age of Householder by Household Income by Race
#   (A=White, B=Black, C=AIAN, D=Asian, E=NHPI, F=Other, G=Two or more)
age_income_race <- list(
  get_acs(geography="tract",county="Northampton",state="VA",year=2020,
          table="B19037A",cache_table=TRUE) %>% filter(GEOID=="51131930100"),
  get_acs(geography="tract",county="Northampton",state="VA",year=2020,
          table="B19037B",cache_table=TRUE) %>% filter(GEOID=="51131930100"),
  get_acs(geography="tract",county="Northampton",state="VA",year=2020,
          table="B19037C",cache_table=TRUE) %>% filter(GEOID=="51131930100"),
  get_acs(geography="tract",county="Northampton",state="VA",year=2020,
          table="B19037D",cache_table=TRUE) %>% filter(GEOID=="51131930100"),
  get_acs(geography="tract",county="Northampton",state="VA",year=2020,
          table="B19037E",cache_table=TRUE) %>% filter(GEOID=="51131930100"),
  get_acs(geography="tract",county="Northampton",state="VA",year=2020,
          table="B19037F",cache_table=TRUE) %>% filter(GEOID=="51131930100"),
  get_acs(geography="tract",county="Northampton",state="VA",year=2020,
          table="B19037G",cache_table=TRUE) %>% filter(GEOID=="51131930100")
)

for(i in 1:length(age_income_race)){
  M_age_income_race[,,i] <- age_income_race[[i]] %>% select(estimate) %>%
    slice(c(3:18, 20:35, 37:52, 54:69)) %>% unlist() %>% as.numeric() %>% matrix(nrow=4, byrow=TRUE)
}

# B25007: Tenure by Age of Householder
# acs.lookup(endyear=2020,span=5,dataset="acs",table.number="B25007")
tenure_age <- get_acs(geography="tract",county="Northampton",state="VA",year=2020,
                      table="B25007",cache_table=TRUE) %>% filter(GEOID=="51131930100")
M_tenure_age[1,1] <- tenure_age$estimate[3]
M_tenure_age[1,2] <- sum(tenure_age$estimate[4:5])
M_tenure_age[1,3] <- sum(tenure_age$estimate[6:8])
M_tenure_age[1,4] <- sum(tenure_age$estimate[9:11])
M_tenure_age[2,1] <- tenure_age$estimate[13]
M_tenure_age[2,2] <- sum(tenure_age$estimate[14:15])
M_tenure_age[2,3] <- sum(tenure_age$estimate[16:18])
M_tenure_age[2,4] <- sum(tenure_age$estimate[19:21])

# B25118: Tenure by Household Income
# acs.lookup(endyear=2020,span=5,dataset="acs",table.number="B25118")
tenure_income <- get_acs(geography="tract",county="Northampton",state="VA",year=2020,
                      table="B25118",cache_table=TRUE) %>% filter(GEOID=="51131930100")
# problem: this table uses different bins for income; skip for now

# B25003A-G: Tenure by Race
# acs.lookup(endyear=2020,span=5,dataset="acs",table.number="B25003A")
tenure_race <- list(
  get_acs(geography="tract",county="Northampton",state="VA",year=2020,
          table="B25003A",cache_table=TRUE) %>% filter(GEOID=="51131930100"),
  get_acs(geography="tract",county="Northampton",state="VA",year=2020,
          table="B25003B",cache_table=TRUE) %>% filter(GEOID=="51131930100"),
  get_acs(geography="tract",county="Northampton",state="VA",year=2020,
          table="B25003C",cache_table=TRUE) %>% filter(GEOID=="51131930100"),
  get_acs(geography="tract",county="Northampton",state="VA",year=2020,
          table="B25003D",cache_table=TRUE) %>% filter(GEOID=="51131930100"),
  get_acs(geography="tract",county="Northampton",state="VA",year=2020,
          table="B25003E",cache_table=TRUE) %>% filter(GEOID=="51131930100"),
  get_acs(geography="tract",county="Northampton",state="VA",year=2020,
          table="B25003F",cache_table=TRUE) %>% filter(GEOID=="51131930100"),
  get_acs(geography="tract",county="Northampton",state="VA",year=2020,
          table="B25003G",cache_table=TRUE) %>% filter(GEOID=="51131930100")
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
round(ipf_out$x.hat) # final cell counts rounded to the nearest household



# TO DO NEXT:
# sample SERIALNO from the microdata table (joined_pums) that matches each cell from IPF output to create synthetic households
# then repeat for each Census tract in Northampton and Accomack
