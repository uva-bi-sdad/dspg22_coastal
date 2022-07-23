# TO DO NEXT:
# sample SERIALNO from the microdata table (joined_pums) that matches each cell from IPF output to create synthetic households
# then repeat for each Census tract in Northampton and Accomack



library(dplyr)
#simple function to take inputs from cleaned pums 
simple<- function(Age, Income, Race, Tenure, Freq){
  filter<- cleaned_pums %>% filter( age == Age, income == Income, race == Race, tenure == Tenure)
  set.seed(Freq)
  sample<- filter[sample(nrow(filter), size = Freq, replace=TRUE), ]
  return(sample)
}
#row1<- simple(Age = "25-44", Income = "Less than 10,000", Race = "White", Tenure = "Owned", Freq = "3")


# Creating a loop function to iterate through ipf output and sample the values accordingly
dfs <- data.frame()
for ( i in 1 : nrow(dfx)){
  if(dfx[i,5] != 0){
    row <- simple(dfx[i,1],dfx[i,2],dfx[i,3],dfx[i,4],dfx[i,5])
    if(nrow(dfs) == 0){
      dfs = row
    }
    else{
      dfs = rbind(dfs,row) 
    }
    
  }
  
}  
