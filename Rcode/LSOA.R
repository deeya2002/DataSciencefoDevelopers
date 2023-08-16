library(data.table)
library(tidyverse)
library(dplyr)


## Cleaning here

setwd("D:/Diy_DataAssign/")
Cleaned_HP = read.csv("Clean/Cleaned_Town_population.csv" )

LSOA = fread("Datasets/Postcode to LSOA.csv")
pattern = ' .*$'
LSOA_Cleaned = LSOA %>%
  select(lsoa11cd,pcds) %>% 
  mutate(shortPostcode=gsub(pattern,"",pcds)) %>% 
  right_join(Cleaned_HP,by="shortPostcode")  %>% 
  group_by(lsoa11cd) %>% 
  select(lsoa11cd,shortPostcode,Town,District,County) 



LSOA_Cleaned


colnames(LSOA_Cleaned)[1] <- "LSOA code"
view(LSOA_Cleaned)
write.csv(LSOA_Cleaned,"Clean/Cleaned_LSOA.csv",row.names = FALSE,col.names = FALSE)

