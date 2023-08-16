library(data.table)
library(tidyverse)
library(stringr)
library(lubridate)

setwd("D:/Diy_DataAssign/")
# Importing CSV files


#oxfordshire data
Crime_2020_05_oxfordshire_ = fread("Datasets/CrimeData/2020-05/2020-05-thames-valley-street.csv")
Crime_2020_06_oxfordshire_ = fread("Datasets/CrimeData/2020-06/2020-06-thames-valley-street.csv")
Crime_2020_07_oxfordshire_ = fread("Datasets/CrimeData/2020-07/2020-07-thames-valley-street.csv")
Crime_2020_08_oxfordshire_ = fread("Datasets/CrimeData/2020-08/2020-08-thames-valley-street.csv")
Crime_2020_09_oxfordshire_ = fread("Datasets/CrimeData/2020-09/2020-09-thames-valley-street.csv")
Crime_2020_10_oxfordshire_ = fread("Datasets/CrimeData/2020-10/2020-10-thames-valley-street.csv")
Crime_2020_11_oxfordshire_ = fread("Datasets/CrimeData/2020-11/2020-11-thames-valley-street.csv")
Crime_2020_12_oxfordshire_ = fread("Datasets/CrimeData/2020-12/2020-12-thames-valley-street.csv")

Crime_2021_01_oxfordshire_  = fread("Datasets/CrimeData/2021-01/2021-01-thames-valley-street.csv")
Crime_2021_02_oxfordshire_  = fread("Datasets/CrimeData/2021-02/2021-02-thames-valley-street.csv")
Crime_2021_03_oxfordshire_  = fread("Datasets/CrimeData/2021-03/2021-03-thames-valley-street.csv")
Crime_2021_04_oxfordshire_  = fread("Datasets/CrimeData/2021-04/2021-04-thames-valley-street.csv")
Crime_2021_05_oxfordshire_  = fread("Datasets/CrimeData/2021-05/2021-05-thames-valley-street.csv")
Crime_2021_06_oxfordshire_  = fread("Datasets/CrimeData/2021-06/2021-06-thames-valley-street.csv")
Crime_2021_07_oxfordshire_  = fread("Datasets/CrimeData/2021-07/2021-07-thames-valley-street.csv")
Crime_2021_08_oxfordshire_  = fread("Datasets/CrimeData/2021-08/2021-08-thames-valley-street.csv")
Crime_2021_09_oxfordshire_  = fread("Datasets/CrimeData/2021-09/2021-09-thames-valley-street.csv")
Crime_2021_10_oxfordshire_  = fread("Datasets/CrimeData/2021-10/2021-10-thames-valley-street.csv")
Crime_2021_11_oxfordshire_  = fread("Datasets/CrimeData/2021-11/2021-11-thames-valley-street.csv")
Crime_2021_12_oxfordshire_  = fread("Datasets/CrimeData/2021-12/2021-12-thames-valley-street.csv")

Crime_2022_01_oxfordshire_  = fread("Datasets/CrimeData/2022-01/2022-01-thames-valley-street.csv")
Crime_2022_02_oxfordshire_  = fread("Datasets/CrimeData/2022-02/2022-02-thames-valley-street.csv")
Crime_2022_03_oxfordshire_  = fread("Datasets/CrimeData/2022-03/2022-03-thames-valley-street.csv")
Crime_2022_04_oxfordshire_  = fread("Datasets/CrimeData/2022-04/2022-04-thames-valley-street.csv")
Crime_2022_05_oxfordshire_  = fread("Datasets/CrimeData/2022-05/2022-05-thames-valley-street.csv")
Crime_2022_06_oxfordshire_  = fread("Datasets/CrimeData/2022-06/2022-06-thames-valley-street.csv")
Crime_2022_07_oxfordshire_  = fread("Datasets/CrimeData/2022-07/2022-07-thames-valley-street.csv")
Crime_2022_08_oxfordshire_  = fread("Datasets/CrimeData/2022-08/2022-08-thames-valley-street.csv")
Crime_2022_09_oxfordshire_  = fread("Datasets/CrimeData/2022-09/2022-09-thames-valley-street.csv")
Crime_2022_10_oxfordshire_  = fread("Datasets/CrimeData/2022-10/2022-10-thames-valley-street.csv")
Crime_2022_11_oxfordshire_  = fread("Datasets/CrimeData/2022-11/2022-11-thames-valley-street.csv")
Crime_2022_12_oxfordshire_  = fread("Datasets/CrimeData/2022-12/2022-12-thames-valley-street.csv")

Crime_2023_01_oxfordshire_  = fread("Datasets/CrimeData/2023-01/2023-01-thames-valley-street.csv")
Crime_2023_02_oxfordshire_  = fread("Datasets/CrimeData/2023-02/2023-02-thames-valley-street.csv")
Crime_2023_03_oxfordshire_  = fread("Datasets/CrimeData/2023-03/2023-03-thames-valley-street.csv")
Crime_2023_04_oxfordshire_  = fread("Datasets/CrimeData/2023-04/2023-04-thames-valley-street.csv")


#north_yorkshire_
Crime_2020_05_north_yorkshire_ = fread("Datasets/CrimeData/2020-05/2020-05-north-yorkshire-street.csv")
Crime_2020_06_north_yorkshire_ = fread("Datasets/CrimeData/2020-06/2020-06-north-yorkshire-street.csv")
Crime_2020_07_north_yorkshire_ = fread("Datasets/CrimeData/2020-07/2020-07-north-yorkshire-street.csv")
Crime_2020_08_north_yorkshire_ = fread("Datasets/CrimeData/2020-08/2020-08-north-yorkshire-street.csv")
Crime_2020_09_north_yorkshire_ = fread("Datasets/CrimeData/2020-09/2020-09-north-yorkshire-street.csv")
Crime_2020_10_north_yorkshire_ = fread("Datasets/CrimeData/2020-10/2020-10-north-yorkshire-street.csv")
Crime_2020_11_north_yorkshire_ = fread("Datasets/CrimeData/2020-11/2020-11-north-yorkshire-street.csv")
Crime_2020_12_north_yorkshire_ = fread("Datasets/CrimeData/2020-12/2020-12-north-yorkshire-street.csv")

Crime_2021_01_north_yorkshire_ = fread("Datasets/CrimeData/2021-01/2021-01-north-yorkshire-street.csv")
Crime_2021_02_north_yorkshire_ = fread("Datasets/CrimeData/2021-02/2021-02-north-yorkshire-street.csv")
Crime_2021_03_north_yorkshire_ = fread("Datasets/CrimeData/2021-03/2021-03-north-yorkshire-street.csv")
Crime_2021_04_north_yorkshire_ = fread("Datasets/CrimeData/2021-04/2021-04-north-yorkshire-street.csv")
Crime_2021_05_north_yorkshire_ = fread("Datasets/CrimeData/2021-05/2021-05-north-yorkshire-street.csv")
Crime_2021_06_north_yorkshire_ = fread("Datasets/CrimeData/2021-06/2021-06-north-yorkshire-street.csv")
Crime_2021_07_north_yorkshire_ = fread("Datasets/CrimeData/2021-07/2021-07-north-yorkshire-street.csv")
Crime_2021_08_north_yorkshire_ = fread("Datasets/CrimeData/2021-08/2021-08-north-yorkshire-street.csv")
Crime_2021_09_north_yorkshire_ = fread("Datasets/CrimeData/2021-09/2021-09-north-yorkshire-street.csv")
Crime_2021_10_north_yorkshire_ = fread("Datasets/CrimeData/2021-10/2021-10-north-yorkshire-street.csv")
Crime_2021_11_north_yorkshire_ = fread("Datasets/CrimeData/2021-11/2021-11-north-yorkshire-street.csv")
Crime_2021_12_north_yorkshire_ = fread("Datasets/CrimeData/2021-12/2021-12-north-yorkshire-street.csv")

Crime_2022_01_north_yorkshire_ = fread("Datasets/CrimeData/2022-01/2022-01-north-yorkshire-street.csv")
Crime_2022_02_north_yorkshire_ = fread("Datasets/CrimeData/2022-02/2022-02-north-yorkshire-street.csv")
Crime_2022_03_north_yorkshire_ = fread("Datasets/CrimeData/2022-03/2022-03-north-yorkshire-street.csv")
Crime_2022_04_north_yorkshire_ = fread("Datasets/CrimeData/2022-04/2022-04-north-yorkshire-street.csv")
Crime_2022_05_north_yorkshire_ = fread("Datasets/CrimeData/2022-05/2022-05-north-yorkshire-street.csv")
Crime_2022_06_north_yorkshire_ = fread("Datasets/CrimeData/2022-06/2022-06-north-yorkshire-street.csv")
Crime_2022_07_north_yorkshire_ = fread("Datasets/CrimeData/2022-07/2022-07-north-yorkshire-street.csv")
Crime_2022_08_north_yorkshire_ = fread("Datasets/CrimeData/2022-08/2022-08-north-yorkshire-street.csv")
Crime_2022_09_north_yorkshire_ = fread("Datasets/CrimeData/2022-08/2022-08-north-yorkshire-street.csv")
Crime_2022_10_north_yorkshire_ = fread("Datasets/CrimeData/2022-10/2022-10-north-yorkshire-street.csv")
Crime_2022_11_north_yorkshire_ = fread("Datasets/CrimeData/2022-11/2022-11-north-yorkshire-street.csv")
Crime_2022_12_north_yorkshire_ = fread("Datasets/CrimeData/2022-12/2022-12-north-yorkshire-street.csv")

Crime_2023_01_north_yorkshire_ = fread("Datasets/CrimeData/2023-01/2023-01-north-yorkshire-street.csv")
Crime_2023_02_north_yorkshire_ = fread("Datasets/CrimeData/2023-02/2023-02-north-yorkshire-street.csv")
Crime_2023_03_north_yorkshire_ = fread("Datasets/CrimeData/2023-03/2023-03-north-yorkshire-street.csv")
Crime_2023_04_north_yorkshire_ = fread("Datasets/CrimeData/2023-04/2023-04-north-yorkshire-street.csv")



#south-yorkshire-street
Crime_2020_05_south_yorkshire_ = fread("Datasets/CrimeData/2020-05/2020-05-south-yorkshire-street.csv")
Crime_2020_06_south_yorkshire_ = fread("Datasets/CrimeData/2020-06/2020-06-south-yorkshire-street.csv")
Crime_2020_07_south_yorkshire_= fread("Datasets/CrimeData/2020-07/2020-07-south-yorkshire-street.csv")
Crime_2020_08_south_yorkshire_ = fread("Datasets/CrimeData/2020-08/2020-08-south-yorkshire-street.csv")
Crime_2020_09_south_yorkshire_= fread("Datasets/CrimeData/2020-09/2020-09-south-yorkshire-street.csv")
Crime_2020_10_south_yorkshire_ = fread("Datasets/CrimeData/2020-10/2020-10-south-yorkshire-street.csv")
Crime_2020_11_south_yorkshire_ = fread("Datasets/CrimeData/2020-11/2020-11-south-yorkshire-street.csv")
Crime_2020_12_south_yorkshire_ = fread("Datasets/CrimeData/2020-12/2020-12-south-yorkshire-street.csv")

Crime_2021_01_south_yorkshire_ = fread("Datasets/CrimeData/2021-01/2021-01-south-yorkshire-street.csv")
Crime_2021_02_south_yorkshire_ = fread("Datasets/CrimeData/2021-02/2021-02-south-yorkshire-street.csv")
Crime_2021_03_south_yorkshire_ = fread("Datasets/CrimeData/2021-03/2021-03-south-yorkshire-street.csv")
Crime_2021_04_south_yorkshire_ = fread("Datasets/CrimeData/2021-04/2021-04-south-yorkshire-street.csv")
Crime_2021_05_south_yorkshire_ = fread("Datasets/CrimeData/2021-05/2021-05-south-yorkshire-street.csv")
Crime_2021_06_south_yorkshire_ = fread("Datasets/CrimeData/2021-06/2021-06-south-yorkshire-street.csv")
Crime_2021_07_south_yorkshire_ = fread("Datasets/CrimeData/2021-07/2021-07-south-yorkshire-street.csv")
Crime_2021_08_south_yorkshire_ = fread("Datasets/CrimeData/2021-08/2021-08-south-yorkshire-street.csv")
Crime_2021_09_south_yorkshire_ = fread("Datasets/CrimeData/2021-09/2021-09-south-yorkshire-street.csv")
Crime_2021_10_south_yorkshire_ = fread("Datasets/CrimeData/2021-10/2021-10-south-yorkshire-street.csv")
Crime_2021_11_south_yorkshire_ = fread("Datasets/CrimeData/2021-11/2021-11-south-yorkshire-street.csv")
Crime_2021_12_south_yorkshire_ = fread("Datasets/CrimeData/2021-12/2021-12-south-yorkshire-street.csv")

Crime_2022_01_south_yorkshire_ = fread("Datasets/CrimeData/2022-01/2022-01-south-yorkshire-street.csv")
Crime_2022_02_south_yorkshire_ = fread("Datasets/CrimeData/2022-02/2022-02-south-yorkshire-street.csv")
Crime_2022_03_south_yorkshire_ = fread("Datasets/CrimeData/2022-03/2022-03-south-yorkshire-street.csv")
Crime_2022_04_south_yorkshire_ = fread("Datasets/CrimeData/2022-04/2022-04-south-yorkshire-street.csv")
Crime_2022_05_south_yorkshire_ = fread("Datasets/CrimeData/2022-05/2022-05-south-yorkshire-street.csv")
Crime_2022_06_south_yorkshire_ = fread("Datasets/CrimeData/2022-06/2022-06-south-yorkshire-street.csv")
Crime_2022_07_south_yorkshire_ = fread("Datasets/CrimeData/2022-07/2022-07-south-yorkshire-street.csv")
Crime_2022_08_south_yorkshire_ = fread("Datasets/CrimeData/2022-08/2022-08-south-yorkshire-street.csv")
Crime_2022_09_south_yorkshire_ = fread("Datasets/CrimeData/2022-08/2022-08-south-yorkshire-street.csv")
Crime_2022_09_south_yorkshire_ = fread("Datasets/CrimeData/2022-10/2022-10-south-yorkshire-street.csv")
Crime_2022_11_south_yorkshire_ = fread("Datasets/CrimeData/2022-11/2022-11-south-yorkshire-street.csv")
Crime_2022_12_south_yorkshire_ = fread("Datasets/CrimeData/2022-12/2022-12-south-yorkshire-street.csv")

Crime_2023_01_south_yorkshire_ = fread("Datasets/CrimeData/2023-01/2023-01-south-yorkshire-street.csv")
Crime_2023_02_south_yorkshire_ = fread("Datasets/CrimeData/2023-02/2023-02-south-yorkshire-street.csv")
Crime_2023_03_south_yorkshire_ = fread("Datasets/CrimeData/2023-03/2023-03-south-yorkshire-street.csv")
Crime_2023_04_south_yorkshire_ = fread("Datasets/CrimeData/2023-04/2023-04-south-yorkshire-street.csv")




#West-yorkshire-street
Crime_2020_05_west_yorkshire_ = fread("Datasets/CrimeData/2020-05/2020-05-west-yorkshire-street.csv")
Crime_2020_06_west_yorkshire_ = fread("Datasets/CrimeData/2020-06/2020-06-west-yorkshire-street.csv")
Crime_2020_07_west_yorkshire_=  fread("Datasets/CrimeData/2020-07/2020-07-west-yorkshire-street.csv")
Crime_2020_08_west_yorkshire_ = fread("Datasets/CrimeData/2020-08/2020-08-west-yorkshire-street.csv")
Crime_2020_09_west_yorkshire_=  fread("Datasets/CrimeData/2020-09/2020-09-west-yorkshire-street.csv")
Crime_2020_10_west_yorkshire_ = fread("Datasets/CrimeData/2020-10/2020-10-west-yorkshire-street.csv")
Crime_2020_11_west_yorkshire_ = fread("Datasets/CrimeData/2020-11/2020-11-west-yorkshire-street.csv")
Crime_2020_12_west_yorkshire_ = fread("Datasets/CrimeData/2020-12/2020-12-west-yorkshire-street.csv")

Crime_2021_01_west_yorkshire_ = fread("Datasets/CrimeData/2021-01/2021-01-west-yorkshire-street.csv")
Crime_2021_02_west_yorkshire_ = fread("Datasets/CrimeData/2021-02/2021-02-west-yorkshire-street.csv")
Crime_2021_03_west_yorkshire_ = fread("Datasets/CrimeData/2021-03/2021-03-west-yorkshire-street.csv")
Crime_2021_04_west_yorkshire_ = fread("Datasets/CrimeData/2021-04/2021-04-west-yorkshire-street.csv")
Crime_2021_05_west_yorkshire_ = fread("Datasets/CrimeData/2021-05/2021-05-west-yorkshire-street.csv")
Crime_2021_06_west_yorkshire_ = fread("Datasets/CrimeData/2021-06/2021-06-west-yorkshire-street.csv")
Crime_2021_07_west_yorkshire_ = fread("Datasets/CrimeData/2021-07/2021-07-west-yorkshire-street.csv")
Crime_2021_08_west_yorkshire_ = fread("Datasets/CrimeData/2021-08/2021-08-west-yorkshire-street.csv")
Crime_2021_09_west_yorkshire_ = fread("Datasets/CrimeData/2021-09/2021-09-west-yorkshire-street.csv")
Crime_2021_10_west_yorkshire_ = fread("Datasets/CrimeData/2021-10/2021-10-west-yorkshire-street.csv")
Crime_2021_11_west_yorkshire_ = fread("Datasets/CrimeData/2021-11/2021-11-west-yorkshire-street.csv")
Crime_2021_12_west_yorkshire_ = fread("Datasets/CrimeData/2021-12/2021-12-west-yorkshire-street.csv")

Crime_2022_01_west_yorkshire_ = fread("Datasets/CrimeData/2022-01/2022-01-west-yorkshire-street.csv")
Crime_2022_02_west_yorkshire_ = fread("Datasets/CrimeData/2022-02/2022-02-west-yorkshire-street.csv")
Crime_2022_03_west_yorkshire_ = fread("Datasets/CrimeData/2022-03/2022-03-west-yorkshire-street.csv")
Crime_2022_04_west_yorkshire_ = fread("Datasets/CrimeData/2022-04/2022-04-west-yorkshire-street.csv")
Crime_2022_05_west_yorkshire_ = fread("Datasets/CrimeData/2022-05/2022-05-west-yorkshire-street.csv")
Crime_2022_06_west_yorkshire_ = fread("Datasets/CrimeData/2022-06/2022-06-west-yorkshire-street.csv")
Crime_2022_07_west_yorkshire_ = fread("Datasets/CrimeData/2022-07/2022-07-west-yorkshire-street.csv")
Crime_2022_08_west_yorkshire_ = fread("Datasets/CrimeData/2022-08/2022-08-west-yorkshire-street.csv")
Crime_2022_09_west_yorkshire_ = fread("Datasets/CrimeData/2022-08/2022-08-west-yorkshire-street.csv")
Crime_2022_10_west_yorkshire_ = fread("Datasets/CrimeData/2022-10/2022-10-west-yorkshire-street.csv")
Crime_2022_11_west_yorkshire_ = fread("Datasets/CrimeData/2022-11/2022-11-west-yorkshire-street.csv")
Crime_2022_12_west_yorkshire_ = fread("Datasets/CrimeData/2022-12/2022-12-west-yorkshire-street.csv")

Crime_2023_01_west_yorkshire_ = fread("Datasets/CrimeData/2023-01/2023-01-west-yorkshire-street.csv")
Crime_2023_02_west_yorkshire_ = fread("Datasets/CrimeData/2023-02/2023-02-west-yorkshire-street.csv")
Crime_2023_03_west_yorkshire_ = fread("Datasets/CrimeData/2023-03/2023-03-west-yorkshire-street.csv")
Crime_2023_04_west_yorkshire_ = fread("Datasets/CrimeData/2023-04/2023-04-west-yorkshire-street.csv")




Crime_combined = rbind(
  Crime_2020_05_oxfordshire_, Crime_2020_06_oxfordshire_,  Crime_2020_07_oxfordshire_, Crime_2020_08_oxfordshire_,
  Crime_2020_09_oxfordshire_, Crime_2020_10_oxfordshire_, Crime_2020_11_oxfordshire_, Crime_2020_12_oxfordshire_,
  Crime_2021_01_oxfordshire_,Crime_2021_02_oxfordshire_,Crime_2021_03_oxfordshire_,Crime_2021_04_oxfordshire_,
  Crime_2021_05_oxfordshire_,Crime_2021_06_oxfordshire_,Crime_2021_07_oxfordshire_,Crime_2021_08_oxfordshire_,
  Crime_2021_09_oxfordshire_,Crime_2021_10_oxfordshire_,Crime_2021_11_oxfordshire_,Crime_2021_12_oxfordshire_,
  Crime_2022_01_oxfordshire_,Crime_2022_02_oxfordshire_,Crime_2022_03_oxfordshire_,Crime_2022_04_oxfordshire_,
  Crime_2022_05_oxfordshire_,Crime_2022_06_oxfordshire_,Crime_2022_07_oxfordshire_,Crime_2022_08_oxfordshire_,
  Crime_2022_09_oxfordshire_,Crime_2022_10_oxfordshire_,Crime_2022_11_oxfordshire_,Crime_2022_12_oxfordshire_,
  Crime_2023_01_oxfordshire_,Crime_2023_02_oxfordshire_,Crime_2023_03_oxfordshire_,Crime_2023_04_oxfordshire_,
  
  Crime_2020_05_north_yorkshire_,Crime_2020_06_north_yorkshire_,Crime_2020_07_north_yorkshire_,Crime_2020_08_north_yorkshire_,
  Crime_2020_09_north_yorkshire_,Crime_2020_10_north_yorkshire_,Crime_2020_11_north_yorkshire_,Crime_2020_12_north_yorkshire_,
  Crime_2021_01_north_yorkshire_,Crime_2021_02_north_yorkshire_,Crime_2021_02_north_yorkshire_,Crime_2021_04_north_yorkshire_,
  Crime_2021_05_north_yorkshire_,Crime_2021_06_north_yorkshire_,Crime_2021_07_north_yorkshire_,Crime_2021_08_north_yorkshire_,
  Crime_2021_09_north_yorkshire_,Crime_2021_10_north_yorkshire_,Crime_2021_11_north_yorkshire_,Crime_2021_12_north_yorkshire_,
  Crime_2022_01_north_yorkshire_,Crime_2022_02_north_yorkshire_,Crime_2022_03_north_yorkshire_,Crime_2022_04_north_yorkshire_,
  Crime_2022_05_north_yorkshire_,Crime_2022_06_north_yorkshire_,Crime_2022_07_north_yorkshire_,Crime_2022_08_north_yorkshire_,
  Crime_2022_09_north_yorkshire_,Crime_2022_10_north_yorkshire_,Crime_2022_11_north_yorkshire_,Crime_2022_12_north_yorkshire_,
  Crime_2023_01_north_yorkshire_,Crime_2023_02_north_yorkshire_,Crime_2023_03_north_yorkshire_,Crime_2023_04_north_yorkshire_,
  
  Crime_2020_05_south_yorkshire_,Crime_2020_06_south_yorkshire_,Crime_2020_07_south_yorkshire_,Crime_2020_08_south_yorkshire_,
  Crime_2020_09_south_yorkshire_,Crime_2020_10_south_yorkshire_,Crime_2020_11_south_yorkshire_,Crime_2020_12_south_yorkshire_,
  Crime_2021_01_south_yorkshire_,Crime_2021_02_south_yorkshire_,Crime_2021_03_south_yorkshire_,Crime_2021_04_south_yorkshire_,
  Crime_2021_05_south_yorkshire_,Crime_2021_06_south_yorkshire_,Crime_2021_07_south_yorkshire_,Crime_2021_08_south_yorkshire_,
  Crime_2021_09_south_yorkshire_,Crime_2021_10_south_yorkshire_,Crime_2021_11_south_yorkshire_,Crime_2021_12_south_yorkshire_,
  Crime_2022_01_south_yorkshire_,Crime_2022_02_south_yorkshire_,Crime_2022_03_south_yorkshire_,Crime_2022_04_south_yorkshire_,
  Crime_2022_05_south_yorkshire_,Crime_2022_06_south_yorkshire_,Crime_2022_07_south_yorkshire_,Crime_2022_08_south_yorkshire_,
  Crime_2022_09_south_yorkshire_,Crime_2020_10_south_yorkshire_,Crime_2022_11_south_yorkshire_,Crime_2022_12_south_yorkshire_,
  Crime_2023_01_south_yorkshire_,Crime_2023_02_south_yorkshire_,Crime_2023_03_south_yorkshire_,Crime_2023_04_south_yorkshire_,
  
  Crime_2020_05_west_yorkshire_,Crime_2020_06_west_yorkshire_,Crime_2020_07_west_yorkshire_,Crime_2020_08_west_yorkshire_,
  Crime_2020_09_west_yorkshire_,Crime_2020_10_west_yorkshire_,Crime_2020_11_west_yorkshire_,Crime_2020_12_west_yorkshire_,
  Crime_2021_01_west_yorkshire_,Crime_2021_02_west_yorkshire_,Crime_2021_03_west_yorkshire_,Crime_2021_04_west_yorkshire_,
  Crime_2021_05_west_yorkshire_,Crime_2021_06_west_yorkshire_,Crime_2021_07_west_yorkshire_,Crime_2021_08_west_yorkshire_,
  Crime_2021_09_west_yorkshire_,Crime_2021_10_west_yorkshire_,Crime_2021_11_west_yorkshire_,Crime_2021_12_west_yorkshire_,
  Crime_2022_01_west_yorkshire_,Crime_2022_02_west_yorkshire_,Crime_2022_03_west_yorkshire_,Crime_2022_04_west_yorkshire_,
  Crime_2022_05_west_yorkshire_,Crime_2022_06_west_yorkshire_,Crime_2022_07_west_yorkshire_,Crime_2022_08_west_yorkshire_,
  Crime_2022_09_west_yorkshire_,Crime_2022_10_west_yorkshire_,Crime_2022_11_west_yorkshire_,Crime_2022_12_west_yorkshire_,
  Crime_2023_01_west_yorkshire_,Crime_2023_02_west_yorkshire_,Crime_2023_03_west_yorkshire_,Crime_2023_04_west_yorkshire_
  
)


Crime_combined<- Crime_combined %>% 
  as_tibble()

write.csv(Crime_combined,"Clean/Combined_Crime_Data.csv",row.names = FALSE)


#cleaning crime data
crimedata = fread('Clean/Combined_Crime_Data.csv') %>% 
  select(Month, `LSOA code`, `Crime type`)

colnames(crimedata) = c("Year", "LSOA.code", "CrimeType")

pattern = ' .*$'
LsoaToPostcode = fread('Clean/Cleaned_LSOA.csv')
colnames(LsoaToPostcode)=c("LSOA.code","shortPostcode","Town","District","County")
view(crimedata)
View(LsoaToPostcode)




crimedataCleaned = crimedata %>%
  slice(1:10000) %>% 
  left_join(LsoaToPostcode,by="LSOA.code") %>% 
  mutate(Year = str_trim(substring(Year, 1,4))) %>% 
  group_by(shortPostcode,CrimeType,Year)%>% 
  select(shortPostcode, Year, CrimeType, ) %>% 
  na.omit() %>% 
  tally() %>% 
  as_tibble()

View(crimedataCleaned)


crimedataCleaned = cbind(ID = 1:nrow(crimedataCleaned), crimedataCleaned)  
colnames(crimedataCleaned)= c("ID","shortPostcode","CrimeType","Year" , "CrimeCount")
View(crimedataCleaned)

write.csv(crimedataCleaned, "Clean/Cleaned_Crime_Data.csv",row.names = FALSE)





Town = read_csv("Clean/Cleaned_Town_population.csv")%>% 
  select(-Year)
crime_Data = read_csv("Clean/Combined_Crime_Data.csv")

View(Town)


crimeData = crime_Data %>% 
  left_join(Town, by = "shortPostcode") %>% 
  na.omit()


crimeData$County[crimeData$County == "WEST YORKSHIRE" ] <- "YORKSHIRE"
crimeData$County[crimeData$County == "NORTH YORKSHIRE" ] <- "YORKSHIRE"
crimeData$County[crimeData$County == "SOUTH YORKSHIRE" ] <- "YORKSHIRE"

view(crimeData)













