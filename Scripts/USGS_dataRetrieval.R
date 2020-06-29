## https://github.com/USGS-R/dataRetrieval/blob/master/README.Rmd
## install.packages("dataRetrieval")
library(dataRetrieval)

## Retrieving discharge/gage height data for Roanoke Rapids
siteNumber <- "02080500"
RoanokeRapids_info <- readNWISsite(siteNumber)
parameterCd <- c("00060","00065")

## Raw Daily Discharge (Mean, ft3/s) and Gage Height (Mean, ft)
raw_Discharge_GageHt <- readNWISdv(siteNumber, parameterCd, "1985-01-01","2020-04-12")

write.csv(raw_Discharge_GageHt, file = "./rawData/RoanokeRapids_USGS_discharge_gageHt_raw.csv")
write.csv(RoanokeRapids_info, file = "./Data/processedData/roanokerapids_USGS_spatial.csv")

### Retrieving gage height/DO/specific conductance/pH/temperature for Roanoke River at Jamesville, NC
siteNumber2 <- "02081094"
Jamesville_info <- readNWISsite(siteNumber2)
parameterCd2 <- c("00065","00010","00300","00095","00400")
raw_parameters_Jamesville <- readNWISdv(siteNumber2, parameterCd2, "2007-10-01", "2020-04-13")

write.csv(raw_parameters_Jamesville, file = "./Data/RawData/jamesville_USGS_waterquality_raw.csv")

### Retrieving gage height/DO/specific conductance/pH/temperature for Roanoke River near Oak City, NC
siteNumber3 <- "02081022"
OakCity_info <- readNWISsite(siteNumber3)
parameterCd3 <- c("00065","00010","00300","00095","00400")
raw_parameters_OakCity <- readNWISdv(siteNumber3, parameterCd3, "2007-10-01", "2020-04-08")

write.csv(raw_parameters_OakCity, file = "./Data/RawData/OakCity_USGS_waterquality_raw.csv")

str(raw_parameters_Jamesville)


