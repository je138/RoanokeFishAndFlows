getwd()
library(ggplot2)
library(tidyverse)

## Importing raw data from NC DMF juvenile alosine survey in Albemarle Sound (see metadata)
raw.DMF.data <- read.csv("./Data/RawData/P100_7219_EYNON.CSV")

## Checking structure of data
str(raw.DMF.data)

## Resetting data classes
raw.DMF.data$control1 <- as.factor(raw.DMF.data$control1)
raw.DMF.data$program <- as.factor(raw.DMF.data$program)
raw.DMF.data$location <- as.factor(raw.DMF.data$location)
raw.DMF.data$quad <- as.factor(raw.DMF.data$quad)
raw.DMF.data$gear1 <- as.factor(raw.DMF.data$gear1)
raw.DMF.data$control3 <- as.factor(raw.DMF.data$control3)
raw.DMF.data$species <- as.factor(raw.DMF.data$species)
raw.DMF.data$spstatus <- as.factor(raw.DMF.data$spstatus)
raw.DMF.data$sex <- as.factor(raw.DMF.data$sex)

## Combining year, month, day columns into one column with class date
raw.DMF.data$date <- as.Date(paste(raw.DMF.data$year, raw.DMF.data$month, raw.DMF.data$day, sep = "-"),
                             format = "%Y-%m-%d")
class(raw.DMF.data$date)

## Creating subset of "Station 9S" observations (exploratory station for A. shad; shorter time series)
station9S <- subset(raw.DMF.data, raw.DMF.data$station == "9S")

## Excluding Station 9S for the purpose of calculating juvenile abundance index (see metadata)
raw.data.without9S <- raw.DMF.data %>% filter(raw.DMF.data$station != "9S")

## Filtering for only first pulls of each month; first replacing NAs with "0"
raw.data.without9S$quad <- as.numeric(raw.data.without9S$quad)
raw.data.without9S$quad <- replace_na(raw.data.without9S$quad, 0)
first.pulls <- raw.data.without9S %>% filter(raw.data.without9S$quad != 2)

## Filtering for only alosine species (A. shad, blueback herring, alewife)
alosines.first.pulls <- first.pulls %>% filter(Species_name != "")

## Investigating spstatus designations ( 0 = combined, 1 = YOY, 2 = adult) by comparing distr. of fork lengths
## for each group
alewife <- alosines.first.pulls %>% filter(Species_name == "Alewife")
ggplot(alewife, aes(x=spstatus, y = FL_mm)) +
  geom_boxplot()

bbh <- alosines.first.pulls %>% filter(Species_name == "Blueback Herring")
ggplot(bbh, aes(x=spstatus, y = FL_mm)) +
  geom_boxplot()

a.shad <- alosines.first.pulls %>% filter(Species_name == "American Shad")
ggplot(a.shad, aes(x=spstatus, y = FL_mm)) +
  geom_boxplot()

## Keeping "combined" and "young of year" observations and dropping records for "adult" (124 rows)
#juvenile.alosines <- alosines.first.pulls %>% filter(spstatus != "2")

## Grouping by control3 (haul id (1 haul of seine = 1 unit of effort), species, and species status)
## and creating summary column of average CPUE
abundance.allgroups <- alosines.first.pulls %>% group_by(control3, date, Species_name, spstatus, location) %>%
  dplyr::select(control1, control3, month, colnum) %>%
  summarise(cpue = mean(colnum))

## Filtering for only juvenile alewife (will make separate index for combined group)
juv.alewife <- abundance.allgroups %>% filter(Species_name == "Alewife" &
                                           spstatus == "1")

abundance.juv.alewife <- juv.alewife %>% dplyr::select(1:6) %>% group_by(date, location) %>%
  summarise(juv.alewife.index = mean(cpue))

# ggplot(data = juv.alewife.abundance, aes(y= juv.alewife.index, x = date)) +
#   geom_line()

## Repeating process for combined (juvenile and adult) group
combined.alewife <- abundance.allgroups %>% filter(Species_name == "Alewife" &
                                                     spstatus == "0")
abundance.combined.alewife <- combined.alewife %>% dplyr::select(1:6) %>% group_by(date, location) %>%
  summarise(combined.alewife.index = mean(cpue))

## Repeating process for juvenile blueback herring
juv.blueback <- abundance.allgroups %>% filter(Species_name == "Blueback Herring" &
                                                 spstatus == "1")
abundance.juv.blueback <- juv.blueback %>% dplyr::select(1:6) %>% group_by(date, location) %>%
  summarise(juv.blueback.index = mean(cpue))

## Repeating process for combined blueback herring
combined.blueback <- abundance.allgroups %>% filter(Species_name == "Blueback Herring" &
                                                      spstatus == "0")
abundance.combined.blueback <- combined.blueback %>% dplyr::select(1:6) %>% group_by(date, location) %>%
  summarise(combined.blueback.index = mean(cpue))

## Repeating process for juvenile American shad
juv.A.shad <- abundance.allgroups %>% filter(Species_name == "American Shad" &
                                                 spstatus == "1")
abundance.juv.A.shad <- juv.A.shad %>% dplyr::select(1:6) %>% group_by(date, location) %>%
  summarise(juv.A.shad.index = mean(cpue))

## Repeating process for combined American shad
combined.A.shad <- abundance.allgroups %>% filter(Species_name == "American Shad" &
                                                    spstatus == "0")
abundance.combined.A.shad <- combined.A.shad %>% dplyr::select(1:6) %>% group_by(date, location) %>%
  summarise(combined.A.shad.index = mean(cpue))

## Creating data frame with one row for each haul (for each control1 code) to join abundance indices
abundance.df <- first.pulls %>% dplyr::select(control1, date, year, month, station, location, surtemp,
                                       bottemp, sursal, botsal, surdo, botdo)
abundance.df <- distinct(abundance.df)

## Joining abundance indices for each species/species status
abundance.df <- left_join(abundance.df, abundance.combined.A.shad, by = c("date","location"))
abundance.df <- left_join(abundance.df, abundance.combined.alewife, by = c("date","location"))
abundance.df <- left_join(abundance.df, abundance.combined.blueback, by = c("date","location"))
abundance.df <- left_join(abundance.df, abundance.juv.A.shad, by = c("date","location"))
abundance.df <- left_join(abundance.df, abundance.juv.alewife, by = c("date","location"))
abundance.df <- left_join(abundance.df, abundance.juv.blueback, by = c("date","location"))

## Replacing "NAs" with 0, under the assumption that for each haul an empty record indicates that
## none of that species were caught
abundance.df <- abundance.df %>% replace_na(list(combined.A.shad.index = 0,
                                         combined.alewife.index = 0,
                                         combined.blueback.index = 0,
                                         juv.A.shad.index = 0,
                                         juv.alewife.index = 0,
                                         juv.blueback.index = 0))

## Graphing abundance over time for each combination of species (A. shad, alewife, blueback herring) and
## species status (combined, juvenile) at all locations

ggplot(data = abundance.df, aes(x= date, y= juv.A.shad.index, color = station)) +
  geom_line()

ggplot(data = abundance.df, aes(x= date, y= juv.alewife.index, color = station)) +
  geom_line()

ggplot(data = abundance.df, aes(x= date, y= juv.blueback.index, color = station)) +
  geom_line()

## Grouping by year and month and adding column of average abundance index for each group in
## order to obtain aggregated abundance index for the entire sound
abundance.all.stations <- abundance.df %>% group_by(year, month) %>%
  summarise(juv.A.shad.index.ALL = mean(juv.A.shad.index),
            juv.alewife.index.ALL = mean(juv.alewife.index),
            juv.blueback.index.ALL = mean(juv.blueback.index),
            combined.A.shad.index.ALL = mean(combined.A.shad.index),
            combined.alewife.index.ALL = mean(combined.alewife.index),
            combined.blueback.index.ALL = mean(combined.blueback.index))
## Adding date column (arbitarily assigning first day of month)
abundance.all.stations$date <- as.Date(paste(abundance.all.stations$year,
                                             abundance.all.stations$month, 1, sep = "-"),
                                       format = "%Y-%m-%d")

## At this point, I have two data frames with abundance indices. One (abundance.df) contains abundance
## indices for each station by species and species group. The other contains combined abundance indices
## for each species/species group (a simple average across all stations for that species/species group
## for each month). For the first data frame, I can bind daily flow data with the indices for each station
## (possibly with a lag). For the other df, I can bind average monthly flows with the averaged abundance
## indices.

## Reading in Roanoke Rapids discharge data set
roanoke.rapids.discharge <- read.csv(file = "./Data/RawData/Roanoke rapids 1912 to June 2020.csv")

## Setting "date" column as date
roanoke.rapids.discharge$date <- as.Date(roanoke.rapids.discharge$date, format = "%m/%d/%y")

## Correcting 1900s date incorrectly set as 20xx
roanoke.rapids.discharge$date <- format(roanoke.rapids.discharge$date, "%y%m%d")

create.early.dates <- (function(d) {
  paste0(ifelse(as.numeric(rownames(roanoke.rapids.discharge)) > 32142,"20","19"),d)
})
roanoke.rapids.discharge$date <- create.early.dates(roanoke.rapids.discharge$date)

roanoke.rapids.discharge$date <- as.Date(roanoke.rapids.discharge$date, format = "%Y%m%d")

## Joining disharge data with station-specific abundance indices
flows.abundance.df <- left_join(abundance.df, roanoke.rapids.discharge, by = "date")

flows.test <- lm(flows.abundance.df$juv.A.shad.index ~ flows.abundance.df$avg.daily.flow)
summary(flows.test)

## Reading in raw data from USGS gage stations at Jamesville and Oak City
jamesville.gage <- read.csv(file = "./Data/RawData/jamesville_USGS_waterquality_raw.csv")
oakcity.gage <- read.csv(file = "./Data/RawData/OakCity_USGS_waterquality_raw.csv")

## Setting data classes
str(oakcity.gage)
oakcity.gage$site_no <- as.factor(oakcity.gage$site_no)
oakcity.gage$Date <- as.Date(oakcity.gage$Date, format = "%Y-%m-%d")
oakcity.gage$X_00095_00003 <- as.numeric(oakcity.gage$X_00095_00003)

str(jamesville.gage)
jamesville.gage$site_no <- as.factor(jamesville.gage$site_no)
jamesville.gage$Date <- as.Date(jamesville.gage$Date, format = "%Y-%m-%d")
jamesville.gage$X_00095_00003 <- as.numeric(jamesville.gage$X_00095_00003)

## Selecting only columns of interest and renaming
jamesville.gage <- jamesville.gage[,-c(1,2,3,6,7,8,10,12)]
jamesville.gage <- jamesville.gage %>% rename(temperature.jamesville = X_00010_00003)
jamesville.gage <- jamesville.gage %>% rename(specific.cond.jamesville = X_00095_00003)
jamesville.gage <- jamesville.gage %>% rename(DO.jamesville = X_00300_00003)
jamesville.gage <- jamesville.gage %>% rename(date = Date)

oakcity.gage <- oakcity.gage[,-c(1,2,3,6,7,8,10,12)]
oakcity.gage <- oakcity.gage %>% rename(temperature.oakcity = X_00010_00003)
oakcity.gage <- oakcity.gage %>% rename(specific.cond.oakcity = X_00095_00003)
oakcity.gage <- oakcity.gage %>% rename(DO.oakcity = X_00300_00003)
oakcity.gage <- oakcity.gage %>% rename(date = Date)

## Joining Jamesville and Oak City water quality data with set of other predictors
predictors.df <- left_join(flows.abundance.df, jamesville.gage, by = "date")
predictors.df <- left_join(predictors.df, oakcity.gage, by = "date")

## Reading in gage station data from Westover, NC (provided by Julie; contains water quality data)
westover.gage <- read.csv(file = "./Data/RawData/westoverNC.gage.csv")

## Setting data classes
str(westover.gage)
westover.gage$date <- as.Date(westover.gage$date, format = "%m/%d/%y")

## Renaming columns to indicate data is from Westover gage
westover.gage <- westover.gage[,-c(1,2)]
westover.gage <- westover.gage %>% rename(temp.C.max.top.westover = Temp.C.max.top)
westover.gage <- westover.gage %>% rename(temp.C.min.top.westover = Temp.C.min.top)
westover.gage <- westover.gage %>% rename(temp.C.mean.top.westover = Temp.C.mean.top)
westover.gage <- westover.gage %>% rename(temp.C.max.bottom.westover = Temp.C.max.bottom)
westover.gage <- westover.gage %>% rename(temp.C.min.bottom.westover = Temp.C.min.bottom)
westover.gage <- westover.gage %>% rename(temp.C.mean.bottom.westover = Temp.C.mean.bottom)

## Joining Westover water quality data with predictor data set
predictors.df <- left_join(predictors.df, westover.gage, by = "date")

## Arranging data set by date
predictors.df <- predictors.df %>% arrange(date)

## Organizing and cleaning data set to be exported to processed data file
predictors.df <- predictors.df[,-6]
predictors.df <- predictors.df[,c(2:5,1,6:30)]

write_csv(predictors.df, "./Data/ProcessedData/predictors.df.csv")

############### Creating abundance data set with all pulls ######################################
## Filtering for only alosine species (A. shad, blueback herring, alewife)
alosines.all.pulls <- raw.data.without9S %>% filter(Species_name != "")

## Grouping by control3 (haul id (1 haul of seine = 1 unit of effort), species, and species status)
## and creating summary column of average CPUE
ap.abundance.allgroups <- alosines.all.pulls %>% group_by(control3, date, Species_name, spstatus, location) %>%
  dplyr::select(control1, control3, month, colnum) %>%
  summarise(cpue = mean(colnum))

## Filtering for only juvenile alewife (will make separate index for combined group)
ap.juv.alewife <- ap.abundance.allgroups %>% filter(Species_name == "Alewife" &
                                                spstatus == "1")

ap.abundance.juv.alewife <- ap.juv.alewife %>% dplyr::select(1:6) %>% group_by(date, location) %>%
  summarise(juv.alewife.index = mean(cpue))

## Repeating process for combined (juvenile and adult) group
ap.combined.alewife <- ap.abundance.allgroups %>% filter(Species_name == "Alewife" &
                                                     spstatus == "0")
ap.abundance.combined.alewife <- ap.combined.alewife %>% dplyr::select(1:6) %>% group_by(date, location) %>%
  summarise(combined.alewife.index = mean(cpue))

## Repeating process for juvenile blueback herring
ap.juv.blueback <- ap.abundance.allgroups %>% filter(Species_name == "Blueback Herring" &
                                                 spstatus == "1")
ap.abundance.juv.blueback <- ap.juv.blueback %>% dplyr::select(1:6) %>% group_by(date, location) %>%
  summarise(juv.blueback.index = mean(cpue))

## Repeating process for combined blueback herring
ap.combined.blueback <- ap.abundance.allgroups %>% filter(Species_name == "Blueback Herring" &
                                                      spstatus == "0")
ap.abundance.combined.blueback <- ap.combined.blueback %>% dplyr::select(1:6) %>% group_by(date, location) %>%
  summarise(combined.blueback.index = mean(cpue))

## Repeating process for juvenile American shad
ap.juv.A.shad <- ap.abundance.allgroups %>% filter(Species_name == "American Shad" &
                                               spstatus == "1")
ap.abundance.juv.A.shad <- ap.juv.A.shad %>% dplyr::select(1:6) %>% group_by(date, location) %>%
  summarise(juv.A.shad.index = mean(cpue))

## Repeating process for combined American shad
ap.combined.A.shad <- ap.abundance.allgroups %>% filter(Species_name == "American Shad" &
                                                    spstatus == "0")
ap.abundance.combined.A.shad <- ap.combined.A.shad %>% dplyr::select(1:6) %>% group_by(date, location) %>%
  summarise(combined.A.shad.index = mean(cpue))

## Creating data frame with one row for each haul (for each control1 code) to join abundance indices
ap.abundance.df <- raw.data.without9S %>% dplyr::select(control1, date, year, month, station, location, surtemp,
                                              bottemp, sursal, botsal, surdo, botdo)
ap.abundance.df <- distinct(ap.abundance.df)

## Joining abundance indices for each species/species status
ap.abundance.df <- left_join(ap.abundance.df, ap.abundance.combined.A.shad, by = c("date","location"))
ap.abundance.df <- left_join(ap.abundance.df, ap.abundance.combined.alewife, by = c("date","location"))
ap.abundance.df <- left_join(ap.abundance.df, ap.abundance.combined.blueback, by = c("date","location"))
ap.abundance.df <- left_join(ap.abundance.df, ap.abundance.juv.A.shad, by = c("date","location"))
ap.abundance.df <- left_join(ap.abundance.df, ap.abundance.juv.alewife, by = c("date","location"))
ap.abundance.df <- left_join(ap.abundance.df, ap.abundance.juv.blueback, by = c("date","location"))

## Replacing "NAs" with 0, under the assumption that for each haul an empty record indicates that
## none of that species were caught
ap.abundance.df <- ap.abundance.df %>% replace_na(list(combined.A.shad.index = 0,
                                                 combined.alewife.index = 0,
                                                 combined.blueback.index = 0,
                                                 juv.A.shad.index = 0,
                                                 juv.alewife.index = 0,
                                                 juv.blueback.index = 0))

ggplot(data = abundance.df, aes(y = abundance.df$juv.A.shad, x = date)) + geom_line()
ggplot(data = ap.abundance.df, aes(y = ap.abundance.df$juv.A.shad.index, x = date)) + geom_line()

### *** For data exploration script

## Graphing aggregated abundance over time for each species/species group
ggplot(data = abundance.all.stations, aes(x = date,
                                          y = abundance.all.stations$juv.A.shad.index.ALL)) +
  geom_line()
ggplot(data = abundance.all.stations, aes(x = date,
                                          y = abundance.all.stations$juv.alewife.index.ALL)) +
  geom_line()
ggplot(data = abundance.all.stations, aes(x = date,
                                          y = abundance.all.stations$juv.blueback.index.ALL)) +
  geom_line()
ggplot(data = abundance.all.stations, aes(x = date,
                                          y = abundance.all.stations$combined.A.shad.index.ALL)) +
  geom_line()
ggplot(data = abundance.all.stations, aes(x = date,
                                          y = abundance.all.stations$combined.alewife.index.ALL)) +
  geom_line()
ggplot(data = abundance.all.stations, aes(x = date,
                                          y = abundance.all.stations$combined.blueback.index.ALL)) +
  geom_line()

## Graphing bottom temperature (degrees Celsius) over time
ggplot(data = abundance.df, aes(x= date, y = bottemp)) +
  geom_line()

## Graphing bottom temp data missingness over time
ggplot(data = subset(abundance.df, is.na(bottemp)), aes(x = date)) +
  geom_histogram()

## Trying out nonmetric MDS
library(MASS)
test <- predictors.df[,6:30]
d <- dist(test)
fit <- isoMDS(d, )


