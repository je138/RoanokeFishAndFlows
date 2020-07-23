getwd()
library(tidyverse)
library(vegan)
library(grid)

## Reading in Project 100 data from NC Division of Marine Fisheries
raw.DMF.data <- read.csv("./Data/RawData/P100_7219_EYNON.CSV")

## Checking structure a resetting data classes
str(raw.DMF.data)
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
## Filtering for stations sampled with river herring seine survey (eleven in total; 48S, 56S, 47S, 46S, 128S, 85S,
## 84S, 126S, 39S, 130S, and 127S)
river.herring.stations <- raw.DMF.data %>% filter(station == "48S" | station == "56S" | station == "47S" |
                                                    station == "46S" | station == "84S" | station == "126S" |
                                                    station == "39S" | station == "130S" | station == "127S" |
                                                    station == "128S")
river.herring.stations$quad <- as.factor(river.herring.stations$quad)

quad2 <- river.herring.stations %>% filter(quad == "2")
## Average day of month for quad 2 (not first pull of month) is approx. 19.5;
## for first pulls (quad = NA or 1) = 13.4
mean(quad2$day)
mean(river.herring.stations$day)

## Are some stations sampled multiple times per month? Yes, 1.3 times on avg. over entire time series and stations
sampling.dates <- river.herring.stations %>% select("control1", "station", "year", "month", "day") %>%
  group_by(station, year, month, day)
sampling.dates <- distinct(sampling.dates)
sampling.dates <- sampling.dates %>% group_by(station, year, month) %>% add_tally()
sampling.dates <- sampling.dates[,c(2:4,6)]
sampling.dates <- distinct(sampling.dates)
avg.times.sampled <- sampling.dates %>% group_by(year) %>% summarise(avg.samples = mean(n))
mean(avg.times.sampled$avg.samples)

## Are some stations sampled more than others? Yes, roughly, with 46S sampled more often and 56S less often
sampling.stations <- river.herring.stations %>% select("control1", "station", "year", "month", "day") %>%
  group_by(station, year, month, day)
sampling.stations <- distinct(sampling.stations)
ggplot(data = sampling.stations, aes(x = station)) +
  geom_bar()

## Will have one data set with weekly resolution including all pulls (not only first pulls of the month)
## Another data set of only first pulls to calculate monthly index

##### Monthly abundance - only first pulls
# Filtering for only first pulls of each month; first replacing NAs with "0"
river.herring.stations$quad <- as.numeric(river.herring.stations$quad)
river.herring.stations$quad <- replace_na(river.herring.stations$quad, 0)
river.herring.stations.fp <- river.herring.stations %>% filter(quad != "2")

## Filtering for only alosine species (a. shad, blueback herring, alewife, and hickory shad)
alosines.fp <- river.herring.stations.fp %>% filter(Species_name != "" |
                                                      species == "8747010103")

# ## Investigating number of sample groups with FL over 100mm
# over100FL.alosines <- alosines.fp %>% filter(FL_mm > 100)

## Adding species name to hickory shad
alosines.fp$Species_name <- as.character(alosines.fp$Species_name)
alosines.fp$Species_name[alosines.fp$Species_name == ""] <- "Hickory Shad"
alosines.fp$Species_name <- as.factor(alosines.fp$Species_name)

## Calculating abundance indices; grouping by control1 (haul id), date, location, and species
## Creating summary column of average CPUE (JAI)
alosine.abundance.all <- alosines.fp %>% group_by(control1, date, Species_name, spstatus, location) %>%
  dplyr::select(control1, date, location, spstatus, colnum) %>%
  summarise(cpue = mean(colnum))  # this eliminated duplicate rows (same count listed for each sample group)

# Next, finding sum of species group counts for each haul to find catch per each unit effort (CPUE/JAI)
alosine.abundance.all <- alosine.abundance.all %>% group_by(control1, date, Species_name, location) %>%
  dplyr::select(control1, date, Species_name, location, cpue) %>% summarise(JAI = sum(cpue))

## Creating JAI panels for each species to join with full predictor dataset. If no record for species on day
## with haul, assume JAI = 0.

JAI.alewife <- alosine.abundance.all %>% filter(Species_name == "Alewife") %>% rename(alewife.JAI = JAI)
JAI.blueback <- alosine.abundance.all %>% filter(Species_name == "Blueback Herring") %>% rename(blueback.herring.JAI = JAI)
JAI.a.shad <- alosine.abundance.all %>% filter(Species_name == "American Shad") %>% rename(american.shad.JAI = JAI)
JAI.hickory.shad <- alosine.abundance.all %>% filter(Species_name == "Hickory Shad") %>% rename(hickory.shad.JAI = JAI)

## Creating data frame with one row for each haul (for each control1 code) to join abundance indices
abundance.df <- river.herring.stations.fp %>% dplyr::select(control1, date, year, month, station, location, surtemp,
                                              bottemp, sursal, botsal, surdo, botdo)
abundance.df <- distinct(abundance.df)

## Joining abundance indices for each species
abundance.df <- left_join(abundance.df, JAI.a.shad, by = c("control1","date","location"))
abundance.df <- left_join(abundance.df, JAI.alewife, by = c("control1","date","location"))
abundance.df <- left_join(abundance.df, JAI.blueback, by = c("control1","date","location"))
abundance.df <- left_join(abundance.df, JAI.hickory.shad, by = c("control1","date","location"))

## Removing duplicate column names
abundance.df <- abundance.df %>% select(-c("Species_name.x", "Species_name.y", "Species_name.x.x", "Species_name.y.y"))

## Replacing "NAs" with 0, under the assumption that for each haul an empty record indicates that
## none of that species were caught
abundance.df <- abundance.df %>% replace_na(list(american.shad.JAI = 0,
                                                 alewife.JAI = 0,
                                                 blueback.herring.JAI = 0,
                                                 hickory.shad.JAI = 0))

write.csv(abundance.df, file = "./Data/ProcessedData/AlosineAbundance_daily.csv")
write.csv(monthly.abundance, "./Data/ProcessedData/AlosineAbundance_monthly.csv")

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
jamesville.gage <- jamesville.gage[,-c(1,2,3,6,8,10)]
jamesville.gage <- jamesville.gage %>% rename(temperature.jamesville = X_00010_00003)
jamesville.gage <- jamesville.gage %>% rename(specific.cond.jamesville = X_00095_00003)
jamesville.gage <- jamesville.gage %>% rename(DO.jamesville = X_00300_00003)
jamesville.gage <- jamesville.gage %>% rename(date = Date)

oakcity.gage <- oakcity.gage[,-c(1,2,3,6,8,10)]
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

# haul.dates <- predictors.df %>% select(c(2)) %>% mutate(haul = "1")
# flow.w.haul.dates <-left_join(roanoke.rapids.discharge, haul.dates, by = c("date"))
# flow.w.haul.dates$avg.weekly.flow <- sapply(flow.w.haul.dates$date,
#   function(x)mean(flow.w.haul.dates[difftime(flow.w.haul.dates$date,x,,"days") %in%
#                                      -1:-7,]$avg.daily.flow))
# flow.w.haul.dates$max.weekly.flow <- sapply(flow.w.haul.dates$date,
#                           function(x)max(flow.w.haul.dates[difftime(flow.w.haul.dates$date,x,,"days") %in%
#                           -1:-7,]$avg.daily.flow))
# flow.w.haul.dates$median.weekly.flow <- sapply(flow.w.haul.dates$date,
#                               function(x)median(flow.w.haul.dates[difftime(flow.w.haul.dates$date,x,,"days") %in%
#                                                               -1:-7,]$avg.daily.flow))
# ## Filtering for only haul dates (dates for which we have abundance data) and joining w/ predictor frame
# weekly.flow.stats <- flow.w.haul.dates %>% filter(haul == "1")
# weekly.flow.stats <- weekly.flow.stats %>% select(-c("haul"))
# predictors.df <- left_join(predictors.df, weekly.flow.stats, by = c("date","avg.daily.flow"))
# 
# test <- lm(predictors.df$hickory.shad.JAI ~ predictors.df$avg.weekly.flow)
# summary(test)

## Putting together weekly stats for DO
# water.quality <- left_join(jamesville.gage, oakcity.gage, by = "date")
# wq.w.haul.dates <- left_join(water.quality, haul.dates, by = "date")
# wq.w.haul.dates <-predictors.df %>% select(c("date", "DO.jamesville", "DO.oakcity"))
# wq.w.haul.dates$avg.weekly.DO.jamesville <- sapply(wq.w.haul.dates$date,
#                             function(x)mean(wq.w.haul.dates[difftime(wq.w.haul.dates$date,x,,"days") %in%
#                             -1:-7,]$DO.jamesville))
# wq.w.haul.dates$avg.weekly.DO.oakcity <- sapply(wq.w.haul.dates$date,
#                             function(x)mean(wq.w.haul.dates[difftime(wq.w.haul.dates$date,x,,"days") %in%
#                            -1:-7,]$DO.oakcity))
# wq.w.haul.dates$min.weekly.DO.jamesville <- sapply(wq.w.haul.dates$date,
#                          function(x)min(wq.w.haul.dates[difftime(wq.w.haul.dates$date,x,,"days") %in%
#                         -1:-7,]$DO.jamesville))
# wq.w.haul.dates$min.weekly.DO.oakcity <- sapply(wq.w.haul.dates$date,
#                         function(x)min(wq.w.haul.dates[difftime(wq.w.haul.dates$date,x,,"days") %in%
#                         -1:-7,]$DO.oakcity))
# weekly.DO.stats <- wq.w.haul.dates %>% filter(haul == "1")
# weekly.DO.stats <- weekly.DO.stats %>% select(-c("haul"))
# weekly.DO.stats <- weekly.DO.stats[-c(1:6),]
# 
# predictors.df <- left_join(predictors.df, weekly.DO.stats, by = "date")

## Filtering for only station closest to Roanoke
roanoke.station <- predictors.df %>% filter(station == "128S")
unique(predictors.df$station)

test <- lm(data = roanoke.station, american.shad.JAI ~ avg.weekly.flow)
summary(test)

##################################################################################################################################
##################################################################################################################################
## Monthly panel

monthly.abundance <- predictors.df %>% group_by(year, month) %>%
  select("year", "month", "date", "american.shad.JAI", "alewife.JAI", "blueback.herring.JAI", "hickory.shad.JAI") %>%
  summarise(monthly.american.shad.JAI = mean(american.shad.JAI),
            monthly.alewife.JAI = mean(alewife.JAI),
            monthly.blueback.herring.JAI = mean(blueback.herring.JAI),
            monthly.hickory.shad.JAI = mean(hickory.shad.JAI))
monthly.abundance$date <- as.Date(paste(monthly.abundance$year,
                 monthly.abundance$month, 1, sep = "-"),
           format = "%Y-%m-%d")

ggplot(data = monthly.abundance, aes(group = month, x=month, y = monthly.american.shad.JAI)) +
  geom_boxplot() +
  scale_y_log10()
ggplot(data = monthly.abundance, aes(group = month, x=month, y = monthly.alewife.JAI)) +
  geom_boxplot() +
  scale_y_log10()
ggplot(data = monthly.abundance, aes(group = month, x=month, y = monthly.blueback.herring.JAI)) +
  geom_boxplot() +
  scale_y_log10()

ggplot(data = monthly.abundance, aes(x = month)) +
  geom_histogram()

## Filtering for only June - October, which were regularly sampled and for which alosine abundance is
## significantly higher than for other months
monthly.abundance_june.thru.nov <- monthly.abundance %>% filter(month %in% c(6:11))
## blueback still present in Sound into November

ggplot(data = monthly.abundance, aes(x = date, y = monthly.american.shad.JAI)) +
  geom_line(color = "blue", alpha = 0.5) +
  geom_line(aes(y= monthly.alewife.JAI), color = "red", alpha = 0.5) +
  geom_line(aes(y= monthly.hickory.shad.JAI), color = "darkgreen", alpha = 0.5) +
    scale_x_date(breaks = scales::pretty_breaks(40)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_line(aes(y= monthly.blueback.herring.JAI), color = "purple", alpha = 0.5)

# ggplot(data = monthly.abundance, aes(x = date, y = monthly.american.shad.JAI)) +
#   geom_line() +
#   scale_x_date(breaks = scales::pretty_breaks(40)) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# ggplot(data = predictors.df, aes(x = date, y = avg.daily.flow)) +
#   geom_line() +
#   scale_x_date(breaks = scales::pretty_breaks(40)) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

##############################################################################################################
##############################################################################################################
##############################################################################################################
## Creating environmental matrix

#### Importing median monthly flows
median.monthly.flow.raw <- read.csv("./Data/IHA Tables/Ready to import/MonthlyMedianFlow.csv")
median.monthly.flow.raw <- median.monthly.flow.raw[,-c(8:13)]
## Renaming columns
data.table::setnames(median.monthly.flow.raw, old = c("Year","March", "April", "May", "June", "July", "August"), 
                     new = c('year','3','4','5','6','7','8'))
## Converting from wide to long format and arranging by year and month
 median.monthly.flow <- gather(median.monthly.flow.raw, month, median.monthly.flow, "3":"8", factor_key = TRUE)
median.monthly.flow <- median.monthly.flow %>% arrange(year, month)

#### Importing max monthly flow data
max.monthly.flow.raw <- read.csv("./Data/IHA Tables/Ready to import/MonthlyMaxFlow.csv")
max.monthly.flow.raw <- max.monthly.flow.raw[,-c(8:13)]
## Renaming columns
data.table::setnames(max.monthly.flow.raw, old = c("Year","March", "April", "May", "June", "July", "August"), 
                     new = c('year','3','4','5','6','7','8'))
## Converting from wide to long format
max.monthly.flow <- gather(max.monthly.flow.raw, month, max.monthly.flow, "3":"8", factor_key = TRUE)
max.monthly.flow <- max.monthly.flow %>% arrange(year, month)

#### Importing monthly fall rate data
monthly.fall.rate.raw <- read.csv("./Data/IHA Tables/Ready to import/MonthlyFallRate.csv")
monthly.fall.rate.raw <- monthly.fall.rate.raw[,-c(8:13)]
## Renaming columns
data.table::setnames(monthly.fall.rate.raw, old = c("Year","March", "April", "May", "June", "July", "August"), 
                     new = c('year','3','4','5','6','7','8'))
## Converting from wide to long format
monthly.fall.rate <- gather(monthly.fall.rate.raw, month, monthly.fall.rate, "3":"8", factor_key = TRUE)
monthly.fall.rate <- monthly.fall.rate %>% arrange(year, month)
# Converting negative rates to positive values (mult. by -1)
monthly.fall.rate <- monthly.fall.rate %>% mutate(monthly.fall.rate = (-1)*monthly.fall.rate)

#### Importing monthly rise rate data
monthly.rise.rate.raw <- read.csv("./Data/IHA Tables/Ready to import/MonthlyRiseRate.csv")
monthly.rise.rate.raw <- monthly.rise.rate.raw[,-c(8:13)]
## Renaming columns
data.table::setnames(monthly.rise.rate.raw, old = c("Year","March", "April", "May", "June", "July", "August"), 
                     new = c('year','3','4','5','6','7','8'))
## Converting from wide to long format
monthly.rise.rate <- gather(monthly.rise.rate.raw, month, monthly.rise.rate, "3":"8", factor_key = TRUE)
monthly.rise.rate <- monthly.rise.rate %>% arrange(year, month)

#### Importing peak timing (julian date of max flow for each month) data
peak.flow.timing.raw <- read.csv("./Data/IHA Tables/Ready to import/MonthlyMaxFlowDate.csv")
peak.flow.timing.raw <- peak.flow.timing.raw[,-c(8:13)]
## Renaming columns
data.table::setnames(peak.flow.timing.raw, old = c("Year","March", "April", "May", "June", "July", "August"), 
                     new = c('year','3','4','5','6','7','8'))
## Converting from wide to long format
peak.flow.timing <- gather(peak.flow.timing.raw, month, date.of.monthly.max.flow, "3":"8", factor_key = TRUE)
peak.flow.timing <- peak.flow.timing %>% arrange(year, month)

#### Importing monthly reversals data
monthly.reversals.raw <- read.csv("./Data/IHA Tables/Ready to import/MonthlyReversals.csv")
## Renaming columns
data.table::setnames(monthly.reversals.raw, old = c("Year","March", "April", "May", "June", "July", "August"), 
                     new = c('year','3','4','5','6','7','8'))
## Converting from wide to long format
monthly.reversals <- gather(monthly.reversals.raw, month, monthly.reversals, "3":"8", factor_key = TRUE)
monthly.reversals <- monthly.reversals %>% arrange(year, month)

## Importing monthly inflow data
monthly.inflows.raw <- read.csv("./Data/IHA Tables/Ready to import/MonthlyInflows.csv")
# Setting date column as.date
monthly.inflows.raw$date <- as.Date(monthly.inflows.raw$date, format = "%m/%d/%y")
## Correcting 1900s date incorrectly set as 20xx
monthly.inflows.raw$date <- format(monthly.inflows.raw$date, "%y%m%d")

create.early.dates2 <- (function(d) {
  paste0(ifelse(as.numeric(rownames(monthly.inflows.raw)) > 17350,"20","19"),d)
})
monthly.inflows.raw$date <- create.early.dates2(monthly.inflows.raw$date)

monthly.inflows.raw$date <- as.Date(monthly.inflows.raw$date, format = "%Y%m%d")

monthly.inflows.raw <- monthly.inflows.raw %>% mutate(month = 
                                                format(as.Date(monthly.inflows.raw$date), "%m"),
                                              year = format(as.Date(monthly.inflows.raw$date), "%Y"))
monthly.inflows.raw$month <- as.numeric(monthly.inflows.raw$month)

monthly.inflows <- monthly.inflows.raw %>% group_by(year, month) %>%
  summarise(monthly.inflows = mean(inflow))
monthly.inflows$year <- as.numeric(monthly.inflows$year)
monthly.inflows$month <- as.factor(monthly.inflows$month)

## Creating column with dummy variable for inflows (precipitation) above 75th percentile for that month
inflow.quartiles <- monthly.inflows
inflow.75pctls <- monthly.inflows %>% group_by(month) %>% summarise(pctl.75 = quantile(monthly.inflows,
                                                                                         probs = 0.75))
inflow.25pctls <- monthly.inflows %>% group_by(month) %>% summarise(pctl.25 = quantile(monthly.inflows,
                                                                                       probs = 0.25))
inflow.quartiles <- left_join(inflow.quartiles, inflow.75pctls, by = "month")
inflow.quartiles <- left_join(inflow.quartiles, inflow.25pctls, by = "month")

inflow.quartiles <- inflow.quartiles %>% mutate(inflows.upperquartile =
                                                  ifelse(monthly.inflows > pctl.75, 1, 0))
inflow.quartiles <- inflow.quartiles %>% mutate(inflows.lowerquartile = 
                                                  ifelse(monthly.inflows < pctl.25, 1, 0))
inflow.quartiles <- inflow.quartiles[,-c(3:5)]

# quantile(monthly.inflows$monthly.inflows)
# # 0%        25%        50%        75%       100% 
# # 592.7667  3114.5785  5535.7387 10001.7177 37563.6000 
# inflows.upperquartile <- monthly.inflows %>% mutate(inflows.upperquartile =
#                                                       ifelse(monthly.inflows > 10001.7177,
#                                                              1,0))
# inflows.upperquartile <- inflows.upperquartile[,-3]
# inflows.lowerquartile <- monthly.inflows %>% mutate(inflows.lowerquartile =
#                                                       ifelse(monthly.inflows < 3114.5785,
#                                                              1,0))
# inflows.lowerquartile <- inflows.lowerquartile[,-3]


environmental.matrix <- left_join(median.monthly.flow, max.monthly.flow, by = c("year", "month"))
environmental.matrix <- left_join(environmental.matrix, peak.flow.timing, by = c("year", "month"))
environmental.matrix <- left_join(environmental.matrix, monthly.fall.rate, by = c("year", "month"))
environmental.matrix <- left_join(environmental.matrix, monthly.rise.rate, by = c("year", "month"))
environmental.matrix <- left_join(environmental.matrix, monthly.reversals, by = c("year", "month"))
environmental.matrix <- left_join(environmental.matrix, monthly.inflows, by = c("year", "month"))
environmental.matrix <- left_join(environmental.matrix, inflow.quartiles, by = c("year", "month"))

environmental.matrix$date <- as.Date(paste(environmental.matrix$year,
                                           environmental.matrix$month,
                                           1, sep = "-"),
                                           format = "%Y-%m-%d")

## Making vector with minimum DO for both Oak City and Jamesville gages
minDO.oakcity <- predictors.df %>% group_by(year, month) %>% summarize(minDO.oakcity = min(DO.oakcity))
minDO.oakcity$month <- as.factor(minDO.oakcity$month)
minDO.jamesville <- predictors.df %>% group_by(year, month) %>% summarize(minDO.jamesville = min(DO.jamesville))
minDO.jamesville$month <- as.factor(minDO.jamesville$month)

environmental.matrix <- left_join(environmental.matrix, minDO.oakcity, by = c("year", "month"))
environmental.matrix <- left_join(environmental.matrix, minDO.jamesville, by = c("year", "month"))
environmental.matrix$year <- as.factor(environmental.matrix$year)
environmental.matrix$month <- as.numeric(environmental.matrix$month)

## Creating columns for number of days over 10k, 15k, 20k, and 30k
rr.discharge_monthly.counts <- roanoke.rapids.discharge %>% mutate(month = 
                                                format(as.Date(roanoke.rapids.discharge$date), "%m"),
                                                year = format(as.Date(roanoke.rapids.discharge$date), "%Y"))
rr.discharge_monthly.counts$month <- as.numeric(rr.discharge_monthly.counts$month)

rr.discharge_count.over10k <- rr.discharge_monthly.counts %>% group_by(month, year) %>%
  tally(avg.daily.flow > 10000)
rr.discharge_count.over10k <- rr.discharge_count.over10k %>% rename(days.over.10k = n)

rr.discharge_count.over15k <- rr.discharge_monthly.counts %>% group_by(month, year) %>%
  tally(avg.daily.flow > 15000)
rr.discharge_count.over15k <- rr.discharge_count.over15k %>% rename(days.over.15k = n)

rr.discharge_count.over20k <- rr.discharge_monthly.counts %>% group_by(month, year) %>%
  tally(avg.daily.flow > 20000)
rr.discharge_count.over20k <- rr.discharge_count.over20k %>% rename(days.over.20k = n)

rr.discharge_count.over30k <- rr.discharge_monthly.counts %>% group_by(month, year) %>%
  tally(avg.daily.flow > 30000)
rr.discharge_count.over30k <- rr.discharge_count.over30k %>% rename(days.over.30k = n)

environmental.matrix <- left_join(environmental.matrix, rr.discharge_count.over10k, by = c("month", "year"))
environmental.matrix <- left_join(environmental.matrix, rr.discharge_count.over15k, by = c("month", "year"))
environmental.matrix <- left_join(environmental.matrix, rr.discharge_count.over20k, by = c("month", "year"))
environmental.matrix <- left_join(environmental.matrix, rr.discharge_count.over30k, by = c("month", "year"))


monthly.abundance_june.thru.nov$month <- factor(monthly.abundance_june.thru.nov$month,
                                                levels = c("6","7","8","9","10","11"))



## Formatting environmental matrix to match dimensions of abundance matrix; trying 3 month lag
# date.of.monthly.max <- environmental.matrix[,c(1:2,5)]
environmental.matrix.3monthlag.allvars <- environmental.matrix %>% mutate(dummy.month = month + 3)
environmental.matrix.3monthlag.allvars$date <- as.Date(paste(environmental.matrix.3monthlag.allvars$year,
                                                             environmental.matrix.3monthlag.allvars$dummy.month,
                                                             1, sep = "-"),
                                                       format = "%Y-%m-%d")
environmental.matrix.3monthlag.allvars.join <- environmental.matrix.3monthlag.allvars
environmental.matrix.3monthlag.allvars <- semi_join(environmental.matrix.3monthlag.allvars,
                                                    monthly.abundance_june.thru.nov, by = "date")
environmental.matrix.3monthlag.allvars <- environmental.matrix.3monthlag.allvars %>% arrange(date)
environmental.matrix.3monthlag.allvars <- environmental.matrix.3monthlag.allvars %>%
  select(-c("year","month",
            "dummy.month", "date"))

## Formatting abundance matrix for CCA/RDA models
monthly.abundance_6_10 <- monthly.abundance_june.thru.nov %>% filter(month != 11)
monthly.abundance_6_9 <- monthly.abundance_june.thru.nov %>% filter(month != 11 & month != 10)

monthly.abundance_june.thru.nov <- monthly.abundance_june.thru.nov %>% arrange(date)
monthly.abundance_june.thru.nov.CCA <- monthly.abundance_june.thru.nov
monthly.abundance_june.thru.nov <- monthly.abundance_june.thru.nov[,-c(1:2,7)]

## Formatting classes of environmental matrix
str(environmental.matrix.3monthlag.allvars)
environmental.matrix.3monthlag.allvars$median.monthly.flow <- 
  as.numeric(environmental.matrix.3monthlag.allvars$median.monthly.flow)
environmental.matrix.3monthlag.allvars$max.monthly.flow <- 
  as.numeric(environmental.matrix.3monthlag.allvars$max.monthly.flow)
environmental.matrix.3monthlag.allvars$date.of.monthly.max.flow <- 
  as.numeric(environmental.matrix.3monthlag.allvars$date.of.monthly.max.flow)
environmental.matrix.3monthlag.allvars$days.over.10k <- 
  as.numeric(environmental.matrix.3monthlag.allvars$days.over.10k)
environmental.matrix.3monthlag.allvars$days.over.15k <- 
  as.numeric(environmental.matrix.3monthlag.allvars$days.over.15k)
environmental.matrix.3monthlag.allvars$days.over.20k <- 
  as.numeric(environmental.matrix.3monthlag.allvars$days.over.20k)
environmental.matrix.3monthlag.allvars$days.over.30k <- 
  as.numeric(environmental.matrix.3monthlag.allvars$days.over.30k)
environmental.matrix.3monthlag.allvars$monthly.rise.rate <- 
  as.numeric(environmental.matrix.3monthlag.allvars$monthly.rise.rate)
environmental.matrix.3monthlag.allvars$monthly.reversals <- 
  as.numeric(environmental.matrix.3monthlag.allvars$monthly.reversals)
environmental.matrix.3monthlag.allvars$monthly.inflows <- 
  as.numeric(environmental.matrix.3monthlag.allvars$monthly.inflows)
environmental.matrix.3monthlag.allvars$inflows.upperquartile <- 
  as.numeric(environmental.matrix.3monthlag.allvars$inflows.upperquartile)
environmental.matrix.3monthlag.allvars$inflows.lowerquartile <- 
  as.numeric(environmental.matrix.3monthlag.allvars$inflows.lowerquartile)

## RDA model test
all.equal(rownames(monthly.abundance_june.thru.nov), rownames(environmental.matrix.3monthlag.allvars))

test <- monthly.abundance_june.thru.nov/rowSums(monthly.abundance_june.thru.nov)

rda_tree <- rda(monthly.abundance_june.thru.nov ~ environmental.matrix.3monthlag.allvars$median.monthly.flow +
                  environmental.matrix.3monthlag.allvars$max.monthly.flow + 
                  environmental.matrix.3monthlag.allvars$date.of.monthly.max.flow +
                  environmental.matrix.3monthlag.allvars$monthly.fall.rate)
rda_tree
RsquareAdj(rda_tree)
plot(rda_tree, type='n', scaling=1)
orditorp(rda_tree, display='sp', cex=0.5, scaling=1, col='blue')
text(rda_tree, display='cn', col='red')

rda.test.3monthlag.oneVar <- rda(monthly.abundance_june.thru.nov ~ 
                                   environmental.matrix.3monthlag.allvars$inflows.upperquartile)
rda.test.3monthlag.oneVar
RsquareAdj(rda.test.3monthlag.oneVar)
plot(rda.test.3monthlag.oneVar, type='n', scaling=1)
orditorp(rda.test.3monthlag.oneVar, display='sp', cex=0.5, scaling=1, col='blue')
text(rda.test.3monthlag.oneVar, display='cn', col='red')
## Adj R2 of each RDA model with only one environmental gradient predictor
#** Fall rate adj R2 = 0.06069033
# Median monthly flow = -0.001742603
# Max monthly flow = 0.006987636
# Date of max monthly flow = -0.003766953
#** Monthly rise rate = 0.08357967
# Count over 10k = 0.003507972
# Count over 15k = -0.003439303
# Count over 20k = -0.002214176
# Count over 30k = -0.003053523
# Monthly reversals = 0.001067839
# Monthly inflows = 0.005165145
# Inflows (upper qtl) = -0.001316689 --> removed
#** Inflows (lower qtl) = 0.01018429 --> removed
# New inflow lower quartile (by month) = -0.00313316
#** New inflow upper quartile (by month) = 0.02518791


## Formatting environmental matrix to match dimensions of abundance matrix; trying 2 month lag
environmental.matrix.2monthlag.allvars <- environmental.matrix %>% mutate(dummy.month = month + 2)
environmental.matrix.2monthlag.allvars$date <- as.Date(paste(environmental.matrix.2monthlag.allvars$year,
                                                             environmental.matrix.2monthlag.allvars$dummy.month,
                                                             1, sep = "-"),
                                                       format = "%Y-%m-%d")
environmental.matrix.2monthlag.allvars.join <- environmental.matrix.2monthlag.allvars 
environmental.matrix.2monthlag.allvars <- semi_join(environmental.matrix.2monthlag.allvars,
                                                    monthly.abundance_6_10, by = "date")
environmental.matrix.2monthlag.allvars <- environmental.matrix.2monthlag.allvars %>% arrange(date)
monthly.abundance_6_10 <- monthly.abundance_6_10 %>% arrange(date)
monthly.abundance_6_10 <- monthly.abundance_6_10[,-c(1:2,7)]
environmental.matrix.2monthlag.allvars <- environmental.matrix.2monthlag.allvars %>%
  select(-c("year","month", "dummy.month", "date"))

## Formatting classes of environmental matrix
str(environmental.matrix.2monthlag.allvars)
environmental.matrix.2monthlag.allvars$median.monthly.flow <- 
  as.numeric(environmental.matrix.2monthlag.allvars$median.monthly.flow)
environmental.matrix.2monthlag.allvars$max.monthly.flow <- 
  as.numeric(environmental.matrix.2monthlag.allvars$max.monthly.flow)
environmental.matrix.2monthlag.allvars$date.of.monthly.max.flow <- 
  as.numeric(environmental.matrix.2monthlag.allvars$date.of.monthly.max.flow)
environmental.matrix.2monthlag.allvars$days.over.10k <- 
  as.numeric(environmental.matrix.2monthlag.allvars$days.over.10k)
environmental.matrix.2monthlag.allvars$days.over.15k <- 
  as.numeric(environmental.matrix.2monthlag.allvars$days.over.15k)
environmental.matrix.2monthlag.allvars$days.over.20k <- 
  as.numeric(environmental.matrix.2monthlag.allvars$days.over.20k)
environmental.matrix.2monthlag.allvars$days.over.30k <- 
  as.numeric(environmental.matrix.2monthlag.allvars$days.over.30k)
environmental.matrix.2monthlag.allvars$monthly.rise.rate <- 
  as.numeric(environmental.matrix.2monthlag.allvars$monthly.rise.rate)
environmental.matrix.2monthlag.allvars$monthly.reversals <- 
  as.numeric(environmental.matrix.2monthlag.allvars$monthly.reversals)
environmental.matrix.2monthlag.allvars$monthly.inflows <- 
  as.numeric(environmental.matrix.2monthlag.allvars$monthly.inflows)
environmental.matrix.2monthlag.allvars$inflows.upperquartile <- 
  as.numeric(environmental.matrix.2monthlag.allvars$inflows.upperquartile)
environmental.matrix.2monthlag.allvars$inflows.lowerquartile <- 
  as.numeric(environmental.matrix.2monthlag.allvars$inflows.lowerquartile)

## New RDA test (2 month lag)
rda_tree.2monthlag <- rda(monthly.abundance_6_10 ~ environmental.matrix.2monthlag.allvars$median.monthly.flow +
                         environmental.matrix.2monthlag.allvars$max.monthly.flow +
                         environmental.matrix.2monthlag.allvars$date.of.monthly.max.flow +
                         environmental.matrix.2monthlag.allvars$monthly.fall.rate)
rda_tree.2monthlag
RsquareAdj(rda_tree.2monthlag)
plot(rda_tree.2monthlag, type='n', scaling=1)
orditorp(rda_tree.2monthlag, display='sp', cex=0.5, scaling=1, col='blue')
text(rda_tree.2monthlag, display='cn', col='red')

rda.test.2monthlag.oneVar <- rda(monthly.abundance_6_10 ~ 
                                   environmental.matrix.2monthlag.allvars$inflows.lowerquartile)
rda.test.2monthlag.oneVar
RsquareAdj(rda.test.2monthlag.oneVar)
plot(rda.test.2monthlag.oneVar, type='n', scaling=1)
orditorp(rda.test.2monthlag.oneVar, display='sp', cex=0.5, scaling=1, col='blue')
text(rda.test.2monthlag.oneVar, display='cn', col='red')
## Adj R2 of each RDA model with only one environmental gradient predictor - 2 month lag
#** Fall rate adj R2 = 0.02006339
# Median monthly flow = 0.003927602
# Max monthly flow = -0.001143629
# Date of max monthly flow = -0.003894399
#** Count over 10k = 0.01288574
#** Count over 15k = 0.009560282
# Count over 20k = -0.004242439
# Rise rate = 0.006148205
# Count over 30k = -0.004272739
# Monthly reversals = -0.002470501
# Monthly inflows = -0.002341899
# Inflows (upper qtl) = -0.003957174 --> replaced with monthly inflow qtls
# Inflows (lower qtl) =  0.004799375 -- replaced with monthly inflow qtls
# ** New inflow upper qtls (by month) = 0.01432136
# New inflow lower qtls (by month) = -0.003332291


##** Formatting environmental matrix to match dimensions of abundance matrix; trying 1 month lag
environmental.matrix.1monthlag.allvars <- environmental.matrix %>% mutate(dummy.month = month + 1)
environmental.matrix.1monthlag.allvars$date <- as.Date(paste(environmental.matrix.1monthlag.allvars$year,
                                                             environmental.matrix.1monthlag.allvars$dummy.month,
                                                             1, sep = "-"),
                                                       format = "%Y-%m-%d")
environmental.matrix.1monthlag.allvars.CCA <- environmental.matrix.1monthlag.allvars
environmental.matrix.1monthlag.allvars <- semi_join(environmental.matrix.1monthlag.allvars,
                                                    monthly.abundance_6_9, by = "date")
environmental.matrix.1monthlag.allvars.join <- environmental.matrix.1monthlag.allvars
environmental.matrix.1monthlag.allvars <- environmental.matrix.1monthlag.allvars %>% arrange(date)
monthly.abundance_6_9 <- monthly.abundance_6_9 %>% arrange(date)
monthly.abundance_6_9_CCA <- monthly.abundance_6_9
monthly.abundance_6_9_combined <- monthly.abundance_6_9
monthly.abundance_6_9 <- monthly.abundance_6_9[,-c(1:2,7)]
environmental.matrix.1monthlag.allvars <- environmental.matrix.1monthlag.allvars %>%
  select(-c("year","month", "dummy.month", "date"))

## Formatting classes of environmental matrix
str(environmental.matrix.1monthlag.allvars)
environmental.matrix.1monthlag.allvars$median.monthly.flow <- 
  as.numeric(environmental.matrix.1monthlag.allvars$median.monthly.flow)
environmental.matrix.1monthlag.allvars$max.monthly.flow <- 
  as.numeric(environmental.matrix.1monthlag.allvars$max.monthly.flow)
environmental.matrix.1monthlag.allvars$date.of.monthly.max.flow <- 
  as.numeric(environmental.matrix.1monthlag.allvars$date.of.monthly.max.flow)
environmental.matrix.1monthlag.allvars$days.over.10k <- 
  as.numeric(environmental.matrix.1monthlag.allvars$days.over.10k)
environmental.matrix.1monthlag.allvars$days.over.15k <- 
  as.numeric(environmental.matrix.1monthlag.allvars$days.over.15k)
environmental.matrix.1monthlag.allvars$days.over.20k <- 
  as.numeric(environmental.matrix.1monthlag.allvars$days.over.20k)
environmental.matrix.1monthlag.allvars$days.over.30k <- 
  as.numeric(environmental.matrix.1monthlag.allvars$days.over.30k)
environmental.matrix.1monthlag.allvars$monthly.rise.rate <- 
  as.numeric(environmental.matrix.1monthlag.allvars$monthly.rise.rate)
environmental.matrix.1monthlag.allvars$monthly.reversals <- 
  as.numeric(environmental.matrix.1monthlag.allvars$monthly.reversals)
environmental.matrix.1monthlag.allvars$monthly.inflows <- 
  as.numeric(environmental.matrix.1monthlag.allvars$monthly.inflows)
environmental.matrix.1monthlag.allvars$inflows.upperquartile <- 
  as.numeric(environmental.matrix.1monthlag.allvars$inflows.upperquartile)
environmental.matrix.1monthlag.allvars$inflows.lowerquartile <- 
  as.numeric(environmental.matrix.1monthlag.allvars$inflows.lowerquartile)

## RDA test - 1 month lag
rda_tree.1monthlag <- rda(monthly.abundance_6_9 ~ 
                            environmental.matrix.1monthlag.allvars$monthly.rise.rate +
                            environmental.matrix.1monthlag.allvars$monthly.fall.rate)
rda_tree.1monthlag
RsquareAdj(rda_tree.1monthlag)
plot(rda_tree.1monthlag, type='n', scaling=1)
orditorp(rda_tree.1monthlag, display='sp', cex=0.5, scaling=1, col='blue')
text(rda_tree.1monthlag, display='cn', col='red')

# Looking at median flow (lagged one month) only (adj. R squared < 0)
rda_tree.1monthlag.medianflow <- rda(monthly.abundance_6_9 ~ 
                                       environmental.matrix.1monthlag.allvars$median.monthly.flow)
rda_tree.1monthlag.medianflow
RsquareAdj(rda_tree.1monthlag.medianflow)

# Looking at max flow (lagged one month) only (adj. R squared approx. 0)
rda_tree.1monthlag.maxflow <- rda(monthly.abundance_6_9 ~ 
                                       environmental.matrix.1monthlag.allvars$max.monthly.flow)
rda_tree.1monthlag.maxflow
RsquareAdj(rda_tree.1monthlag.maxflow)

# Looking at date of max flow (also lagged one month, but should not make a difference?) only (adj. R2 < 0)
rda_tree.1monthlag.datemaxflow <- rda(monthly.abundance_6_9 ~ 
                                    environmental.matrix.1monthlag.allvars$date.of.monthly.max.flow)
rda_tree.1monthlag.datemaxflow
RsquareAdj(rda_tree.1monthlag.datemaxflow)

#*** Looking at fall rate (lagged one month) only (adj R2 = .07)!
rda_tree.1monthlag.fallrate <- rda(monthly.abundance_6_9 ~ 
                                        environmental.matrix.1monthlag.allvars$monthly.fall.rate)
rda_tree.1monthlag.fallrate
RsquareAdj(rda_tree.1monthlag.fallrate)

#*** Looking at rise rate (lagged one month) only (adj. R2 = 0.03629081)
rda_tree.1monthlag.riserate <- rda(monthly.abundance_6_9 ~ 
                                         environmental.matrix.1monthlag.allvars$monthly.rise.rate)
rda_tree.1monthlag.riserate
RsquareAdj(rda_tree.1monthlag.riserate)

# Looking at # days over 10k (lagged one month) only (adj. R2 < 0)
rda_tree.1monthlag.countover10k <- rda(monthly.abundance_6_9 ~ 
                                     environmental.matrix.1monthlag.allvars$days.over.10k)
rda_tree.1monthlag.countover10k
RsquareAdj(rda_tree.1monthlag.countover10k)

# Looking at # days over 15k (lagged one month) only (adj. R2 < 0)
rda_tree.1monthlag.countover15k <- rda(monthly.abundance_6_9 ~ 
                                         environmental.matrix.1monthlag.allvars$days.over.15k)
rda_tree.1monthlag.countover15k
RsquareAdj(rda_tree.1monthlag.countover15k)

# Looking at # days over 20k (lagged one month) only (adj. R2 < 0)
rda_tree.1monthlag.countover20k <- rda(monthly.abundance_6_9 ~ 
                                         environmental.matrix.1monthlag.allvars$days.over.20k)
rda_tree.1monthlag.countover20k
RsquareAdj(rda_tree.1monthlag.countover20k)

# Looking at # days over 30k (lagged one month) only (adj. R2 < 0)
rda_tree.1monthlag.countover30k <- rda(monthly.abundance_6_9 ~ 
                                         environmental.matrix.1monthlag.allvars$days.over.30k)
rda_tree.1monthlag.countover30k
RsquareAdj(rda_tree.1monthlag.countover30k)

# Looking at # of reversals (lagged one month) only (adj. R2 = 0)
rda_tree.1monthlag.reversals <- rda(monthly.abundance_6_9 ~ 
                                         environmental.matrix.1monthlag.allvars$monthly.reversals)
rda_tree.1monthlag.reversals
RsquareAdj(rda_tree.1monthlag.reversals)
# Looking at monthly inflow (lagged one month) only (adj. R2 < 0)
rda_tree.1monthlag.inflows <- rda(monthly.abundance_6_9 ~ 
                                      environmental.matrix.1monthlag.allvars$monthly.inflows)
rda_tree.1monthlag.inflows
RsquareAdj(rda_tree.1monthlag.inflows)

# Looking at upper quartile of monthly inflow (lagged one month) only (adj. R2 = 0)
rda_tree.1monthlag.upperqtl.inflows <- rda(monthly.abundance_6_9 ~ 
                                    environmental.matrix.1monthlag.allvars$inflows.upperquartile)
rda_tree.1monthlag.upperqtl.inflows
RsquareAdj(rda_tree.1monthlag.upperqtl.inflows)

# Looking at lower quartile of monthly inflow (lagged one month) only (adj. R2 = 0)
rda_tree.1monthlag.lowerqtl.inflows <- rda(monthly.abundance_6_9 ~ 
                                             environmental.matrix.1monthlag.allvars$inflows.lowerquartile)
rda_tree.1monthlag.lowerqtl.inflows
RsquareAdj(rda_tree.1monthlag.lowerqtl.inflows)

# Looking at new inflow upper quartile by month (lagged one month) only (adj. R2 < 0)
rda_tree.1monthlag.upperqtl.inflows2 <- rda(monthly.abundance_6_9 ~ 
                                             environmental.matrix.1monthlag.allvars$inflows.upperquartile)
rda_tree.1monthlag.upperqtl.inflows2
RsquareAdj(rda_tree.1monthlag.upperqtl.inflows2)

# Looking at new inflow lower quartile by month (lagged one month) only (adj. R2 < 0)
rda_tree.1monthlag.lowerqtl.inflows2 <- rda(monthly.abundance_6_9 ~ 
                                             environmental.matrix.1monthlag.allvars$inflows.lowerquartile)
rda_tree.1monthlag.lowerqtl.inflows2
RsquareAdj(rda_tree.1monthlag.lowerqtl.inflows2)


# Exploring which variables are significant
# onemonthlag.adonis <- adonis(monthly.abundance_6_9 ~ ., data=environmental.matrix.1monthlag.allvars)

## CCA model test
test <- monthly.abundance_6_9_CCA[,-c(1:2)]
test <- subset(test,rowSums(test[,c(1:4)])!=0)
environmental.matrix.1monthlag.allvars.CCA <- semi_join(environmental.matrix.1monthlag.allvars.CCA, test,
                                                        by = c("date"))
test <- test[,-c(5)]
cca_tree <- cca(test ~ environmental.matrix.1monthlag.allvars.CCA$median.monthly.flow +
                  environmental.matrix.1monthlag.allvars.CCA$max.monthly.flow + 
                  environmental.matrix.1monthlag.allvars.CCA$date.of.monthly.max.flow +
                  environmental.matrix.1monthlag.allvars.CCA$monthly.fall.rate +
                  environmental.matrix.1monthlag.allvars.CCA$days.over.10k +
                  environmental.matrix.1monthlag.allvars.CCA$monthly.rise.rate)
RsquareAdj(cca_tree, 100)
anova(cca_tree, permutations = 999)
anova(cca_tree, by = 'margin', permutations = 999)

### Organizing environmental matrices with different lag times into one matrix
environmental.matrix.1monthlag.allvars.join <-
  environmental.matrix.1monthlag.allvars.join %>% rename(median.flow.1monthlag = median.monthly.flow,
                                                         max.flow.1monthlag = max.monthly.flow,
                                                         date.of.max.flow.1monthlag = date.of.monthly.max.flow,
                                                         fall.rate.1monthlag = monthly.fall.rate,
                                                         minDO.oakcity.1monthlag = minDO.oakcity,
                                                         minDO.jamesville.1monthlag = minDO.jamesville,
                                                         days.over.10k.1monthlag = days.over.10k,
                                                         days.over.15k.1monthlag = days.over.15k,
                                                         days.over.20k.1monthlag = days.over.20k,
                                                         days.over.30k.1monthlag = days.over.30k,
                                                         rise.rate.1monthlag = monthly.rise.rate,
                                                         reversals.1monthlag = monthly.reversals,
                                                         inflows.1monthlag = monthly.inflows,
                                                         inflows.lowerqtl.1monthlag = inflows.lowerquartile,
                                                         inflows.upperqtl.1monthlag = inflows.upperquartile)
environmental.matrix.2monthlag.allvars.join <-
  environmental.matrix.2monthlag.allvars.join %>% rename(median.flow.2monthlag = median.monthly.flow,
                                                         max.flow.2monthlag = max.monthly.flow,
                                                         date.of.max.flow.2monthlag = date.of.monthly.max.flow,
                                                         fall.rate.2monthlag = monthly.fall.rate,
                                                         minDO.oakcity.2monthlag = minDO.oakcity,
                                                         minDO.jamesville.2monthlag = minDO.jamesville,
                                                         days.over.10k.2monthlag = days.over.10k,
                                                         days.over.15k.2monthlag = days.over.15k,
                                                         days.over.20k.2monthlag = days.over.20k,
                                                         days.over.30k.2monthlag = days.over.30k,
                                                         rise.rate.2monthlag = monthly.rise.rate,
                                                         reversals.2monthlag = monthly.reversals,
                                                         inflows.2monthlag = monthly.inflows,
                                                         inflows.lowerqtl.2monthlag = inflows.lowerquartile,
                                                         inflows.upperqtl.2monthlag = inflows.upperquartile)
environmental.matrix.3monthlag.allvars.join <-
  environmental.matrix.3monthlag.allvars.join %>% rename(median.flow.3monthlag = median.monthly.flow,
                                                         max.flow.3monthlag = max.monthly.flow,
                                                         date.of.max.flow.3monthlag = date.of.monthly.max.flow,
                                                         fall.rate.3monthlag = monthly.fall.rate,
                                                         minDO.oakcity.3monthlag = minDO.oakcity,
                                                         minDO.jamesville.3monthlag = minDO.jamesville,
                                                         days.over.10k.3monthlag = days.over.10k,
                                                         days.over.15k.3monthlag = days.over.15k,
                                                         days.over.20k.3monthlag = days.over.20k,
                                                         days.over.30k.3monthlag = days.over.30k,
                                                         rise.rate.3monthlag = monthly.rise.rate,
                                                         reversals.3monthlag = monthly.reversals,
                                                         inflows.3monthlag = monthly.inflows,
                                                         inflows.lowerqtl.3monthlag = inflows.lowerquartile,
                                                         inflows.upperqtl.3monthlag = inflows.upperquartile)

environmental.matrix.combined <- left_join(environmental.matrix.3monthlag.allvars.join,
                                           environmental.matrix.2monthlag.allvars.join,
                                           by = "date")
environmental.matrix.combined <- left_join(environmental.matrix.combined,
                                           environmental.matrix.1monthlag.allvars.join,
                                           by = "date")
environmental.matrix.combined <- left_join(environmental.matrix.combined,
                                           environmental.matrix,
                                           by = "date")
environmental.matrix.combined <- environmental.matrix.combined %>% select(-c("year.x", "month.x",
                                                                             "dummy.month.x", "year.y",
                                                                             "month.y","dummy.month.y",
                                                                             "year.y.y","month.y.y","dummy.month",
                                                                             "year.x.x","month.x.x"))
environmental.matrix.combined <- environmental.matrix.combined %>% select(c(10,1:9,11:61))
write.csv(environmental.matrix.combined, file = "./Data/ProcessedData/environmental.matrix.final.csv")

### Creating RDA model with combined environmental matrix (can select different lag time for different gradients)
# First matching rows in environmental matrix with the June-September abundance matrix (with 1 month lag,
# can only look at abundance through September, since only have environmental data up to August). After this
# model, next step is to include non-lagged flow variables.

env.matrix <- semi_join(environmental.matrix.combined, monthly.abundance_6_9_combined, by = "date")

## Focus on predictors identified as significant above: monthly fall rate (3 month lag), monthly rise rate (3
## month lag)*, fall rate (2 month), count over 10k (2 month), count over 15k (2 month)*, fall rate (1 month)*,
## rise rate (1 month)

## Secondary interest: Max monthly flow (3 month), count over 10k (3 month), rise rate (2 month), median
## monthly flow (2 month), max flow (1 month)
monthly.abundance_6_9_noHickory <- monthly.abundance_6_9 %>% select(-4)
rda_tree.combinedlag <- rda(monthly.abundance_6_9_noHickory ~ env.matrix$fall.rate.3monthlag +
                              env.matrix$rise.rate.3monthlag +
                              env.matrix$fall.rate.1monthlag +
                              env.matrix$inflows.2monthlag + env.matrix$inflows.upperqtl.3monthlag +
                              env.matrix$inflows.upperqtl.2monthlag + env.matrix$inflows.lowerqtl.1monthlag +
                              env.matrix$inflows.lowerqtl.3monthlag)
rda_tree.combinedlag
RsquareAdj(rda_tree.combinedlag)
plot(rda_tree.combinedlag, type='n', scaling=1)
orditorp(rda_tree.combinedlag, display='sp', cex=0.5, scaling=1, col='blue')
text(rda_tree.combinedlag, display='cn', col='red')
# Remove *median flow.2month (18.08; 18:54; 18.96; 19.35), *maxflow.3month (18.12; 18.63;19.04),
# *rise rate.1month (18.26), *days over 15k.2month (18.08; 18.55; 18.86; 19.20; 19.65),
# *days over 10k.2mth (18.22; 18.68), *fallrate.2month (18.11;18.57; 19.26; 19.55)
# removed riserate.1month, days.over.10k.2month, maxflow.3month, median.flow.2month, days.over.15k.2month,
# fall.rate.2month
## Add reversals?
# With upper qtls for entire series
# $r.squared
# [1] 0.2178875
# 
# $adj.r.squared
# [1] 0.1692817

# With upper qtls by month (decreased with upper qtl, 2 month lag alone)
# $r.squared
# [1] 0.2089647
# 
# $adj.r.squared
# [1] 0.1598043

## Testing without Hickory Shad (very few observations)


rda_tree_noHickory <- rda(monthly.abundance_6_9_noHickory ~ env.matrix$fall.rate.3monthlag +
                            env.matrix$rise.rate.3monthlag + env.matrix$fall.rate.2monthlag +
                            env.matrix$days.over.10k.2monthlag + env.matrix$days.over.15k.2monthlag +
                            env.matrix$fall.rate.1monthlag + env.matrix$rise.rate.1monthlag +
                            env.matrix$max.flow.3monthlag + env.matrix$median.flow.2monthlag +
                            env.matrix$inflows.2monthlag)
rda_tree_noHickory
RsquareAdj(rda_tree_noHickory)
plot(rda_tree_noHickory, type='n', scaling=1)
orditorp(rda_tree_noHickory, display='sp', cex=0.5, scaling=1, col='blue')
text(rda_tree_noHickory, display='cn', col='red')
## R2 and adj. R2 were nearly identical between the models with and without hickory shad. Exclude species -
## too few collections to detect influence

## Result of model selection by adjusted R2 comparison
rda_tree_R2modelselection <- rda(monthly.abundance_6_9 ~ 
      env.matrix$rise.rate.3monthlag +
      env.matrix$days.over.10k.2monthlag + env.matrix$days.over.15k.2monthlag +
      env.matrix$fall.rate.1monthlag + env.matrix$median.flow.2monthlag)

rda_tree_R2modelselection
RsquareAdj(rda_tree_R2modelselection)
plot(rda_tree_R2modelselection, type='n', scaling=1)
orditorp(rda_tree_R2modelselection, display='sp', cex=0.5, scaling=1, col='blue')
text(rda_tree_R2modelselection, display='cn', col='red')

test <- lm(monthly.abundance_6_9$monthly.american.shad.JAI ~ env.matrix$median.flow.2monthlag)
summary(test)
test.residuals <- resid(test)
plot(env.matrix$rise.rate.3monthlag, test.residuals)

## An increase in fall rate, lagged one month, is associated with an increase in juvenile blueback herring abundance
## Increase in # days over 15k cfs, with 2 month lag, associated with slight increase in abundance of juv.
## hickory shad.
## Increase in rise rate, lagged 3 months, is associated with an increase in juv blueback abundance

cor(env.matrix$rise.rate.3monthlag, env.matrix$fall.rate.3monthlag) # rise and fall rates strongly correlated

## Removing NA observations in order to include DO in model
# First removing one column with min DO in order to keep as many observations as possible (removing Oak City first)
env.matrix.DOmodel <- env.matrix %>% select(-minDO.oakcity.1monthlag,
                                            -minDO.oakcity.2monthlag, -minDO.oakcity.3monthlag)
env.matrix.DO_1monthlag <- env.matrix.DOmodel %>% select(-minDO.jamesville.3monthlag, -minDO.jamesville.2monthlag)
env.matrix.DO_1monthlag <- na.omit(env.matrix.DO_1monthlag)
monthly.abundance_DO1 <- semi_join(monthly.abundance_6_9_combined, env.matrix.DO_1monthlag, by = "date")
monthly.abundance_DO1 <- monthly.abundance_DO1[,-c(1:2,7)]
str(monthly.abundance_DO1)
env.matrix.DO_1monthlag$minDO.jamesville.1monthlag <- as.numeric(env.matrix.DO_1monthlag$minDO.jamesville.1monthlag)

## Modelling abundance on min DO for Jamesville w/ 1 month lag
rda_tree_DO1 <- rda(monthly.abundance_DO1 ~ env.matrix.DO_1monthlag$minDO.jamesville.1monthlag)
RsquareAdj(rda_tree_DO1)
plot(rda_tree_DO1, type='n', scaling=1)
orditorp(rda_tree_DO1, display='sp', cex=0.5, scaling=1, col='blue')
text(rda_tree_DO1, display='cn', col='red')

## Repeating for 2 month lag min DO at Jamesville
env.matrix.DO_2monthlag <- env.matrix.DOmodel %>% select(-minDO.jamesville.3monthlag, -minDO.jamesville.1monthlag)
env.matrix.DO_2monthlag <- na.omit(env.matrix.DO_2monthlag)
monthly.abundance_DO2 <- semi_join(monthly.abundance_6_9_combined, env.matrix.DO_2monthlag, by = "date")
monthly.abundance_DO2 <- monthly.abundance_DO2[,-c(1:2,7)]
str(monthly.abundance_DO1)
env.matrix.DO_2monthlag$minDO.jamesville.2monthlag <- as.numeric(env.matrix.DO_2monthlag$minDO.jamesville.2monthlag)

## Modelling abundance on min DO for Jamesville w/ 1 month lag
rda_tree_DO2 <- rda(monthly.abundance_DO2 ~ env.matrix.DO_2monthlag$minDO.jamesville.2monthlag)

RsquareAdj(rda_tree_DO2)
plot(rda_tree_DO2, type='n', scaling=1)
orditorp(rda_tree_DO2, display='sp', cex=0.5, scaling=1, col='blue', )
text(rda_tree_DO2, display='cn', col='red')

# ## Repeating for 3 month lag min DO at Jamesville
# env.matrix.DO_3monthlag <- env.matrix.DOmodel %>% select(-minDO.jamesville.2monthlag, -minDO.jamesville.1monthlag)
# env.matrix.DO_3monthlag <- na.omit(env.matrix.DO_3monthlag)
# monthly.abundance_DO3 <- semi_join(monthly.abundance_6_9_combined, env.matrix.DO_3monthlag, by = "date")
# monthly.abundance_DO3 <- monthly.abundance_DO3[,-c(1:2,7)]
# str(monthly.abundance_DO3)
# env.matrix.DO_3monthlag$minDO.jamesville.3monthlag <- as.numeric(env.matrix.DO_3monthlag$minDO.jamesville.3monthlag)

# ## Modelling abundance on min DO for Jamesville w/ 1 month lag
# rda_tree_DO3 <- rda(monthly.abundance_DO3 ~ env.matrix.DO_3monthlag$minDO.jamesville.3monthlag)
# RsquareAdj(rda_tree_DO3)
# plot(rda_tree_DO3, type='n', scaling=1)
# orditorp(rda_tree_DO3, display='sp', cex=0.5, scaling=1, col='blue', )
# text(rda_tree_DO3, display='cn', col='red')


### ANOVA test of abundance for upper quartile inflow years vs. lower quartile inflow years
full.env.abundance.matrix <- left_join(monthly.abundance, environmental.matrix.combined, by = "date")
full.env.abundance.matrix$inflows.upperquartile <- as.factor(full.env.abundance.matrix$inflows.upperquartile)
full.env.abundance.matrix$inflows.lowerquartile <- as.factor(full.env.abundance.matrix$inflows.lowerquartile)
full.env.abundance.matrix$inflows.lowerqtl.3monthlag <- as.factor(full.env.abundance.matrix$inflows.lowerqtl.3monthlag)
full.env.abundance.matrix$inflows.upperqtl.3monthlag <- as.factor(full.env.abundance.matrix$inflows.upperqtl.3monthlag)
rainy.months <- full.env.abundance.matrix %>% filter(inflows.upperquartile == 1)
dry.months <- full.env.abundance.matrix %>% filter(inflows.lowerquartile == 1)
extreme.inflows <- subset(full.env.abundance.matrix, inflows.upperquartile == 1 | inflows.lowerquartile == 1)
extreme.inflows.lag1month <- subset(full.env.abundance.matrix,
                                    inflows.upperqtl.1monthlag == 1 | inflows.lowerqtl.1monthlag == 1)
extreme.inflows.lag2month <- subset(full.env.abundance.matrix,
                                    inflows.upperqtl.2monthlag == 1 | inflows.lowerqtl.2monthlag == 1)
extreme.inflows.lag3month <- subset(full.env.abundance.matrix,
                                    inflows.upperqtl.3monthlag == 1 | inflows.lowerqtl.3monthlag == 1)

ggplot(subset(full.env.abundance.matrix, inflows.upperquartile == 1 | inflows.lowerquartile == 1),
       aes(x = inflows.upperquartile, y = monthly.blueback.herring.JAI)) +
  geom_boxplot()

## Using ANOVA to test whether abundances are significantly different between years with high/low rainfall
shapiro.test(extreme.inflows$monthly.blueback.herring.JAI[extreme.inflows$inflows.upperquartile == 1])
shapiro.test(extreme.inflows$monthly.blueback.herring.JAI[extreme.inflows$inflows.lowerquartile == 1])
# Neither well approximated by normal distribution (as expected). Using (non-parametric) wilcox test
twosample.wilcox.bbhJAI.extremeinflows <- wilcox.test(extreme.inflows$monthly.blueback.herring.JAI ~
                                                        extreme.inflows$inflows.upperquartile)
twosample.wilcox.bbhJAI.extremeinflows
## No significant difference between blueback herring abundance in wet vs. dry months (in same month)
## (two way Wilcox test; p-value = 0.31)

## Performing two way Wilcox again with high/low inflows lagged one month
twosample.wilcox.abundance.extremeinflows.1monthlag <- 
  wilcox.test(extreme.inflows.lag1month$monthly.blueback.herring.JAI ~
                extreme.inflows.lag1month$inflows.upperqtl.1monthlag)
twosample.wilcox.abundance.extremeinflows.1monthlag
## No significant difference between blueback herring abundance in wet vs. dry months (lagged one month)
## (two way Wilcox test; p-value = 0.219)
## No significant difference for A. shad (p = 0.867)
## No significant difference for alewife (p = 0.573)

## Performing two way Wilcox again with high/low inflows lagged two months
twosample.wilcox.abundance.extremeinflows.2monthlag <- 
  wilcox.test(extreme.inflows.lag2month$monthly.blueback.herring.JAI ~
                extreme.inflows.lag2month$inflows.upperqtl.2monthlag)
twosample.wilcox.abundance.extremeinflows.2monthlag
## No significant difference between blueback herring abundance in wet vs. dry months (lagged two months)
## (two way Wilcox test; p-value = 0.8052)
## No significant difference for A. shad (p = 0.63)
## No significant difference for alewife (p = 0.846)

## Performing two way Wilcox again with high/low inflows lagged three months
twosample.wilcox.abundance.extremeinflows.3monthlag <- 
  wilcox.test(extreme.inflows.lag3month$monthly.blueback.herring.JAI ~
                extreme.inflows.lag3month$inflows.upperqtl.3monthlag)
twosample.wilcox.abundance.extremeinflows.3monthlag
## Significant difference between blueback herring abundance in wet vs. dry months (lagged three months)
## (two way Wilcox test; p-value = 0.029)
## No significant difference for A. shad (p = 0.38)
## No significant difference for alewife (p = 0.992)

ggplot(data = extreme.inflows.lag3month, aes(y = monthly.blueback.herring.JAI, x = inflows.upperqtl.3monthlag)) +
  geom_boxplot()

## Exploring relationship between abundance and extreme inflow, now at a seasonal resolution

