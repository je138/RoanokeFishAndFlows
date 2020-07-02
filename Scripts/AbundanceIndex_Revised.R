getwd()
library(tidyverse)

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
                                                    station == "39S" | station == "130S" | station == "127S")
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

## Filtering for only alosine species (a. shad, blueback herring, alewife)
alosines.fp <- river.herring.stations.fp %>% filter(Species_name != "")

## Investigating number of sample groups with FL over 100mm
over100FL.alosines <- alosines.fp %>% filter(FL_mm > 100)
