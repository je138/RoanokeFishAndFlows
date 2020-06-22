getwd()
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
  select(control1, control3, month, colnum) %>%
  summarise(cpue = mean(colnum))

## Filtering for only juvenile alewife (will make separate index for combined group)
juv.alewife <- abundance.allgroups %>% filter(Species_name == "Alewife" &
                                           spstatus == "1")

abundance.juv.alewife <- juv.alewife %>% select(1:6) %>% group_by(date, location) %>%
  summarise(juv.alewife.index = mean(cpue))

# ggplot(data = juv.alewife.abundance, aes(y= juv.alewife.index, x = date)) +
#   geom_line()

## Repeating process for combined (juvenile and adult) group
combined.alewife <- abundance.allgroups %>% filter(Species_name == "Alewife" &
                                                     spstatus == "0")
abundance.combined.alewife <- combined.alewife %>% select(1:6) %>% group_by(date, location) %>%
  summarise(combined.alewife.index = mean(cpue))

## Repeating process for juvenile blueback herring
juv.blueback <- abundance.allgroups %>% filter(Species_name == "Blueback Herring" &
                                                 spstatus == "1")
abundance.juv.blueback <- juv.blueback %>% select(1:6) %>% group_by(date, location) %>%
  summarise(juv.blueback.index = mean(cpue))

## Repeating process for combined blueback herring
combined.blueback <- abundance.allgroups %>% filter(Species_name == "Blueback Herring" &
                                                      spstatus == "0")
abundance.combined.blueback <- combined.blueback %>% select(1:6) %>% group_by(date, location) %>%
  summarise(combined.blueback.index = mean(cpue))

## Repeating process for juvenile American shad
juv.A.shad <- abundance.allgroups %>% filter(Species_name == "American Shad" &
                                                 spstatus == "1")
abundance.juv.A.shad <- juv.A.shad %>% select(1:6) %>% group_by(date, location) %>%
  summarise(juv.A.shad.index = mean(cpue))

## Repeating process for combined American shad
combined.A.shad <- abundance.allgroups %>% filter(Species_name == "American Shad" &
                                                    spstatus == "0")
abundance.combined.A.shad <- combined.A.shad %>% select(1:6) %>% group_by(date, location) %>%
  summarise(combined.A.shad.index = mean(cpue))

## Creating data frame with one row for each haul (for each control1 code) to join abundance indices
abundance.df <- first.pulls %>% select(control1, date, year, month, station, location, surtemp,
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

### *** For data exploration script
## Graphing bottom temperature (degrees Celsius) over time
ggplot(data = abundance.df, aes(x= date, y = bottemp)) +
  geom_line()

## Graphing bottom temp data missingness over time
ggplot(data = subset(abundance.df, is.na(bottemp)), aes(x = date)) +
  geom_histogram()
