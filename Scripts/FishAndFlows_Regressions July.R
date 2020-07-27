library(tidyverse)
library(MASS)
library(dplyr)
library(mgcv)
library(GauPro)

## Importing annual american shad CPUE data from NCWRC
adult.shad.WRC.raw <- read_csv("./Data/RawData/AdultAmericanShadCPUE.csv")

## Importing monthly alosine abundance indices
monthly.alosine.abundance <- read_csv("./Data/ProcessedData/AlosineAbundance_monthly.csv")

## Importing environmental matrix (no lags) in order to summarize environmental parameters for good vs bad years
environmental.matrix <- read.csv("./Data/ProcessedData/environmental.matrix.clean.csv")
str(environmental.matrix)
environmental.matrix$year <- as.factor(environmental.matrix$year)
environmental.matrix$month <- as.factor(environmental.matrix$month)
columns.as.numeric <- colnames(environmental.matrix[,c(4:19)])
environmental.matrix[columns.as.numeric] <- lapply(environmental.matrix[columns.as.numeric], as.numeric)
environmental.matrix <- environmental.matrix %>% dplyr::select(-"X")

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

roanoke.rapids.discharge <- roanoke.rapids.discharge %>% mutate(month = 
                                                    format(as.Date(roanoke.rapids.discharge$date), "%m"),
                                                    year = format(as.Date(roanoke.rapids.discharge$date), "%Y"))

## Creating lists of good years and bad years for (primarily) blueback herring abundance
good.years <- c(2004, 2005, 2013, 2018, 2019)
bad.years <- c(2002, 2007, 2008, 2012)

env.matrix.goodyears <- environmental.matrix %>% filter(year %in% good.years)
env.matrix.badyears <- environmental.matrix %>% filter(year %in% bad.years)

flow.goodyears <- roanoke.rapids.discharge %>% filter(year %in% good.years)
flow.badyears <- roanoke.rapids.discharge %>% filter(year %in% bad.years)

flow.goodyears$month <- as.numeric(flow.goodyears$month)
flow.goodyears <- flow.goodyears %>% filter(month %in% c(03,04,05,06))
flow.badyears$month <- as.numeric(flow.badyears$month)
flow.badyears <- flow.badyears %>% filter(month %in% c(03,04,05,06))

## Finding average flow by month (and season) in good years and bad years
avg.flow.goodyears <- flow.goodyears %>% group_by(month) %>% summarise(avg.flow = mean(avg.daily.flow))
avg.flow.badyears <- flow.badyears %>% group_by(month) %>% summarise(avg.flow = mean(avg.daily.flow))

write.csv(avg.flow.goodyears, file = "./Data/Summary Tables/avg.flow.goodyears.csv")
write.csv(avg.flow.badyears, file = "./Data/Summary Tables/avg.flow.badyears.csv")

avg.flow.seasonal.goodyears <- flow.goodyears %>% filter(month != 06) %>%
  summarise(avg.flow = mean(avg.daily.flow))

avg.flow.seasonal.badyears <- flow.badyears %>% filter(month != 06) %>%
  summarise(avg.flow = mean(avg.daily.flow))

write.csv(avg.flow.seasonal.goodyears, file = "./Data/Summary Tables/avg.seasonal.flow.goodyears.csv")
write.csv(avg.flow.seasonal.badyears, file = "./Data/Summary Tables/avg.seasonal.flow.badyears.csv")

## Finding average environmental parameters by month and by season in good years and bad years
avg.parameters.goodyears$month <- as.numeric(avg.parameters.goodyears$month)
avg.parameters.goodyears <- env.matrix.goodyears %>% filter(month %in% c(3,4,5,6)) %>%
  group_by(month) %>% summarise(avg.max.flow = mean(max.monthly.flow),
                                avg.date.max.flow = mean(date.of.monthly.max.flow),
                                avg.monthly.fall.rate = mean(monthly.fall.rate),
                                avg.monthly.rise.rate = mean(monthly.rise.rate),
                                avg.reversals = mean(monthly.reversals),
                                avg.inflows = mean(monthly.inflows),
                                avg.days.over.10k = mean(days.over.10k),
                                avg.days.over.15k = mean(days.over.15k),
                                avg.days.over.20k = mean(days.over.20k),
                                avg.days.over.30k = mean(days.over.30k))
avg.parameters.seasonal.goodyears <- env.matrix.goodyears %>% filter(month == 3 | month == 4 | month == 5) %>%
  summarise(avg.max.flow = mean(max.monthly.flow),
         avg.date.max.flow = mean(date.of.monthly.max.flow),
         avg.monthly.fall.rate = mean(monthly.fall.rate),
         avg.monthly.rise.rate = mean(monthly.rise.rate),
         avg.reversals = mean(monthly.reversals),
         avg.inflows = mean(monthly.inflows),
         avg.days.over.10k = mean(days.over.10k),
         avg.days.over.15k = mean(days.over.15k),
         avg.days.over.20k = mean(days.over.20k),
         avg.days.over.30k = mean(days.over.30k))

write.csv(avg.parameters.goodyears, file = "./Data/Summary Tables/avg.parameters.goodyears.csv")
write.csv(avg.parameters.seasonal.goodyears, file = "./Data/Summary Tables/avg.seasonal.parameters.goodyears.csv")

avg.parameters.badyears <- env.matrix.badyears %>% filter(month %in% c(3,4,5,6)) %>%
  group_by(month) %>% summarise(avg.max.flow = mean(max.monthly.flow),
                                avg.date.max.flow = mean(date.of.monthly.max.flow),
                                avg.monthly.fall.rate = mean(monthly.fall.rate),
                                avg.monthly.rise.rate = mean(monthly.rise.rate),
                                avg.reversals = mean(monthly.reversals),
                                avg.inflows = mean(monthly.inflows),
                                avg.days.over.10k = mean(days.over.10k),
                                avg.days.over.15k = mean(days.over.15k),
                                avg.days.over.20k = mean(days.over.20k),
                                avg.days.over.30k = mean(days.over.30k))
avg.parameters.seasonal.badyears <- env.matrix.badyears %>% filter(month == 3 | month == 4 | month == 5) %>%
  summarise(avg.max.flow = mean(max.monthly.flow),
            avg.date.max.flow = mean(date.of.monthly.max.flow),
            avg.monthly.fall.rate = mean(monthly.fall.rate),
            avg.monthly.rise.rate = mean(monthly.rise.rate),
            avg.reversals = mean(monthly.reversals),
            avg.inflows = mean(monthly.inflows),
            avg.days.over.10k = mean(days.over.10k),
            avg.days.over.15k = mean(days.over.15k),
            avg.days.over.20k = mean(days.over.20k),
            avg.days.over.30k = mean(days.over.30k))

write.csv(avg.parameters.badyears, file = "./Data/Summary Tables/avg.parameters.badyears.csv")
write.csv(avg.parameters.seasonal.badyears, file = "./Data/Summary Tables/avg.seasonal.parameters.badyears.csv")

env.matrix.goodyears.spring <- env.matrix.goodyears %>% filter(month %in% c(3,4,6)) %>%
  dplyr::select(year, month, max.monthly.flow, date.of.monthly.max.flow) %>%
  group_by(year) %>%
  summarize(seasonal.max.flow = max(max.monthly.flow))
env.matrix.goodyears.spring <- env.matrix.goodyears.spring %>% mutate(max.monthly.flow = seasonal.max.flow)
env.matrix.goodyears.spring <- left_join(env.matrix.goodyears.spring, env.matrix.goodyears,
                                         by = c("year", "max.monthly.flow"))
mean(env.matrix.goodyears.spring$date.of.monthly.max.flow)

env.matrix.badyears.spring <- env.matrix.badyears %>% filter(month %in% c(3,4,6)) %>%
  dplyr::select(year, month, max.monthly.flow, date.of.monthly.max.flow) %>%
  group_by(year) %>%
  summarize(seasonal.max.flow = max(max.monthly.flow))
env.matrix.badyears.spring <- env.matrix.badyears.spring %>% mutate(max.monthly.flow = seasonal.max.flow)
env.matrix.badyears.spring <- left_join(env.matrix.badyears.spring, env.matrix.badyears,
                                         by = c("year", "max.monthly.flow"))
mean(env.matrix.badyears.spring$date.of.monthly.max.flow)

## Selecting years of interest, then summarizing yearly abundance averages for american shad
annual.american.shad.abundance <- monthly.alosine.abundance %>% filter(year %in% c(2000:2019)) %>% group_by(year) %>% 
  summarise(annual.juv.abundance = mean(monthly.american.shad.JAI))
juvenile.shad.abundance.june <- monthly.alosine.abundance %>% filter(year %in% c(2000:2019) & month == 6) %>%
  dplyr::select("year", "monthly.american.shad.JAI")
colnames(juvenile.shad.abundance.june)[colnames(juvenile.shad.abundance.june) == "monthly.american.shad.JAI"] <-
  "june.abundance"

## Joining panels for regression analysis
american.shad.panel <- left_join(adult.shad.WRC.raw, annual.american.shad.abundance, by = "year")
american.shad.panel <- left_join(american.shad.panel, juvenile.shad.abundance.june, by = "year")
shapiro.test(american.shad.panel$total.CPUE) ## p > 0.05, fail to reject null hypothesis of normal distribution
shapiro.test(american.shad.panel$annual.juv.abundance) ## p < 0.05, reject null hypothesis of normal distribution

## Testing both parametric and non-parametric simple linear regressions
a.shad.regression <- lm(american.shad.panel$annual.juv.abundance ~ american.shad.panel$total.CPUE)
summary(a.shad.regression)

a.shad.regression2 <- lm(american.shad.panel$june.abundance ~ american.shad.panel$total.CPUE)
summary(a.shad.regression2)

a.shad.regression3 <- lm(american.shad.panel$annual.juv.abundance ~ american.shad.panel$male.CPUE)
summary(a.shad.regression3)

a.shad.regression4 <- lm(american.shad.panel$annual.juv.abundance ~ american.shad.panel$female.CPUE)
summary(a.shad.regression4)

# Non-parametric Spearman's Rho model
cor.test(american.shad.panel$annual.juv.abundance, american.shad.panel$total.CPUE, 
         method = "spearman", exact = FALSE)

## Evidence does not suggest significant relationship between adult and juvenile american shad CPUE;
## however, small sample size (simple linear regression; p = 0.38, n = 20)

ggplot(american.shad.panel, aes(x = total.CPUE, y = annual.juv.abundance)) +
  geom_smooth(method = "lm") +
  geom_point() 


## Running regressions on June blueback abundance
july.bbh.abundance <- monthly.alosine.abundance %>% filter(month == 7) %>%
  dplyr::select(c("date", "year", "month", "monthly.blueback.herring.JAI"))
july.bbh.abundance <- july.bbh.abundance %>% filter(year %in% c(1986:2020))

lagged.env.matrix <- read.csv("./Data/ProcessedData/environmental.matrix.final.csv")
lagged.env.matrix$date <- as.Date(lagged.env.matrix$date, format = "%Y-%m-%d")

regression.env.matrix <- semi_join(lagged.env.matrix, july.bbh.abundance, by = "date")
regression.env.matrix <- regression.env.matrix %>% dplyr::select(-c("X"))
colnames(regression.env.matrix) = gsub("3monthlag", "march", colnames(regression.env.matrix))
colnames(regression.env.matrix) = gsub("2monthlag", "april", colnames(regression.env.matrix))
colnames(regression.env.matrix) = gsub("1monthlag", "may", colnames(regression.env.matrix))
regression.env.matrix <- regression.env.matrix %>% dplyr::select(-c("minDO.oakcity.march", "minDO.jamesville.march",
                                                             "minDO.oakcity.april", "minDO.jamesville.april",
                                                             "minDO.oakcity.may", "minDO.jamesville.may",
                                                             "minDO.oakcity", "minDO.jamesville"))
bbh.regression.panel <- left_join(july.bbh.abundance, regression.env.matrix, by = "date")
bbh.regression.panel <- bbh.regression.panel %>% dplyr::select(-c("date", "month"))

bbh.model <- lm(data = bbh.regression.panel, formula = monthly.blueback.herring.JAI ~
                  poly(bbh.regression.panel$days.over.30k.march, 1, raw = TRUE))
summary(bbh.model)
cor(bbh.regression.panel$inflows.may, bbh.regression.panel$rise.rate.may)

plot(bbh.regression.panel$rise.rate.may, resid(bbh.model))

ggplot(data = bbh.regression.panel, aes(days.over.20k.march, monthly.blueback.herring.JAI)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 1, raw = TRUE))

## Some variables that stood out:
# median flow march (.0041, .0009) ***
# max flow march (0.002266, .053) .
# date of max flow march (-1.29, 0.185)
# Inflows march (.002, 0.156)
# Days over 10k march (2.124, .0333) *
# Days over 15k march (2.31, .0226) *
# Days over 20k march (3.24, .00579) **
# Days over 30k march (7.429, .000015) ***

## Adding all variables to linear model then performing stepwise selection of variables
bbh.model.stepwise <- lm(data = bbh.regression.panel, formula = monthly.blueback.herring.JAI ~
                           median.flow.march + max.flow.march + date.of.max.flow.march + inflows.march +
                           days.over.10k.march + days.over.15k.march + days.over.30k.march)
summary(bbh.model.stepwise)
stepwise.bbh.model <- stepAIC(bbh.model.stepwise)
summary(stepwise.bbh.model)
range(bbh.regression.panel$rise.rate.april)

model.test <- lm(data = bbh.regression.panel, formula = monthly.blueback.herring.JAI ~
                   fall.rate.may)
summary(model.test)
cor(bbh.regression.panel$rise.rate.may, bbh.regression.panel$fall.rate.may)

## Variables selected by stepwise function: April rise rate (coeff = .026, p = .034),
## May rise rate (coeff = .012, p = .0007)

ggplot(data = bbh.regression.panel, aes(x = monthly.blueback.herring.JAI)) +
  geom_histogram()

monthly.alosine.abundance$month <- as.factor(monthly.alosine.abundance$month)
ggplot(data = monthly.alosine.abundance, aes(x = month, y = monthly.blueback.herring.JAI)) +
  geom_boxplot()

## Testing other, non-linear forms
# Gaussian
gp <- GauPro(bbh.regression.panel$max.flow.march, bbh.regression.panel$monthly.blueback.herring.JAI,
             parallel=FALSE)
plot(bbh.regression.panel$max.flow.march, bbh.regression.panel$monthly.blueback.herring.JAI)
curve(gp$predict(x), add=T, col=2)

gam.bbh.model.test <- gam(data = bbh.regression.panel, s(median.flow.april))


## Removing outlier
bbh.regression.panel.outlier.removed <- bbh.regression.panel %>% filter(year != 1993)

bbh.model.stepwise.outlier.removed <- lm(data = bbh.regression.panel.outlier.removed, formula = monthly.blueback.herring.JAI ~
                           median.flow.march + max.flow.march + date.of.max.flow.march + inflows.march +
                           days.over.10k.march + days.over.15k.march + days.over.30k.march)
stepwise.bbh.model.outlier.removed <- stepAIC(bbh.model.stepwise.outlier.removed)
summary(stepwise.bbh.model.outlier.removed)

