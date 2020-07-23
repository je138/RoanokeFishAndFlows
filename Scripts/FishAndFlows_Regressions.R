library(tidyverse)

## Importing annual american shad CPUE data from NCWRC
adult.shad.WRC.raw <- read_csv("./Data/RawData/AdultAmericanShadCPUE.csv")

## Importing monthly alosine abundance indices
monthly.alosine.abundance <- read_csv("./Data/ProcessedData/AlosineAbundance_monthly.csv")

## Selecting years of interest, then summarizing yearly abundance averages for american shad
annual.american.shad.abundance <- monthly.alosine.abundance %>% filter(year %in% c(2000:2019)) %>% group_by(year) %>% 
  summarise(annual.juv.abundance = mean(monthly.american.shad.JAI))
juvenile.shad.abundance.june <- monthly.alosine.abundance %>% filter(year %in% c(2000:2019) & month == 6) %>%
  select("year", "monthly.american.shad.JAI")
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





