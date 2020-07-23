getwd()
library(tidyverse)
library(vegan)
library(grid)

##### Script for CCA modelling and other analysis

environmental.matrix <- read.csv("./Data/ProcessedData/environmental.matrix.final.csv")
columns.as.numeric <- colnames(environmental.matrix[,c(3:62)])
environmental.matrix[columns.as.numeric] <- lapply(environmental.matrix[columns.as.numeric], as.numeric)
environmental.matrix <- environmental.matrix %>% select(-"X")
environmental.matrix$date <- as.Date(environmental.matrix$date, format = "%Y-%m-%d")

abundance.matrix.allmonths <- read.csv("./Data/ProcessedData/AlosineAbundance_monthly.csv")
abundance.matrix.allmonths <- abundance.matrix.allmonths %>% select(-"X")
abundance.matrix.allmonths$date <- as.Date(abundance.matrix.allmonths$date, format = "%Y-%m-%d")

## Formatting the abundance matrix for CCA model (rows sum to 1 to indicate species composition; 0 abundance rows removed)
abundance.matrix.CCA.format <- abundance.matrix.allmonths[,-c(1:2)]
abundance.matrix.CCA.format <- subset(abundance.matrix.CCA.format,rowSums(abundance.matrix.CCA.format[,c(1:4)])!=0)
divide.by.rowsum <- function(x, na.rm = FALSE) (x/rowSums(abundance.matrix.CCA.format[,c(1:4)]))
abundance.matrix.CCA.format <- abundance.matrix.CCA.format %>% select(c(1:5)) %>% mutate_at(c(1:4),
                                                                              .funs = divide.by.rowsum)
abundance.matrix.CCA.format <- abundance.matrix.CCA.format %>% mutate(month = 
                                                  format(as.Date(abundance.matrix.CCA.format$date), "%m"),
                                                  year = format(as.Date(abundance.matrix.CCA.format$date), "%Y"))
abundance.matrix.CCA.format$month <- as.numeric(abundance.matrix.CCA.format$month)
abundance.matrix.CCA.format$year <- as.numeric(abundance.matrix.CCA.format$year)
## Have environmental data for March - August (3-11); and have abundance data for all months. For a 3 month lag,
## only have sufficient environmental data to look at abundance in June - November. However, do not have (unlagged)
## environmental data for months 9-11 (will only look at months June-October with full env. data set) (without unlagged
## data, can look at June - November)

## Creating abundance matrices for June through...Sept, Oct, Nov
abundance.matrix.CCA.Jun_Oct <- abundance.matrix.CCA.format %>% filter(month == 06 | month == 07 | month == 08 |
                                                                         month == 09 | month == 10)
abundance.matrix.CCA.Jun_Sep <- abundance.matrix.CCA.format %>% filter(month == 06 | month == 07 | month == 08 |
                                                                         month == 09)
abundance.matrix.CCA.Jun_Nov <- abundance.matrix.CCA.format %>% filter(month == 06 | month == 07 | month == 08 |
                                                                            month == 09 | month == 10 | month == 11)

## Removing unmatched rows (with abundance matrix) from environmental matrix
environmental.matrix.CCA.Jun_Oct <- semi_join(environmental.matrix, abundance.matrix.CCA.Jun_Oct, by = "date")
environmental.matrix.CCA.Jun_Sep <- semi_join(environmental.matrix, abundance.matrix.CCA.Jun_Sep, by = "date")
environmental.matrix.CCA.Jun_Nov <- semi_join(environmental.matrix, abundance.matrix.CCA.Jun_Nov, by = "date")

## Removing data/year/month columns
abundance.matrix.CCA.Jun_Oct <- abundance.matrix.CCA.Jun_Oct %>% select(-c("date", "month", "year"))
abundance.matrix.CCA.Jun_Sep <- abundance.matrix.CCA.Jun_Sep %>% select(-c("date", "month", "year"))
abundance.matrix.CCA.Jun_Nov <- abundance.matrix.CCA.Jun_Nov %>% select(-c("date", "month", "year"))

environmental.matrix.CCA.Jun_Oct <- environmental.matrix.CCA.Jun_Oct %>% select(-c("date"))
environmental.matrix.CCA.Jun_Sep <- environmental.matrix.CCA.Jun_Sep %>% select(-c("date"))
environmental.matrix.CCA.Jun_Nov <- environmental.matrix.CCA.Jun_Nov %>% select(-c("date"))

## Setting data classes
str(environmental.matrix.CCA.Jun_Nov)
str(abundance.matrix.CCA.Jun_Nov)

## Removing DO columns for purpose of finding significant variables
environmental.matrix.CCA.Jun_Nov_clean <- environmental.matrix.CCA.Jun_Nov %>% select(-c("minDO.oakcity.3monthlag",
                                                                                         "minDO.jamesville.3monthlag",
                                                                                         "minDO.oakcity.2monthlag",
                                                                                         "minDO.jamesville.2monthlag",
                                                                                         "minDO.oakcity.1monthlag",
                                                                                         "minDO.jamesville.1monthlag",
                                                                                         "minDO.oakcity",
                                                                                         "minDO.jamesville"))

## Using adonis to find significant variables
adonis.varselect <- adonis(abundance.matrix.CCA.Jun_Nov ~ ., data=environmental.matrix.CCA.Jun_Nov_clean)
adonis.varselect

bestEnvVariables <- rownames(adonis.varselect$aov.tab)[adonis.varselect$aov.tab$"Pr(>F)"<=0.05]
bestEnvVariables<-bestEnvVariables[!is.na(bestEnvVariables)]
eval(parse(text=paste("CCA.model.selection <- cca(abundance.matrix.CCA.Jun_Nov ~ ",
                      do.call(paste,c(as.list(bestEnvVariables),sep=" + ")),
                      ",data=environmental.matrix.CCA.Jun_Nov_clean)",sep="")))
scrs<-scores(CCA.model.selection, display=c("sp","wa","lc","bp","cn"))
df_sites<-data.frame(scrs$sites,t(as.data.frame(strsplit(rownames(scrs$sites),"_"))))
colnames(df_sites)<-c("x","y","month")

p <- ggplot()
p<-p+geom_point(data=df_sites,aes(x,y))
multiplier <- vegan:::ordiArrowMul(scrs$biplot)
df_arrows<- scrs$biplot*multiplier
colnames(df_arrows)<-c("x","y")
df_arrows=as.data.frame(df_arrows)
p<-p+geom_segment(data=df_arrows, aes(x = 0, y = 0, xend = x, yend = y),
                  arrow = arrow(length = unit(0.2, "cm")),color="#808080",alpha=0.5)
p<-p+geom_text(data=as.data.frame(df_arrows*1.1),aes(x, y, label = rownames(df_arrows)),color="#808080",alpha=0.5)
df_species<- as.data.frame(scrs$species)
colnames(df_species)<-c("x","y")
# p<-p+geom_text(data=df_species,aes(x,y,label=rownames(df_species)))
p<-p+geom_point(data=df_species,aes(x,y,shape="Species"))+scale_shape_manual("",values=2)
p<-p+theme_bw()
p

CCA_model.allmonths <- cca(abundance.matrix.CCA.Jun_Nov ~ environmental.matrix.CCA.Jun_Nov$median.monthly.flow)
RsquareAdj(CCA_model.allmonths, 100)
anova(CCA_model.allmonths, permutations = 999)
anova(CCA_model.allmonths, by = 'margin', permutations = 999)

## Significant predictors (adj. R2):
#** Date of max flow - 3 month lag (0.1058)
# Fall rate - 3 month lag (0.02759021)
# Rise rate - 3 month lag (0.02260607)
# Reversals - 3 month lag (0.01151084)
# Days over 10k - 3 month lag - (0.002251548) - most significant "days over X" var w/ 3 month lag
# Max flow - 2 month lag (0.01129475)
# Date of max flow - 2 month lag (0.1058873)
# Fall rate - 2 month lag (0.02668163)
# Rise rate - 2 month lag (0.03601864)
# Reversals - 2 month lag (0.03759623)
# Inflows - 2 month lag (0.01649611)
# Median flow - 1 month lag (0.01361455)
# Max flow - 1 month lag (0.01046577)
# Date of max flow - 1 month lag (0.1034599)
# Fall rate - 1 month lag (0.0156292)
# Rise rate - 1 month lag (0.01651586)
# Reversals - 1 month lag (0.01391993)
# Inflows - 1 month lag (0.01263315)
#


# CCA_model.allmonths <- cca(abundance.matrix.CCA.Jun_Sep ~ environmental.matrix.CCA.June_Sep$fall.rate.3monthlag +
#                              environmental.matrix.CCA.June_Sep$inflows.1monthlag +
#                              environmental.matrix.CCA.Jun_Sep $rise.rate.3monthlag +
#                              environmental.matrix.CCA.Jun_Sep$inflows.2monthlag + 
#                              environmental.matrix.CCA.Jun_Sep$inflows.upperqtl.3monthlag +
#                              environmental.matrix.CCA.Jun_Sep$inflows.upperqtl.2monthlag +
#                              environmental.matrix.CCA.Jun_Sep$inflows.lowerqtl.3monthlag)

