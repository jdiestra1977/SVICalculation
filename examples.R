library(tidycensus)
library(tidyverse)
library(zipcodeR)

setwd("~/Documents/GitHub/SVICalculation/")
source("functionsForSVI.R")

#I am using three functions that are in functionsForSVI.R source

# 1- getVariables(geo,state,year)
#This function has arguments that define the geography and time for the extraction
#of variables to be used to calculate SVI
#geo - this can be either "zcta" or "cbg"
#state - Short version of states name. Example "TX", "NM", etc
#year - is coupled with the geo option
#       if geo = "cbg", then year can take values in the range 2015 - 2022
#       if geo = "zcta", then year can take values in the range 2015 - 2019

geo="zcta"
state="TX"
year=2019
#I am extracting the socioeconomic variables at the zcta level in the whole Texas state,
#for the year 2018
socioEcoVars<-getVariables(geo,state,year)

# Once this data frame is generated, it would be used in the main function
# to calculate SVI rankingSVI()
# 2- rankingSVI(), takes a data frame as argument with the variables needed to
# calculate SVI

#The data frame created by this function has the SVI of each zcta, calculated
#with respect all zcta's in the state
sviTexas<-rankingAndSvi(socioEcoVars)

#3- getVariablesAllUS(). This function calculates SVI over all US at the 
#For all US, at the zcta level, we can extract data in the period [2015,2022]
geo="county"
year=2020
allUS<-getVariablesAllUS(geo,year)
#SVI of all counties in the USA
sviAllUS<-rankingAndSvi(allUS)

#SVI calculated by the CDC 
svi_cdc <- read_csv("SVI_2020_US_county_CDC.csv")

bothSVIs<-svi_cdc %>% select(FIPS,RPL_THEMES) %>%
  left_join(sviAllUS %>% select(FIPS=Zip,SVI))  %>% drop_na()

#Correlation between SVI calculated here and CDC.
bothSVIs %>%
  ggplot(aes(x=RPL_THEMES,y=SVI)) +
  geom_point() + geom_abline(slope=1,intercept = 0)

#Some statistics to evaluate similarity.
cor(bothSVIs$RPL_THEMES,bothSVIs$SVI)
summary(lm(SVI~RPL_THEMES,data=bothSVIs))



