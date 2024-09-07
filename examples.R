library(tidycensus)
library(tidyverse)
library(zipcodeR)

setwd("~/Documents/GitHub/SVICalculation/")
source("functionsForSVI.R")

#I am using two functions that are in functionsForSVI.R source

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
year=2018

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
