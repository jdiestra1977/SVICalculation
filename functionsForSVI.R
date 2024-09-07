#This code is an adaptation of the original code that can be found here:
# https://github.com/azh2/Social-Vulnerability-R

#These are the variables that will be extracted from the American Community Survey (ACS) to
#calculate the SVI (ADPTVCAPACITY)
#Note: not all variables are being used for the index, but may be useful to know

getVariables<-function(geo,state,year){
  varsNew<-c("B01003_001E","C17002_002E","C17002_003E","C17002_001E","B23025_005E","B23025_003E","B19301_001E",
             "B15003_016E","B15003_017E","B15003_018E","B15003_019E","B15003_020E","B15003_021E","B15003_022E",
             "B15003_023E","B15003_024E","B15003_025E","B15003_001E","B99163_005E","B99163_001E","B09021_022E",
             "B09021_001E","B01001_003E","B01001_004E","B01001_005E","B01001_006E","B01001_027E","B01001_028E",
             "B01001_029E","B01001_030E","B18101_026E","B18101_007E","C18130_010E","C18130_017E","B18101_025E",
             "B18101_006E","C18130_009E","C18130_016E","B23008_008E","B23008_021E","B23008_002E","B23008_015E",
             "B25024_007E","B25024_008E","B25024_009E","B25024_001E","B25033_006E","B25033_007E","B25033_012E",
             "B25033_013E","B25033_001E","B25014_005E","B25014_006E","B25014_007E","B25014_011E","B25014_012E",
             "B25014_013E","B25014_001E","B25044_003E","B25044_010E","B25044_001E","B26001_001E","B03002_003E",
             "B03002_001E","B02001_004E","B02001_001E","B02001_005E","B02001_003E","B03003_003E","B03003_001E",
             "B02001_006E","B02001_007E","B02001_008E","B03002_003E","B03002_001E")
  #Gets socioeconomic variables from ACS (varsNew) for all zip codes in the USA
  dataVars <- get_acs(
    geography = geo,
    variables = varsNew,
    year = year,
    state=state,
    output = 'wide'
  )
  return(dataVars)
}

#Function that calculates SVI. It needs a data frame with all socioeconomic
#variables from above.
rankingAndSvi<-function(x){
  tablasJuntas2019<-x
  ######SOCIOECONOMIC STATUS: #####
  tablasJuntas2019 <- tablasJuntas2019 %>% mutate(TOTPOP=B01003_001E) #TOTAL_POPULATION - 
  #Percent of Population Below Poverty Level
  # C17002_001 - Estimate!!Total - Ratio of Income to Poverty in the Last 12 Months 
  # C17002_002 - Estimate!!Total!!Under .50 
  # C17002_003 - Estimate!!Total!!.50 to .99
  tablasJuntas2019 <- tablasJuntas2019 %>% mutate(POV =(C17002_002E+C17002_003E)/C17002_001E) #PER_POVERTY
  #Percent of Population Unemployed - Civilian (Age 16+) Unemployed Estimate
  # B23025_003 - Estimate!!Total!!In labor force!!Civilian labor force 
  # B23025_005 - Estimate!!Total!!In labor force!!Civilian labor force!!Unemployed
  tablasJuntas2019 <- tablasJuntas2019 %>% mutate(UNEMP =B23025_005E/B23025_003E) #PER_UNEMPLOYED 
  #B19301_001 - Estimate!!Per capita income in the past 12 months (in 2018 inflation-adjusted dollars)
  tablasJuntas2019<- tablasJuntas2019 %>% mutate(PCI=B19301_001E) #PER_CAPITA_INCOME
  
  ######LANGUAGE AND EDUCATION:#####
  #Percent of Population 25+ with Less than a 12th Grade Education
  # B15003_001 - Estimate!!Total 
  # B15003_016 - Estimate!!Total!!12th grade, no diploma # B15003_017 - Estimate!!Total!!Regular high school diploma 
  # B15003_018 - Estimate!!Total!!GED or alternative credential 
  # B15003_019 - Estimate!!Total!!Some college, less than 1 year 
  # B15003_020 - Estimate!!Total!!Some college, 1 or more years, no degree #B15003_021 - Estimate!!Total!!Associate's degree 
  # B15003_022 - Estimate!!Total!!Bachelor's degree 
  # B15003_023 - Estimate!!Total!!Master's degree 
  # B15003_024 - Estimate!!Total!!Professional school degree 
  # B15003_025 - Estimate!!Total!!Doctorate degree
  tablasJuntas2019 <-tablasJuntas2019 %>% mutate(NOHSDP= 1-((B15003_016E+B15003_017E+B15003_018E+B15003_019E+
                                                               B15003_020E+B15003_021E+B15003_022E+B15003_023E+
                                                               B15003_024E+B15003_025E)/B15003_001E)) #PER_LESS_HS_GRAD
  tablasJuntas2019 <- tablasJuntas2019 %>% mutate(LIMENG=B99163_005E/B99163_001E) #PER_POOR_ENGLISH
  
  #######DEMOGRAPHICS:######
  #Percent Of Population 65+ - Persons Aged 65 And Older Estimate
  # B09021_022 - Estimate!!Total!!65 years and over 
  # B09021_001 - Estimate!!Total
  tablasJuntas2019 <- tablasJuntas2019 %>% mutate(AGE65=B09021_022E/B09021_001E) #PER_OVER_65
  #Percent of Population 17 Years of Age and Under
  # B01001_003 - Estimate!!Total!!Male!!Under 5 years 
  # B01001_004 - Estimate!!Total!!Male!!5 to 9 years 
  # B01001_005 - Estimate!!Total!!Male!!10 to 14 years 
  # B01001_006 - Estimate!!Total!!Male!!15 to 17 years 
  # B01001_027 - Estimate!!Total!!Female!!Under 5 years 
  # B01001_028 - Estimate!!Total!!Female!!5 to 9 years 
  # B01001_029 - Estimate!!Total!!Female!!10 to 14 years 
  # B01001_030 - Estimate!!Total!!Female!!15 to 17 years
  tablasJuntas2019 <- tablasJuntas2019 %>% mutate(AGE17=(B01001_003E+B01001_004E+B01001_005E+B01001_006E+B01001_027E+B01001_028E+B01001_029E+B01001_030E)/B01003_001E) #PER_UNDER_17 
  #Percent of Population 5< with a Disability - Civilian Non-institutionalized Population With A Disability Estimate)
  # B18101_025 - Estimate!!Total!!Female!!5 to 17 years 
  # B18101_026 - Estimate!!Total!!Female!!5 to 17 years!!With a disability 
  # B18101_006 - Estimate!!Total!!Male!!5 to 17 years 
  # B18101_007 - Estimate!!Total!!Male!!5 to 17 years!!With a disability 
  # C18130_009 - Estimate!!Total!!18 to 64 years 
  # C18130_010 - Estimate!!Total!!18 to 64 years!!With a disability 
  # C18130_016 - Estimate!!Total!!65 years and over 
  # C18130_017 - Estimate!!Total!!65 years and over!!With a disability
  tablasJuntas2019 <- tablasJuntas2019 %>% mutate(DISABL=(B18101_026E+B18101_007E+C18130_010E+C18130_017E)/(B18101_025E+B18101_006E+C18130_009E+C18130_016E)) #PER_DISABLED
  #Percent of Children Living in Single Parent Households
  # B09008_001 - Estimate!!Total 
  # B09008_010 - Estimate!!Total!!No unmarried partner of householder present!!In family households!!In male householder, no wife present, family 
  # B09008_011 - Estimate!!Total!!No unmarried partner of householder present!!In family households!!In female householder, no husband present, family 
  # B09008_012 - Estimate!!Total!!No unmarried partner of householder present!!In nonfamily households
  #tablasJuntas2019 <- tablasJuntas2019 %>% mutate(SNGPNT=(B09008_010E+B09008_011E+B09008_012E)/B09008_001E) #PER_SINGL_PRNT #PER_SINGL_PRNT Option 2 (See Notes 496-521)
  ## OR (for 2019)
  #B23008_008 - Estimate!!Total!!Under 6 years!!Living with one parent
  #B23008_021 - Estimate!!Total!!6 to 17 years!!Living with one parent
  #B23008_002 - Estimate!!Total!!Under 6 years
  #B23008_015 - Estimate!!Total!!6 to 17 years
  tablasJuntas2019 <- tablasJuntas2019 %>% mutate(SNGPNT=(B23008_008E+B23008_021E)/(B23008_002E+B23008_015E)) 
  
  #####HOUSING AND TRANSPORTATION:####
  #Percent of Housing Units with 10+ Units in Structure - Housing in Structures With 10 or More Units Estimate
  # B25024_001 - Estimate!!Total 
  # B25024_007 - Estimate!!Total!!10 to 19 
  # B25024_008 - Estimate!!Total!!20 to 49 
  # B25024_009 - Estimate!!Total!!50 or more
  tablasJuntas2019<- tablasJuntas2019 %>% mutate(MUNIT=(B25024_007E+B25024_008E+B25024_009E)/B25024_001E) #PER_MULTI_DWELL
  #Percent of Population Living in Mobile Homes - Mobile Homes Estimate
  # B25033_001 - Estimate!!Total 
  # B25033_006 - Estimate!!Total!!Owner occupied!!Mobile home 
  # B25033_007 - Estimate!!Total!!Owner occupied!!Boat, RV, van, etc. 
  # B25033_012 - Estimate!!Total!!Renter occupied!!Mobile home 
  # B25033_013 - Estimate!!Total!!Renter occupied!!Boat, RV, van, etc.
  tablasJuntas2019 <- tablasJuntas2019 %>% mutate(MOBILE=(B25033_006E+B25033_007E+B25033_012E+B25033_013E)/B25033_001E) #PER_MOBILE_DWEL
  #Percent of Population Living In Accommodations with Less Than 1 Room Per Person/Crowding - At Household Level, Occupied Housing Units, More People Than Rooms Estimate
  # B25014_001 - Estimate!!Total 
  # B25014_005 - Estimate!!Total!!Owner occupied!!1.01 to 1.50 occupants per room 
  # B25014_006 - Estimate!!Total!!Owner occupied!!1.51 to 2.00 occupants per room 
  # B25014_007 - Estimate!!Total!!Owner occupied!!2.01 or more occupants per room 
  # B25014_011 - Estimate!!Total!!Renter occupied!!1.01 to 1.50 occupants per room 
  # B25014_012 - Estimate!!Total!!Renter occupied!!1.51 to 2.00 occupants per room 
  # B25014_013 - Estimate!!Total!!Renter occupied!!2.01 or more occupants per room
  tablasJuntas2019 <- tablasJuntas2019 %>% mutate(CROWD=(B25014_005E+B25014_006E+B25014_007E+B25014_011E+B25014_012E+B25014_013E)/B25014_001E) #PER_CROWD_DWELL
  #Percent of Population with No Vehicle Available - Households With No Vehicle Available Estimate
  # B25044_001 - Estimate!!Total 
  # B25044_003 - Estimate!!Total!!Owner occupied!!No vehicle available 
  # B25044_010 - Estimate!!Total!!Renter occupied!!No vehicle available
  tablasJuntas2019 <- tablasJuntas2019 %>% mutate(NOVEH=(B25044_003E+B25044_010E)/B25044_001E) #PER_NO_VEH_AVAIL
  #Percent of Population Living in Group Quarters
  # B26001_001 - Estimate!!Total!!Group quarters population
  tablasJuntas2019 <- tablasJuntas2019 %>% mutate(GROUPQ=B26001_001E/B01003_001E) #PER_GROUP_DWELL
  
  #####RACIAL AND ETHNIC MAKEUP:####
  #Percent Minority
  # B03002_003 - Estimate!!Total (WHITE ALONE, NOT HISPANIC OR LATINO)
  tablasJuntas2019 <- tablasJuntas2019 %>% mutate(MINORITY= 1-(B03002_003E/B03002_001E))
  # B02001_004 - Estimate!!Total!!American Indian and Alaska Native alone
  tablasJuntas2019 <- tablasJuntas2019 %>% mutate(NTVAMRCN=B02001_004E/B02001_001E)
  # B02001_005 -  Estimate!!Total!!Asian alone
  tablasJuntas2019 <- tablasJuntas2019 %>% mutate(ASIAN=B02001_005E/B02001_001E)
  # B02001_003 - Estimate!!Total!!Black or African American alone
  tablasJuntas2019 <- tablasJuntas2019 %>% mutate(BLACK=B02001_003E/B02001_001E)
  # B03003_003 - Estimate!!Total!!Hispanic or Latino HISPANIC OR LATINO ORIGIN
  tablasJuntas2019 <- tablasJuntas2019 %>% mutate(HISPLATX=B03003_003E/B03003_001E)
  # B02001_006 - Estimate!!Total!!Native Hawaiian and Other Pacific Islander alone
  tablasJuntas2019 <- tablasJuntas2019 %>% mutate(PACISL=B02001_006E/B02001_001E)
  # B02001_007 - Estimate!!Total!!Some other race alone
  tablasJuntas2019 <- tablasJuntas2019 %>% mutate(OTHRRACE=B02001_007E/B02001_001E)
  #B02001_008 - Estimate!!Total!!Two or more races
  tablasJuntas2019 <- tablasJuntas2019 %>% mutate(MULTRACE=B02001_008E/B02001_001E)
  # B03002_003 - Estimate!!Total!!Not Hispanic or Latino!!White alone
  tablasJuntas2019 <- tablasJuntas2019 %>% mutate(WHITE=B03002_003E/B03002_001E)
  
  #####OPTIONAL VARIABLES:####
  
  #JndTbls$HOMESOCCPD <- 1-JndTbls$B25002_003E/JndTbls$B25002_001E
  #JndTbls$RENTER <- JndTbls$B25003_003E/JndTbls$B25003_001E
  #JndTbls$RENTBURDEN <- (JndTbls$B25070_007E+JndTbls$B25070_008E+JndTbls$B25070_009E+JndTbls$B25070_010E)/JndTbls$B25070_001E
  #JndTbls$RENTASPERINCOME <- (JndTbls$B25071_001E/100)
  #JndTbls$OVR65ALONE <- JndTbls$B11007_003E/JndTbls$B11007_001E
  #JndTbls$BLTBFR1969 <- (JndTbls$B25034_008E+JndTbls$B25034_009E+JndTbls$B25034_010E+JndTbls$B25034_011E)/JndTbls$B25034_001E
  #JndTbls$SVRPOV <- JndTbls$C17002_002E/JndTbls$C17002_001E
  #JndTbls$MODPOV <- JndTbls$C17002_004E/JndTbls$C17002_001E
  #JndTbls$SINGLMTHRPVRTY <-(JndTbls$B17023_016E+JndTbls$B17023_017E+JndTbls$B17023_018E)/JndTbls$B17023_001E
  
  #####RANKING#####
  
  #These functions rank each of the variables, variables with matching values across ranks are given the max score, 
  #this is the default in excel where the original formulae were derived
  
  a <- tablasJuntas2019$RNKPOV <- rank(x = -tablasJuntas2019$POV, na.last = "keep", ties.method = "max")
  b <- tablasJuntas2019$RNKUNEMP <- rank(x = -tablasJuntas2019$UNEMP, na.last = "keep", ties.method = "max")
  c <- tablasJuntas2019$RNKPCI <- rank(x = tablasJuntas2019$PCI, na.last = "keep", ties.method = "max") #Note that we are not taking the inverse here because the higher the Per Capita Income, the greater the Adaptive Capacity of a given blockgroup
  d <- tablasJuntas2019$RNKNOHSDP <- rank(x = -tablasJuntas2019$NOHSDP, na.last = "keep", ties.method = "max")
  e <- tablasJuntas2019$RNKLIMENG <- rank(x = -tablasJuntas2019$LIMENG, na.last = "keep", ties.method = "max")
  f <- tablasJuntas2019$RNKAGE65 <- rank(x = -tablasJuntas2019$AGE65, na.last = "keep", ties.method = "max")
  g <- tablasJuntas2019$RNKAGE17 <- rank(x = -tablasJuntas2019$AGE17, na.last = "keep", ties.method = "max")
  h <- tablasJuntas2019$RNKDISABL <- rank(x = -tablasJuntas2019$DISABL, na.last = "keep", ties.method = "max")
  i <- tablasJuntas2019$RNKSNGPNT <- rank(x = -tablasJuntas2019$SNGPNT, na.last = "keep", ties.method = "max")
  j <- tablasJuntas2019$RNKMUNIT <- rank(x = -tablasJuntas2019$MUNIT, na.last = "keep", ties.method = "max")
  k <- tablasJuntas2019$RNKMOBILE <- rank(x = -tablasJuntas2019$MOBILE, na.last = "keep", ties.method = "max")
  l <- tablasJuntas2019$RNKCROWD <- rank(x = -tablasJuntas2019$CROWD, na.last = "keep", ties.method = "max")
  m <- tablasJuntas2019$RNKNOVEH <- rank(x = -tablasJuntas2019$NOVEH, na.last = "keep", ties.method = "max")
  n <- tablasJuntas2019$RNKGROUPQ <- rank(x = -tablasJuntas2019$GROUPQ, na.last = "keep", ties.method = "max")
  
  #Sum The Ranks
  tablasJuntas2019$SUMRANK <- a+b+c+d+e+f+g+h+i+j+k+l+m+n
  #Derive the Adaptive Capacity Index
  tablasJuntas2019$ADPTVCAPACITY <- dplyr::percent_rank(tablasJuntas2019$SUMRANK)
  tablasJuntas2019$SVI=1-tablasJuntas2019$ADPTVCAPACITY
  
  tablasJuntas2019<-tablasJuntas2019 %>% drop_na()
#  tablasJuntas2019 %>% select(GEOID,SVI,ADPTVCAPACITY)
  sviAllUSA2019<-tablasJuntas2019 %>% select(Zip=GEOID,SVI)
  return(sviAllUSA2019)
}

