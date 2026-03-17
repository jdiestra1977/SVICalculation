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

socioEcoVars_cbgs<-getVariables("cbg",state,2022)

socioEcoVars_cbgs_only_Dallas<- socioEcoVars_cbgs %>% 
  filter(substr(GEOID, 1, 5) == "48113")

# Once this data frame is generated, it would be used in the main function
# to calculate SVI rankingSVI()
# 2- rankingSVI(), takes a data frame as argument with the variables needed to
# calculate SVI

#The data frame created by this function has the SVI of each zcta, calculated
#with respect all zcta's in the state
#sviTexas<-rankingAndSvi(socioEcoVars)

sviTexas_cbgs<-rankingAndSvi(socioEcoVars_cbgs)

dallas_svi_texas <- sviTexas_cbgs %>%
  filter(substr(Zip, 1, 5) == "48113") %>% drop_na()
dallas_svi_local<-rankingAndSvi(socioEcoVars_cbgs_only_Dallas)

dallas_svi_texas %>%
  ggplot(aes(x=SVI)) + geom_density()

dallas_svi_local %>%
  ggplot(aes(x=SVI)) + geom_density()

dallas_county_svi_cbgs<-dallas_svi_texas %>% rename("SVI_Texas"="SVI") %>%
  left_join(dallas_svi_local %>% rename("SVI_local"="SVI")) %>% rename("cbg"="Zip")

write_csv(dallas_county_svi_cbgs,file="dallas_county_svi_cbgs.csv")

dallas_county_svi_cbgs %>%
  ggplot(aes(x=SVI_Texas,y=SVI_local)) +
  geom_point() + geom_abline(slope = 1,intercept = 0)


#library(tidycensus)
library(dplyr)
library(sf)

# Example: 2022 ACS 5-year, block groups, Dallas County — includes geometry
bg_geo <- get_acs(
  geography = "block group",
  variables = "B01003_001",  # total population (any variable works)
  state = "TX",
  county = "Dallas",
  year = 2022,
  survey = "acs5",
  geometry = TRUE,
  output = "wide"
) %>%
  st_transform(4326) %>%
  select(GEOID, geometry) %>%
  rename(cbg = GEOID)

cbg_joined <- bg_geo %>% left_join(dallas_county_svi_cbgs, by = "cbg")

write_csv(cbg_joined,file="dallas_county_svi_cbgs.csv")

cbg_joined %>%
  ggplot(aes(fill=SVI_local)) + geom_sf() + theme_void()

cbg_joined %>% pivot_longer(cols = contains("SVI")) %>%
  ggplot(aes(fill=value)) + theme_void() +
  geom_sf() + facet_wrap(~name) + 
  scale_fill_viridis_c(
    option = "magma",     # or "plasma", "inferno", "viridis"
    direction = -1,       # -1 reverses (so dark = high vulnerability)
    limits = c(0, 1),
    na.value = "grey90",
    name = "SVI"
  )

cbg_joined %>% pivot_longer(cols = contains("SVI")) %>%
  ggplot(aes(fill=value)) + theme_void() +
  geom_sf() + facet_wrap(~name) + 
  scale_fill_gradient(
    low = "#a1d99b",  # pale pink
    high = "#a50f15", # dark red
    limits = c(0, 1),
    na.value = "grey90",
    name = "SVI"
  ) + theme(legend.position = "top",text=element_text(size=15))+
  guides(
    fill = guide_colorbar(
      barwidth = 20,   # <-- length of the bar (increase this)
      barheight = 1,   # <-- thickness of the bar
      title.position = "top",
      title.hjust = 0.5
    ))

ggsave(last_plot(),file="Dallas_svi_both.png")

#save(sviTexas,file="~/Documents/GitHub/Mpox_2024/Data/sviTexas.RData")

#3- getVariablesAllUS(). This function calculates SVI over all US at the 
#For all US, at the zcta level, we can extract data in the period [2015,2022]
geo="county"
year=2022
allUS<-getVariablesAllUS(geo,year)
#SVI of all counties in the USA
sviAllUS<-rankingAndSvi(allUS)

#SVI calculated by the CDC 
svi_cdc <- read_csv("SVI_2020_US_county_CDC.csv")

bothSVIs<-svi_cdc %>% select(FIPS,RPL_THEMES) %>%
  left_join(sviAllUS %>% select(FIPS=Zip,SVI))  %>% drop_na()

#Correlation between SVI calculated here and CDC.
bothSVIs %>%
  ggplot(aes(y=RPL_THEMES,x=SVI)) + theme_bw() +
  geom_point(size=2) + geom_abline(slope=1,intercept = 0,color="red") +
  theme(text=element_text(size=17))

#Some statistics to evaluate similarity.
cor(bothSVIs$RPL_THEMES,bothSVIs$SVI)
summary(lm(SVI~RPL_THEMES,data=bothSVIs))

# SVI SCTA in the US

geo="zcta"
year=2020
allUS<-getVariablesAllUS(geo,year)
#SVI of all counties in the USA
sviAllUS_ZCTA<-rankingAndSvi(allUS)
save(sviAllUS_ZCTA,file="~/Documents/GitHub/Mpox_2024/Data/sviAll_US.RData")

#State level SVI (Shraddha)
geo="state"
year=2025
allUS<-getVariablesAllUS(geo,year)
#SVI of all counties in the USA
sviAllUS<-rankingAndSvi(allUS)

svi_state_time<-NULL
for (y in 2015:2024) {
  geo="state"
  #year=2025
  allUS<-getVariablesAllUS(geo,y)
  #SVI of all counties in the USA
  sviAllUS<-rankingAndSvi(allUS) %>%
    mutate(year=y)
  svi_state_time<-rbind(svi_state_time,sviAllUS)
}

svi_state_time<-svi_state_time %>%
  left_join(allUS %>% select(Zip=GEOID,State=NAME)) %>%
  rename("GEOID"="Zip")

svi_state_time %>% mutate(year=as.integer(year)) %>%
  ggplot(aes(x=as.factor(year),y=SVI,group = GEOID,color=GEOID)) +
  geom_line()

svi_state_time %>% 
  ggplot(aes(x=State,y=SVI)) +
  geom_boxplot() + xlab("") +
  theme(axis.text.x = element_text(angle = 45,hjust=1))

write_csv(svi_state_time,file="svi_state_time.csv")

svi_state_time %>% 
  filter(year %in% c(2015,2024)) %>%
  select(-GEOID) %>% 
  pivot_wider(names_from = "year", values_from = "SVI") %>%
  mutate(diff_total = `2024` - `2015`) %>%
  filter(!State %in% c("Alaska","Puerto Rico","Hawaii")) %>%
  ggplot(aes(x = reorder(State, diff_total), y = diff_total)) + theme_bw() +
  geom_col() + xlab("") + ylab("Difference in SVI (2024 - 2015)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(last_plot(),file="change_in_SVI_states.png")


