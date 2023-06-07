#create a regression model to communicate the claim that higher temperatures cause higher mortality

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Env 175 Project 5")

##############
#LOAD PACKAGES
##############

library(ggplot2)
library(dplyr)
library(readr)
library(janitor)
library(lubridate)
library(maps)
library(mapdata)
library(ggmap)
library(sf)
library(spatialEco)
library(taRifx)

####################
#CLEAN CDC MORT DATA
####################

#read in tab delimited file
cdc_california_1999_2018 <- read_delim("~/Library/Mobile Documents/com~apple~CloudDocs/Env 175 Project 5/cdc/cdc_california_1999_2018.txt",
                                       "\t", escape_double = FALSE, col_types = cols(`Month Code` = col_date(format = "%Y/%m")), trim_ws = TRUE)

#lowercase variable names
cdc_california_1999_2018 <- clean_names(cdc_california_1999_2018)

#extract year and month from date variable
cdc_california_1999_2018 <- mutate(cdc_california_1999_2018, year=year(month_code), month=month(month_code))

#remove unnecessary columns
cdc_california_1999_2018 <- select(cdc_california_1999_2018, county_code, deaths, year, month)

###################
#CLEAN CDC POP DATA
###################

#read in tab delimited file
pop_california_1990_2018 <- read_delim("~/Library/Mobile Documents/com~apple~CloudDocs/Env 175 Project 5/pop/pop_california_1990_2018.txt",
                                       "\t", escape_double = FALSE, trim_ws = TRUE)

#lowercase variable names
pop_california_1990_2018 <- clean_names(pop_california_1990_2018)

#rename variables
pop_california_1990_2018 <- rename(pop_california_1990_2018, year=yearly_july_1st_estimates)

#remove unnecessary columns
pop_california_1990_2018 <- select(pop_california_1990_2018, county_code, year, population)

#remove rows from before 1999
pop_california_1999_2018 <- filter(pop_california_1990_2018, year>=1999)

######################
#CREATE JOINED DATASET
######################

#join population and mortality data
cdc_pop_1999_2018 <- full_join(pop_california_1999_2018,
                               cdc_california_1999_2018,
                               by = c("county_code", "year"))

#destring variables
cdc_pop_1999_2018 <- mutate(cdc_pop_1999_2018, county_code=destring(county_code), population=destring(population), deaths=destring(deaths))

#create countyfip column
cdc_pop_1999_2018 <- mutate(cdc_pop_1999_2018, countyfip=county_code-6000)

#create deathrate column
cdc_pop_1999_2018 <- mutate(cdc_pop_1999_2018, deathrate=deaths/population*100000)

#summarize
summary(cdc_pop_1999_2018$deathrate)

###################
#CLEAN WEATHER DATA
###################

#read in weather data
ghcn_california_1999_2019 <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Env 175 Project 5/ghcn/ghcn_california_1999_2019.csv",
                                      col_types = cols(DATE = col_date(format = "%Y-%m-%d")))

#clean variable names
ghcn_california_1999_2019 <- clean_names(ghcn_california_1999_2019)

#remove unnecessary columns
ghcn_california_1999_2019 <- select(ghcn_california_1999_2019, -name, -latitude, -longitude, -elevation)

#create year and month column
ghcn_california_1999_2019 <- mutate(ghcn_california_1999_2019, year=year(date), month=month(date))

#remove 2019 since we don't have mortality data for this year
ghcn_california_1999_2018 <- filter(ghcn_california_1999_2019, year<2019)

#summarize
summary(ghcn_california_1999_2018$tmax)

################################
#IMPORT STATION-COUNTY CROSSWALK
################################

crosswalk_100km <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Env 175 Project 5/station_county_crosswalk_100km.csv")

########################################
#JOIN WEATHER DATA WITH COUNTY CROSSWALK
########################################

ghcn_stn_100km <- full_join(crosswalk_100km, ghcn_california_1999_2018, by = "station")

#########################
#INVERSE DISTANCE WEIGHTS
#########################

ghcn_stn_100km <- mutate(ghcn_stn_100km, invdist=1/distance)

############################################
#SUMMARIZE WEATHER COUNTY-YEAR-MONTH AVERAGE
############################################

#group
ghcn_stn_100km <- group_by(ghcn_stn_100km, countyfip, year, month)

#summarize
ghcn_county_100km <- summarise(ghcn_stn_100km,
                               tmax=weighted.mean(tmax, invdist, na.rm = TRUE))

#ungroup
ghcn_stn_100km <- ungroup(ghcn_stn_100km)

#save
write_csv(ghcn_county_100km, "~/Library/Mobile Documents/com~apple~CloudDocs/Env 175 Project 5/outputs/ghcn_county_100km_california_1999_2018.csv")

########################################
#JOIN WITH CDC DATA AT COUNTY-YEAR-MONTH
########################################

cdc_ghcn_100km <- full_join(ghcn_county_100km,
                            cdc_pop_1999_2018,
                            by = c("countyfip", "year", "month"))

#save
write_csv(cdc_ghcn_100km, "~/Library/Mobile Documents/com~apple~CloudDocs/Env 175 Project 5/outputs/cdc_ghcn_100km_california_1999_2018.csv")

#######
#GRAPHS
#######

#scatterplot - all counties
ggplot(cdc_ghcn_100km,
       aes(y=deathrate,
           x=tmax,
           size=population,
           weight=population))  +
  geom_point(color="plum1") +
  geom_smooth(method = lm,
              se = FALSE,
              color="darkorchid4",
              size=1) +
  labs(y = "Death Rate",
       x = "Max Temperature in CA Year-Round") +
  theme(panel.background = element_rect(fill="white"),
        legend.position = "none",
        axis.line = element_line(colour = "darkorchid4") )

#scatterplot - all counties, August
ggplot(filter(cdc_ghcn_100km,
              month==8),
       aes(y=deathrate,
           x=tmax,
           size=population,
           weight=population)) +
  geom_point(color="plum1") +
  geom_smooth(method = lm,
              se = FALSE,
              color="darkorchid4",
              size=1) +
  labs(y = "Death Rate",
       x = "Max Temperature in CA in August") +
  theme(panel.background = element_rect(fill="white"),
        legend.position = "none",
        axis.line = element_line(colour = "darkorchid4") )

#scatterplot - LA, August
ggplot(filter(cdc_ghcn_100km,
              month==8 & countyfip==37),
       aes(y=deathrate,
           x=tmax,
           size=population,
           weight=population)) +
  geom_point(color="plum1") +
  geom_smooth(method = lm,
              se = FALSE,
              color="darkorchid4",
              size=1) +
  labs(y = "Death Rate",
       x = "Max Temperature in Los Angeles in August") +
  theme(panel.background = element_rect(fill="white"),
        legend.position = "none",
        axis.line = element_line(colour = "darkorchid4") )

############
#REGRESSIONS
############

#save a log file of regression estimates
sink("~/Library/Mobile Documents/com~apple~CloudDocs/Env 175 Project 5/outputs/log_tutorial14.txt")

#all counties
regression1 <- lm(deathrate ~ tmax,
                  data=cdc_ghcn_100km,
                  weight=population)
summary(regression1)

#all counties, August only
regression2 <- lm(deathrate ~ tmax,
                  data=filter(cdc_ghcn_100km, month==8),
                  weight=population)
summary(regression2)

#LA, August only
regression3 <- lm(deathrate ~ tmax,
                  data=filter(cdc_ghcn_100km, month==8 & countyfip==37),
                  weight=population)
summary(regression3)

sink()

###############################################
#REMOVE AVG (FIXED) DIFFERENCES ACROSS COUNTIES
###############################################

#visualize data in three sample counties: LA, SB, Fresno
ggplot(filter(cdc_ghcn_100km,
              (countyfip==37 | countyfip==83 | countyfip==19) & month==8),
       aes(y=deathrate, x=tmax, color=factor(countyfip))) +
  geom_point() +
  geom_smooth(method = lm,
              se = FALSE,
              color="black",
              size=1) +
  labs(y = "Death Rate",
       x = "Max Temperature") +
  scale_colour_manual(values=c("dodgerblue1", "goldenrod1","limegreen")) +
  theme(panel.background = element_rect(fill="white"),
        legend.position = "none",
        axis.line = element_line(colour = "black"))

#determine the average for a given county month
cdc_ghcn_100km <- group_by(cdc_ghcn_100km, countyfip, month)
cdc_ghcn_100km <- mutate(cdc_ghcn_100km,
                         tmax_avg=weighted.mean(tmax, population, na.rm = TRUE),
                         deathrate_avg=weighted.mean(deathrate, population, na.rm = TRUE))

#calculate the differences from the mean for each variable
cdc_ghcn_100km <- mutate(cdc_ghcn_100km,
                         tmax_dev=tmax-tmax_avg,
                         deathrate_dev=deathrate-deathrate_avg)
cdc_ghcn_100km <- ungroup(cdc_ghcn_100km)

#visualize deviations: LA, SB, Fresno
ggplot(filter(cdc_ghcn_100km, (countyfip==37 | countyfip==83 | countyfip==19) & month==8),
       aes(y=deathrate_dev, x=tmax_dev, color=factor(countyfip))) +
  geom_point() +
  labs(y = "Death Rate",
       x = "Max Temperature") +
  scale_colour_manual(values=c("dodgerblue1", "goldenrod1","limegreen")) +
  theme(panel.background = element_rect(fill="white"),
        legend.position = "none",
        axis.line = element_line(colour = "black"))

####################
#DEMEANED REGRESSION
####################

#save a log file of regression estimates
sink("~/Library/Mobile Documents/com~apple~CloudDocs/Env 175 Project 5/outputs/log_tutorial15.txt")

#demeaned, August only
regression4 <- lm(deathrate_dev ~ tmax_dev,
                  data=filter(cdc_ghcn_100km, month==8),
                  weight=population)
summary(regression4)

############################################
#FIXED EFFECTS (DEMEAN ALL COUNTIES QUICKLY)
############################################

#fixed effect for county, August only
regression5 <- lm(deathrate ~ tmax + factor(countyfip),
                  data=filter(cdc_ghcn_100km, month==8),
                  weight=population)
summary(regression5)

sink()

######################
#CREATE DUMMY VARIABLE
######################

#create hotmonth dummy variable
dummy_august <- mutate(filter(cdc_ghcn_100km, month==8), hotmonth = ifelse(tmax>88, 1, 0))

#################################
#REGRESSIONS USING DUMMY VARIABLE
#################################

#save a log file of regression estimates
sink("~/Library/Mobile Documents/com~apple~CloudDocs/Env 175 Project 5/outputs/log_tutorial16.txt")

#all counties, August only, no fixed effect
regression6 <- lm(deathrate ~ hotmonth,
                  data = dummy_august,
                  weight=population)
summary(regression6)

#fixed effect for county, August only
regression7 <- lm(deathrate ~ hotmonth + factor(countyfip),
                  data = dummy_august,
                  weight=population)
summary(regression7)

sink()

