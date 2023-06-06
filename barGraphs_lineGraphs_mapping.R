setwd("~/Desktop/spring 2020/env 175/Project 4")


##############
#LOAD PACKAGES
##############


library(maps)
library(mapdata)
library(ggmap)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(janitor)
library(sf)
library(spatialEco)
library(taRifx)


############################
#IMPORT WEATHER STATION DATA
############################


stations_california <- read_csv("~/Desktop/spring 2020/env 175/Project 4/ghcn/ghcn_stations_california.csv")


#############
#PREPARE MAPS
#############


#get county boundaries
map_us_counties <- map_data("county")
map_us_counties <- rename(map_us_counties, state=region, county_name=subregion)
map_california_counties <- filter(map_us_counties, state=="california")


#read in county names
fips_names_california <- read_csv("~/Desktop/spring 2020/env 175/Project 3/fips_names_california.csv")


#join county fips and names
map_california_counties <- full_join(map_california_counties,
                                     fips_names_california,
                                     by = "county_name")


#####################
#MAP WEATHER STATIONS
#####################


#map of california counties
ggplot(map_california_counties, aes(y=lat, x=long, group=group)) +
  geom_polygon(fill="lightcyan", color="gray75") +
  geom_point(data=stations_california,
             aes(y=latitude, x=longitude, group=station), color="blue2", size=0.5) +
  coord_fixed(1.3) +
  theme(panel.background = element_rect(fill="white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


ggsave("~/Desktop/spring 2020/env 175/Project 4/Tutorial Outputs/map_stations_california.pdf")


#########################
#STATIONS WITHIN COUNTIES
#########################


#read in shapefile
counties_sf <- st_read("~/Desktop/spring 2020/env 175/Project 4/tl_2019_us_county/tl_2019_us_county.shp")


#restrict to California
counties_sf <- filter(counties_sf, STATEFP=="06")


#convert station data into shapefile (sf)
stations_sf = st_as_sf(stations_california, coords = c("longitude", "latitude"))


#set coordinates of stations_sf to match counties_sf
st_crs(stations_sf) <- 4269


#counties containing stations
county_station_within <- point.in.poly(stations_sf, counties_sf)


#convert the matrix to a dataframe
county_station_within <- as.data.frame(county_station_within)


##############
#CLEAN UP DATA
##############


#lowercase names
county_station_within <- clean_names(county_station_within)


#rename fip variable
county_station_within <- rename(county_station_within, countyfip=countyfp)


#keep only needed variables
county_station_within <- select(county_station_within, station, countyfip)


#change fip variable from string to numeric
county_station_within <- mutate(county_station_within, countyfip=destring(countyfip))
#TUTORIAL 12


##########################
#IMPORT CDC MORTALITY DATA
##########################


#read in data
cdc_california_1999_2018 <- read_delim("~/Desktop/spring 2020/env 175/Project 4/cdc/cdc_california_1999_2018.txt",
                                       "\t", escape_double = FALSE, col_types = cols('Month Code' = col_date(format = "%Y/%m")), trim_ws = TRUE)


###########
#CLEAN DATA
###########


#clean the column names
cdc_california_1999_2018 <- clean_names(cdc_california_1999_2018)


#rename columns
cdc_california_1999_2018 <- mutate(cdc_california_1999_2018, year=year(month_code), month=month(month_code))


#remove unnecessary columns
cdc_california_1999_2018 <- select(cdc_california_1999_2018, county_code, deaths, year, month)


###########################
#IMPORT CDC POPULATION DATA
###########################


#read in data
pop_california_1990_2018 <- read_delim("~/Desktop/spring 2020/env 175/Project 4/pop/pop_california_1990_2018.txt",
                                       "\t", escape_double = FALSE, trim_ws = TRUE)


###########
#CLEAN DATA
###########


#clean the column names
pop_california_1990_2018 <- clean_names(pop_california_1990_2018)


#rename columns
pop_california_1990_2018 <- rename(pop_california_1990_2018, year=yearly_july_1st_estimates)


#remove unnecessary columns
pop_california_1990_2018 <- select(pop_california_1990_2018, county_code, year, population)


#remove rows with year before 1999
pop_california_1999_2018 <- filter(pop_california_1990_2018, year>=1999)


##########
#JOIN DATA
##########


#join data
cdc_pop_1999_2018 <- full_join(pop_california_1999_2018,
                               cdc_california_1999_2018,
                               by = c("county_code", "year"))


#destring variables
cdc_pop_1999_2018 <- mutate(cdc_pop_1999_2018,
                            county_code=destring(county_code), population=destring(population), deaths=destring(deaths))


#add a countyfip column
cdc_pop_1999_2018 <- mutate(cdc_pop_1999_2018, countyfip=county_code-6000)


############
#DEATH RATES
############


#create a death rate per 100,000 people
cdc_pop_1999_2018 <- mutate(cdc_pop_1999_2018, deathrate=deaths/population*100000)


#show the summary of deathrates
summary(cdc_pop_1999_2018$deathrate)


#########################
#IMPORT GHCN WEATHER DATA
#########################


#read in data
ghcn_california_1999_2019 <- read_csv("~/Desktop/spring 2020/env 175/Project 4/ghcn/ghcn_california_1999_2019.csv",
                                      col_types = cols(DATE = col_date(format = "%Y-%m-%d")))


###########
#CLEAN DATA
###########


#clean the column names
ghcn_california_1999_2019 <- clean_names(ghcn_california_1999_2019)


#remove unnecessary columns
ghcn_california_1999_2019 <- select(ghcn_california_1999_2019, -name, -latitude, -longitude, -elevation)


#rename columns -- CONSISTENT TIME UNIT FOR WEATHER DATA
ghcn_california_1999_2019 <- mutate(ghcn_california_1999_2019, year=year(date), month=month(date))


#remove rows with year before 2019
ghcn_california_1999_2018 <- filter(ghcn_california_1999_2019, year<2019)


#show the summary of maximum temperatures
summary(ghcn_california_1999_2018$tmax)


##########################
#JOIN WITH DISTANCE MATRIX
##########################


#join weather data and stations within counties
ghcn_stn_within <- full_join(county_station_within, ghcn_california_1999_2018, by = "station")


#####################################
#AVERAGE WEATHER DATA BY COUNTY-MONTH
#####################################


#group by county-year-month
ghcn_stn_within <- group_by(ghcn_stn_within, countyfip, year, month)


#find the average temperatures
ghcn_county_within <- summarise(ghcn_stn_within, tmax=mean(tmax, na.rm = TRUE))


#ungroup
ghcn_stn_within <- ungroup(ghcn_stn_within)


##########################################
#JOIN CDC & GHCN DATA AT COUNTY-YEAR-MONTH
##########################################


#join data
cdc_ghcn_within <- full_join(ghcn_county_within,
                             cdc_pop_1999_2018,
                             by = c("countyfip", "year", "month"))


###################################
#SUMMARIZE/COLLAPSE TO COUNTY LEVEL
###################################


#group
cdc_ghcn_within <- group_by(cdc_ghcn_within, countyfip)


#summarize
cdc_ghcn_within_county_avg <- summarise(cdc_ghcn_within,
                                        tmax_avg=weighted.mean(tmax, population, na.rm = TRUE))


########################
#MAP COUNTY TEMPERATURES
########################


#join california counties map and tmax_average
mappable <- full_join(map_california_counties,
                      cdc_ghcn_within_county_avg,
                      by = c("countyfip"))


#map of average temperatures in california counties
ggplot(mappable, aes(y=lat, x=long, group=group, fill=tmax_avg)) +
  geom_polygon(color="black") +
  coord_fixed(1.3) +
  theme(panel.background = element_rect(fill="white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_gradient(name = "Average Temperature",
                      low="cyan", high="darkblue", na.value = "gray90",
                      breaks = seq(0, 90, 5))


ggsave("~/Desktop/spring 2020/env 175/Project 4/Tutorial Outputs/map_temps_california.pdf")


#############
#MAKE TABLE 1
#############


#group, weighted means of temp and deathrate, ungroup
final_county_hotgroup_month <- cdc_ghcn_within %>%
  group_by(countyfip) %>%
  summarise_at(vars(tmax, deathrate), funs(weighted.mean(.,population, na.rm=TRUE))) %>%
  ungroup(final_county_hotgroup_month)


#find the median of the tmax
median <- median(final_county_hotgroup_month$tmax, na.rm = TRUE)


#new column of hot (above median) or cold (below median) counties
final_county_hotgroup_month <- mutate(final_county_hotgroup_month,
                                      hotgroup = ifelse(tmax > median, "Hot county", "Cold county"))


#join hotgroup data and deathrate data
final_county_hotgroup_month <- full_join(cdc_ghcn_within,
                                         final_county_hotgroup_month, by="countyfip")


#group, weighted means of above new groupings, remove NA rows, ungroup
final_county_hotgroup_month <- final_county_hotgroup_month %>%
  group_by(hotgroup, month) %>%
  summarise_at(vars(tmax.x, deathrate.x),
               funs(weighted.mean(.,population, na.rm=TRUE))) %>%
  na.omit() %>%
  ungroup()


#rename
final_county_hotgroup_month <- rename(final_county_hotgroup_month, tmax = tmax.x, deathrate = deathrate.x)


#############
#MAKE TABLE 2
#############


#add season column
final_county_hotgroup_seas <- mutate(final_county_hotgroup_month,
                                     season = ifelse(month >5 & month < 9, "Summer", "Non-Summer"))


#remove unnecessary column
final_county_hotgroup_seas <- select(final_county_hotgroup_seas, -month)


#group by season
final_county_hotgroup_seas <- final_county_hotgroup_seas %>%
  group_by(hotgroup, season) %>%
  summarize(tmax = mean(tmax, na.rm = TRUE), deathrate = mean(deathrate, na.rm = TRUE)) %>%
  na.omit() %>%
  ungroup()


########
#FIGURES
########


#COMMENT THIS OUT ONCE YOU FIGURE OUT HOW TO CREATE DATA YOURSELF
#final_county_hotgroup_month <- read_csv("~/Desktop/spring 2020/env 175/Project 4/final_county_hotgroup_month - Sheet1.csv")


#COMMENT THIS OUT ONCE YOU FIGURE OUT HOW TO CREATE DATA YOURSELF
#final_county_hotgroup_seas <- read_csv("~/Desktop/spring 2020/env 175/Project 4/final_county_hotgroup_seas - Sheet1.csv")


#hot counties bar graph
ggplot(filter(final_county_hotgroup_seas, hotgroup=="Hot county"),
       aes(y=deathrate, x=season)) +
  geom_bar(stat="identity", width = 0.5, color = "darkblue", fill = "cadetblue2") +
  labs(y = "Death rate", x = "Hot Counties") +
  scale_y_continuous(expand = c(0,0)) +
  theme(panel.background = element_rect(fill="white"),
        axis.line = element_line(colour = "black"),
        legend.title = element_blank() )


ggsave("~/Desktop/spring 2020/env 175/Project 4/Tutorial Outputs/bar_deathrate_seas_hotgroup_california_1999_2018.pdf")


#all counties bar graph
ggplot(final_county_hotgroup_seas, aes(fill = season, y = deathrate, x = hotgroup)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("cyan2","darkorange")) +
  labs(y = "Death rate") +
  scale_y_continuous(expand = c(0,0)) +
  theme(panel.background = element_rect(fill="white"),
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.title = element_blank() )


ggsave("~/Desktop/spring 2020/env 175/Project 4/Tutorial Outputs/bar_deathrate_seas_bothgroup_california_1999_2018.pdf")


#hot counties line graph
ggplot(filter(final_county_hotgroup_month, hotgroup == "Hot county"), aes(y = deathrate, x = month)) +
  geom_point(color = "darkblue") +
  geom_line(color = "cyan3") +
  labs(y = "Death rate") +
  scale_x_continuous(breaks = 1:12,
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) +
  theme(panel.background = element_rect(fill="white"),
        legend.position = "none",
        axis.line = element_line(colour = "black"),
        axis.title.x = element_blank())


ggsave("~/Desktop/spring 2020/env 175/Project 4/Tutorial Outputs/scat_deathrate_month_hotgroup_california_1999_2018.pdf")


#all counties line graph
ggplot(final_county_hotgroup_month, aes(x = month, y = deathrate)) +
  geom_line(aes(color = hotgroup, linetype = hotgroup)) +
  scale_color_manual(values = c("cyan3", "darkorange")) +
  scale_linetype_manual(values=c("solid", "longdash")) +
  labs(y = "Death rate") +
  scale_x_continuous(breaks = 1:12,
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        legend.title = element_blank())


ggsave("~/Desktop/spring 2020/env 175/Project 4/Tutorial Outputs/scat_deathrate_month_bothgroup_california_1999_2018.pdf")