setwd("/Users/cassidywood/Desktop/spring 2020/env 175/Project 3")


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


#####################
#IPUMS (POVERTY) DATA
#####################


#import and view dataset
ipums_california_2018 <- read_csv("~/Desktop/spring 2020/env 175/Project 3/ipums_california_2018.csv")
View(ipums_california_2018)


#clean the data
ipums_clean <- clean_names(ipums_california_2018)


#keep certain variables
ipums_clean <- select(ipums_clean, "hhwt", "statefip", "countyfip", "poverty")
ipums_clean <- filter(ipums_clean, poverty != 0)


#poverty dummy variable
ipums_clean <- mutate(ipums_clean, pov100=ifelse(poverty<100,1,0))


#dummy variable for everyone
ipums_clean <- mutate(ipums_clean, pop=1)


#duplicate poverty dummy variable
ipums_clean <- mutate(ipums_clean, povrate100_2=pov100)


#group the clean data by county
ipums_group <- group_by(ipums_clean, countyfip)


#summarize the groups and view summary
ipums_county_sum <- summarise(ipums_group, pov100=sum(pov100), pop=sum(pop), povrate100_2=mean(povrate100_2))
View(ipums_county_sum)


#ungroup
ungroup(ipums_group)


#calculate poverty rate
ipums_county_sum <- mutate(ipums_county_sum, povrate100=pov100/pop)


#save
write_csv(ipums_county_sum, "~/Desktop/spring 2020/env 175/Project 3/ipums_povrate_california_2018.csv")


#######################
#JOIN COUNTY FIPS CODES
#######################


#read in county names
fips_names_california <- read_csv("fips_names_california.csv")
View(fips_names_california)


#join the names
ipums_names <- full_join(ipums_county_sum, fips_names_california, by = "countyfip")
View(ipums_names)


##########################
#READ IN/CLEAN UP EPA DATA
##########################


#read in epa data
annual_aqi_by_county_2018 <- read_csv("annual_aqi_by_county_2018.csv")
View(annual_aqi_by_county_2018)


#fix variable names
annual_aqi_by_county_2018 <- clean_names(annual_aqi_by_county_2018)


#make names lowercase
annual_aqi_by_county_2018 <- mutate(annual_aqi_by_county_2018, state = tolower(state), county=tolower(county))


#filter california and drop countyfip 0
epa_california_2018 <- filter(annual_aqi_by_county_2018, state=="california")
View(epa_california_2018)


##########
#JOIN DATA
##########


epa_california_2018 <- rename(epa_california_2018, county_name=county)
final_ipums_epa <- full_join(epa_california_2018, ipums_names, by = "county_name")


#drop unneeded variables
final_ipums_epa <- select(final_ipums_epa, -"state", -"state_name")
final_ipums_epa <- mutate(final_ipums_epa, bad_days=unhealthy_for_sensitive_groups_days+unhealthy_days+very_unhealthy_days+hazardous_days)
write.csv(final_ipums_epa, "~/Desktop/spring 2020/env 175/Project 3/pollution_poverty_california_2018.csv")


###########
#PLOT PT. 1
###########


#scatterplot
ggplot(final_ipums_epa, aes(y=bad_days, x=povrate100)) +
  geom_point(colour = "darkgreen") +
  labs(y = "Bad Pollution Days", x = "Poverty Rate (100% Cutoff)") +
  theme_minimal()


#####
#MAPS
#####


#read in data
map_us_counties <- map_data("county")
map_california_counties <- filter(map_us_counties, region == "california")
View(map_california_counties)


#rename variables
map_california_counties <- rename(map_california_counties, state=region, county_name=subregion)


#join ipums and epa data
final_map <- full_join(final_ipums_epa, map_california_counties, by = "county_name")
View(final_map)


#creates a map with county lines
ggplot(final_map, aes(y=lat, x=long, group=group, fill=povrate100)) +
  geom_polygon(color="black") +
  coord_fixed(1.3) +
  theme(panel.background = element_rect(fill="white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank() ) +
  scale_fill_gradient(name = "Poverty Rate", low="yellow", high="red", breaks = seq(0.08, 0.20, 0.04))


#save the graph
ggsave("~/Desktop/spring 2020/env 175/Project 3/map_poverty_california_2018.pdf")


#repeat graphing process for bad_days
ggplot(final_map, aes(y=lat, x=long, group=group, fill=bad_days)) +
  geom_polygon(color="black") +
  coord_fixed(1.3) +
  theme(panel.background = element_rect(fill="white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank() ) +
  scale_fill_gradient(name = "Bad Pollution Days", low="yellow", high="red")


#save the graph
ggsave("~/Desktop/spring 2020/env 175/Project 3/map_pollution_california_2018.pdf")


###########
#PLOT PT. 2
###########


#unweighted scatterplot
ggplot(final_ipums_epa, aes(y=bad_days, x=povrate100)) +
  geom_point(color = "blue") +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(y = "Bad Air Days", x = "Poverty Rate") +
  theme(panel.background = element_rect(fill="white"), axis.line=element_line(color = "gray", size = 0.5, linetype = "solid"))


#save
ggsave("~/Desktop/spring 2020/env 175/Project 3/scat_bad_days_povrate100_unwgt_california_2018.pdf")


#weighted scatterplot
ggplot(final_ipums_epa, aes(y=bad_days, x=povrate100, size=pop, weight=pop)) +
  geom_point(pch = 1, color = "blue") +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(y = "Bad Air Days", x = "Poverty Rate") +
  theme(panel.background = element_rect(fill="white"), legend.position = "none", axis.line=element_line(color = "gray", size = 0.5, linetype = "solid"))


#save
ggsave("~/Desktop/spring 2020/env 175/Project 3/scat_bad_days_povrate100_wgt_california_2018.pdf")


############################
#GROUP INTO HIGH/LOW POVERTY
############################


#calculate median of the poverty rates
#final_ipums_epa <- mutate(final_ipums_epa, pov_median=median(povrate100, na.rm = TRUE))
pov_median <- median(final_ipums_epa$povrate100, na.rm = TRUE)


#group
final_ipums_epa <- mutate(final_ipums_epa, povgroup=ifelse(povrate100>pov_median, "High poverty", "Low poverty"))


#group by
final_ipums_epa <- group_by(final_ipums_epa, povgroup)


#calculate means by group
final_ipums_epa <- mutate(final_ipums_epa, bad_days_wgt=bad_days)
final_group_sum <- summarise(final_ipums_epa, bad_days=mean(bad_days, na.rm = TRUE), bad_days_wgt=weighted.mean(bad_days_wgt, pop, na.rm = TRUE))
ungroup(final_ipums_epa)
View(final_group_sum)


#drop missing observations
final_group_sum <- na.omit(final_group_sum)


#unweighted bargraph
ggplot(final_group_sum, aes(y=bad_days, x=povgroup)) +
  geom_bar(stat="identity", color = "darkblue", fill = "darkblue") +
  labs(y = "Bad Air Days") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,70)) +
  theme(panel.background = element_rect(fill="white"),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.line = element_line(color = "black") )


#save
ggsave("~/Desktop/spring 2020/env 175/Project 3/bar_bad_days_povrate100_unwgt_california_2018.pdf")


#weighted bargraph
ggplot(final_group_sum, aes(y=bad_days_wgt, x=povgroup)) +
  geom_bar(stat="identity", color = "darkgreen", fill = "darkgreen") +
  labs(y = "Bad Air Days") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,100)) +
  theme(panel.background = element_rect(fill="white"),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.line = element_line(color = "black") )


#save
ggsave("~/Desktop/spring 2020/env 175/Project 3/bar_bad_days_povrate100_wgt_california_2018.pdf")