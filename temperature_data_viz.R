setwd("/Users/cassidywood/Desktop/spring 2020/env 175/Project 2/ghcn_lax_1968_2019")

#scatterplot of 2019 data
ggplot(ghcn_lax_2019, aes(date, tmax)) + geom_point(colour="dark green", size = 1)

#scatterplot of 2018 data
ghcn_lax_2018 <- read_csv("/Users/cassidywood/Desktop/spring 2020/env 175/Project 2/ghcn_lax_1968_2019/ghcn_lax_2018.csv")
View(ghcn_lax_2018)
ggplot(ghcn_lax_2018, aes(date, tmax)) + geom_point(colour="dark blue", size = 1)

#combine and scatterplot 2018 and 2019 data
ghcn_lax_2018_2019 <- bind_rows(ghcn_lax_2018, ghcn_lax_2019)
View(ghcn_lax_2018_2019)
ggplot(ghcn_lax_2018_2019, aes(date, tmax)) + geom_point(colour="magenta", size = 1)

#install libraries
library(ggplot2)
library(dplyr)
library(readr)

#assign data from each year to variables
ghcn_lax_2019 <- read_csv("/Users/cassidywood/Desktop/spring 2020/env 175/Project 2/ghcn_lax_1968_2019/ghcn_lax_2019.csv")
ghcn_lax_2018 <- read_csv("/Users/cassidywood/Desktop/spring 2020/env 175/Project 2/ghcn_lax_1968_2019/ghcn_lax_2018.csv")
ghcn_lax_2017 <- read_csv("/Users/cassidywood/Desktop/spring 2020/env 175/Project 2/ghcn_lax_1968_2019/ghcn_lax_2017.csv")
ghcn_lax_2016 <- read_csv("/Users/cassidywood/Desktop/spring 2020/env 175/Project 2/ghcn_lax_1968_2019/ghcn_lax_2016.csv")
ghcn_lax_2015 <- read_csv("/Users/cassidywood/Desktop/spring 2020/env 175/Project 2/ghcn_lax_1968_2019/ghcn_lax_2015.csv")
ghcn_lax_2014 <- read_csv("/Users/cassidywood/Desktop/spring 2020/env 175/Project 2/ghcn_lax_1968_2019/ghcn_lax_2014.csv")
ghcn_lax_2013 <- read_csv("/Users/cassidywood/Desktop/spring 2020/env 175/Project 2/ghcn_lax_1968_2019/ghcn_lax_2013.csv")
ghcn_lax_2012 <- read_csv("/Users/cassidywood/Desktop/spring 2020/env 175/Project 2/ghcn_lax_1968_2019/ghcn_lax_2012.csv")
ghcn_lax_2011 <- read_csv("/Users/cassidywood/Desktop/spring 2020/env 175/Project 2/ghcn_lax_1968_2019/ghcn_lax_2011.csv")
ghcn_lax_2010 <- read_csv("/Users/cassidywood/Desktop/spring 2020/env 175/Project 2/ghcn_lax_1968_2019/ghcn_lax_2010.csv")

#combine the data from each year into one variable
ghcn_lax_2010_2019 <- bind_rows(ghcn_lax_2010, ghcn_lax_2011, ghcn_lax_2012, ghcn_lax_2013, ghcn_lax_2014, ghcn_lax_2015, ghcn_lax_2016, ghcn_lax_2017, ghcn_lax_2018, ghcn_lax_2019)

#export combined data as a csv file
write_csv(ghcn_lax_2010_2019, "/Users/cassidywood/Desktop/spring 2020/env 175/Project 2/ghcn_lax_1968_2019/ghcn_lax_2010_2019.csv")

#scatterplot of combined (decade) data
ggplot(ghcn_lax_2010_2019, aes(y=tmax, x=date)) +
  geom_point(colour="cornflowerblue", size = 0.5) +
  ylab("Max Temperature") +
  xlab("") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme(panel.background = element_rect(fill="white"))

#save scatterplot
ggsave("/Users/cassidywood/Desktop/spring 2020/env 175/Project 2/ghcn_lax_1968_2019/scat_tmax_date_lax_2010_2019.pdf")

#import 1968 data to have something to bind other years' data to
ghcn_lax_1968 <- read_csv("/Users/cassidywood/Desktop/spring 2020/env 175/Project 2/ghcn_lax_1968_2019/ghcn_lax_1968.csv")
ghcn_lax_1968_2019 <- ghcn_lax_1968

#import each year of csv file starting at 1969
for (year in 1969:2019){
  #temporary names
  tempname <- paste0("ghcn_lax_", year)
  csvname <- paste0("/Users/cassidywood/Desktop/spring 2020/env 175/Project 2/ghcn_lax_1968_2019/ghcn_lax_", year, ".csv")
  
  #import
  tempdata <- read_csv(csvname)
  
  #bind data and overwrite ghcn_lax_1968_2019
  ghcn_lax_1968_2019 <- bind_rows(ghcn_lax_1968_2019, tempdata)
  
  #rename tempdata as tempname to start the loop over
  assign(tempname, tempdata)
}
rm(tempdata, csvname, tempname, year)

#save data
write.csv(ghcn_lax_1968_2019, "/Users/cassidywood/Desktop/spring 2020/env 175/Project 2/ghcn_lax_1968_2019/ghcn_lax_1968_2019.csv")

#create a scatterplot of all the years
ggplot(ghcn_lax_1968_2019, aes(y=tmax, x=date)) +
  geom_point(colour="dark red", size = 0.5) +
  labs(y = "Max Temperature", x = "") +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  theme(panel.background = element_rect(fill="white"))

#save scatterplot
ggsave("/Users/cassidywood/Desktop/spring 2020/env 175/Project 2/ghcn_lax_1968_2019/scat_tmax_date_lax_1968_2019.pdf")

#goal: create period variable and table of averages

#install libraries
library(lubridate)

#make a copy of data
ghcn_group <- ghcn_lax_1968_2019

#add year variable
ghcn_group = mutate(ghcn_group, year = year(date))

#create period variable
ghcn_group <- mutate(ghcn_group, period = ifelse(year<1986, "1968-1985", "1986-2019"))

#group data
ghcn_group <- group_by(ghcn_group, period)

#average max temperature date per period
ghcn_period_avg <- summarise(ghcn_group, tmax = mean(tmax))

#ungroup
ungroup(ghcn_group)

#bar graph
ggplot(ghcn_period_avg, aes(y=tmax, x=period)) +
  geom_bar(stat="identity", fill = "aquamarine2", width = 0.5) +
  labs(y = "Max Temperature", x = "Period") +
  theme(panel.background = element_rect(fill="black"))
ggsave("/Users/cassidywood/Desktop/spring 2020/env 175/Project 2/ghcn_lax_1968_2019/bar_tmax_period_lax_1968_2019.pdf")

#scatterplot
ggplot(ghcn_period_avg, aes(y=tmax, x=period)) +
  geom_point(stat="identity", colour = "red3", size = 5) +
  labs(y = "Maximum Temperature", x = "Time Period") +
  ylim(70, 71) +
  theme(panel.background = element_rect(fill="papayawhip"))
ggsave("/Users/cassidywood/Desktop/spring 2020/env 175/Project 2/ghcn_lax_1968_2019/scat_tmax_period_lax_1968_2019.pdf")

#now group data by year
ghcn_group <- group_by(ghcn_group, year)

#average max temperature date per year
ghcn_year_avg <- summarise(ghcn_group, tmax = mean(tmax))

#ungroup again
ungroup(ghcn_group)

#line graph
ggplot(ghcn_year_avg, aes(y=tmax, x=year)) +
  geom_line(colour = "coral2") +
  labs(y = "Maximum Temperature", x = "") +
  #theme_minimal()
  theme(panel.background = element_rect(fill="white"))
ggsave("/Users/cassidywood/Desktop/spring 2020/env 175/Project 2/ghcn_lax_1968_2019/line_tmax_year_lax_1968_2019.pdf")
