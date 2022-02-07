library(tidyverse)
library(lubridate)
data = read_csv("restaurant_inspections.csv")
View(data[1:20,])
data$DATE_ = ymd_hms(data$DATE_)
data$month_year = floor_date(data$DATE_, unit="month")
# This was to give the dates an easier format
View(data[1:20,])                             
monthly_inspections = group_by(data, month_year)
# This was to group the data in chronological order
View(data[1:20,])
ggplot(monthly_inspections, aes(x=DATE_, y=SCORE)) +
geom_point()
# This was for the visualization question. Shows the distribution of inspection
# scores. Chose this type of graph to show increase in quantity.
data$RESTAURANTOPENDATE = ymd_hms(data$RESTAURANTOPENDATE)
data$opendate = floor_date(data$RESTAURANTOPENDATE, unit="month")
restaurant_age = group_by(data,opendate)
ggplot(restaurant_age, aes(x=RESTAURANTOPENDATE, y=SCORE)) +
  geom_point()
# Organized restaurant opening by date and graphed it with scores.
unique(data$CITY)
data$fullcity = recode(data$CITY, "RALEIGH" = "raleigh", "Raleigh" = "raleigh",
                       "CARY" = "cary", "Cary" = "cary", "FUQUAY VARINA" = "FV",
                       "FUQUAY-VARINA" = "FV", "Fuquay Varina" = "FV", "Fuquay-Varina" = "FV",
                       "GARNER" = "garner", "Garner" = "garner", "MORRISVILLE" = "morrisville",
                       "Morrisville" = "morrisville", "MORRISVILE" = "morrisville", 
                       "RESEARCH TRIANGLE PARK" = "RTP", "APEX" = "Apex", "HOLLY SPRINGS" = "Holly Springs",
                       "HOLLY SPRING" = "Holly Springs", "ZEBULON" = "Zebulon",
                       "WAKE FOREST" = "Wake Forest")
unique(data$fullcity)
group_by(data, fullcity) %>%
  summarize(mean_SCORE=mean(SCORE))
# Reorganized all the cities together and then found the mean scores by city
unique(data$INSPECTOR)
group_by(data, INSPECTOR) %>%
  summarise(mean_SCORE=mean(SCORE))
# Looking for variance by inspector by using mean score

count(data, INSPECTOR)
count(data, fullcity)
# Used this command to count sample size
unique(data$FACILITYTYPE)
group_by(data, FACILITYTYPE) %>%
  summarize(mean_SCORE=mean(SCORE))
# Used to find mean score by facility type
restaurant_data = filter(data, FACILITYTYPE == "Restaurant")
ggplot(restaurant_data, aes(x=DATE_, y=SCORE)) +
  geom_point()
# Question 1 but with only restaurant data
ggplot(restaurant_data, aes(x=RESTAURANTOPENDATE, y=SCORE)) +
  geom_point()
# Question 2 but with only restaurant data
group_by(restaurant_data, fullcity) %>%
  summarize(mean_SCORE=mean(SCORE))
# Question 3 but with only restaurant data
group_by(restaurant_data, INSPECTOR) %>%
  summarise(mean_SCORE=mean(SCORE))
# Question 4 but with only restaurant data
count(restaurant_data, INSPECTOR)
count(restaurant_data, fullcity)
# Question 5 but with only restaurant data\

