library(dplyr)
library(tidyr)
library(sp)
library(ggplot2)
library(rgdal)
library(RColorBrewer)

#Bring the data in from the .csv file downloaded from NHGIS.
heating_1940 <- read.csv("nhgis1940.csv", stringsAsFactors = FALSE)


# Code help courtesy of Debby Kermer Let's get a weighted average of home values for each census tract.
heating_1940$weighted_total <-
  (heating_1940$ BVB001*250)+
  (heating_1940$ BVB002*600)+
  (heating_1940$ BVB003*850)+
  (heating_1940$ BVB004*1250)+
  (heating_1940$ BVB005*1750)+
  (heating_1940$ BVB006*2250)+
  (heating_1940$ BVB007*2750)+
  (heating_1940$ BVB008*3500)+
  (heating_1940$ BVB009*4500)+
  (heating_1940$ BVB010*5500)+
  (heating_1940$ BVB011*6750)+
  (heating_1940$ BVB012*8750)+
  (heating_1940$ BVB013*12500)+
  (heating_1940$ BVB014*17500)+
  (heating_1940$ BVB015*30000)
#Calculate the total number of houses in a tract.
heating_1940$total_houses <-
  (heating_1940$ BVB001)+
  (heating_1940$ BVB002)+
  (heating_1940$ BVB003)+
  (heating_1940$ BVB004)+
  (heating_1940$ BVB005)+
  (heating_1940$ BVB006)+
  (heating_1940$ BVB007)+
  (heating_1940$ BVB008)+
  (heating_1940$ BVB009)+
  (heating_1940$ BVB010)+
  (heating_1940$ BVB011)+
  (heating_1940$ BVB012)+
  (heating_1940$ BVB013)+
  (heating_1940$ BVB014)+
  (heating_1940$ BVB015)
#Calculate the weighted average value of homes in a tract.
heating_1940$weighted_average <-
  heating_1940$weighted_total / heating_1940$total_houses


#Get the percentage of houses with and without central heat.
heating_1940 <- heating_1940 %>%
  mutate(percent_with_heat = BVO001 / (BVO001 + BVO002)) %>%
  mutate(percent_without_heat = BVO002 / (BVO001 + BVO002))



