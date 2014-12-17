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


#Chart some data to see what it shows
ggplot(data = heating_1940,
       aes(x = COUNTY, y = weighted_average, size = percent_with_heat))+
  geom_point()+ facet_wrap(~STATE)
ggtitle("1940 Average Home Price and Percentage of Homes with Heat")

#Find out what is going on in Illinois
#Madison & St. Clair Counties are suburbs of St. Louis, which explains the different pattern from Cook County which is Chicago.
il_heating_1940 <- heating_1940 %>%
  filter(STATE == "Illinois")
ggplot(data = il_heating_1940,
       aes(x = COUNTY, y = weighted_average, size = percent_with_heat))+
  geom_point()+
  ggtitle("1940 Average Home Price and Percentage of Homes with Heat, Illinois")


#Find out what is going on in TX
#Dallas looks interesting
tx_heating_1940 <- heating_1940 %>%
  filter(STATE == "Texas")
ggplot(data = tx_heating_1940,
       aes(x = COUNTY, y = weighted_average, size = percent_with_heat))+
  geom_point()+
  ggtitle("1940 Average Home Price and Percentage of Homes with Heat, Texas")


#Home Price and Percentage of homes with Heat, by State
ggplot(data = heating_1940,
       aes(x = weighted_average, y = percent_with_heat))+
  geom_point()+ facet_wrap(~STATE)+
  geom_smooth()+
  ggtitle("1940 Average Home Price and Percentage of Homes with Heat")+
  ylim(0, 1) + xlim(0, 30000)


#Shape file that provides the county and state outlines for mapping
shape_simple1940 <- readOGR("nhgis1940_shape/", "US_tractcounty_1940_simplified")
#Fortify into dataframe for ggplot2
shape_simple1940_df <- fortify(shape_simple1940, region = "GISJOIN")


#As per NHGIS, this is the file needed to use to join with the data.
shape_tract_simple <- readOGR("nhgis1940_tract_shape/", "US_tract_1940_simplified")
shape_tract_simple_df <- fortify(shape_tract_simple, region = "GISJOIN")
shape_tract_simple_df <- shape_tract_simple_df %>%
  left_join(heating_1940, by = c("id" = "GISJOIN"))

#Create a map with the data as well as the state and county outlines 
#Percentage of homes with central heat
combined <- ggplot()+
  #This part has the data.
  geom_map(data = shape_tract_simple_df,
           map = shape_tract_simple_df,
           aes(x = long, y = lat, group = group, map_id = id, fill = percent_with_heat),
           color = "gray",
           size = 0.2)+
  coord_map()+
  #This part makes the state and county outlines
  geom_path(data = shape_simple1940_df,
            aes(x=long, y=lat, group=group),
            color="black", size=0.5)+
  theme_minimal()  
combined

dc <- combined +xlim(-77.25, -76.75) + ylim(38.75, 39) + ggtitle("District of Columbia")
dc

la <- combined +xlim(-119, -117.5) + ylim(32.75, 35) + ggtitle("Los Angeles, CA")
la

dallas <- combined + xlim(-97, -96.5) + ylim(32.5, 33) + ggtitle("Dallas, TX")
dallas


#weighted average value of homes per tract

house_values <- ggplot()+
  #This part has the data.
  geom_map(data = shape_tract_simple_df,
           map = shape_tract_simple_df,
           aes(x = long, y = lat, group = group, map_id = id, fill = weighted_average),
           color = "gray",
           size = 0.2)+
  coord_map()+
  labs(legend = "Weighted Average Home Value")+
  #This part makes the state and county outlines
  geom_path(data = shape_simple1940_df,
            aes(x=long, y=lat, group=group),
            color="black", size=0.5)+
  theme_minimal()  
house_values

dc_values <- house_values +xlim(-77.25, -76.75) + ylim(38.75, 39) + ggtitle("District of Columbia")
dc_values
