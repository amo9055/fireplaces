load("FFX_res_data.Rdata")
library(dplyr)
library(ggplot2)
#keep ggplot in here for plots in files where this code is included.

#x number fireplaces and size of house
fp_sqft <- FFX_res_data %>%
  select(fireplaces, year_built, sqft_living_area, decade)%>%
  filter(year_built < 1840)

#calculate how many houses have x number of fireplaces
FFX_number_fp_per_house <-FFX_res_data %>%
  select(fireplaces, decade) %>%
  group_by(decade, fireplaces) %>%
  summarize(houses_with_x_fps = n())

#calculate how many houses were built per decade
FFX_houses_built_per_decade <- FFX_res_data %>%
  group_by(decade) %>%
  summarize(houses_built = n())

#join two tables together
FFX_number_fp_per_house <- FFX_number_fp_per_house %>%
  left_join(FFX_houses_built_per_decade, by = "decade")

#calculate percentage
FFX_number_fp_per_house <- FFX_number_fp_per_house %>%
  mutate(percentage = houses_with_x_fps/houses_built)
  

  
 