load("FFX_res_data.Rdata")
library(tidyr)
library(dplyr)
library(ggplot2)

#size of house, pre 1840
house_size <- FFX_res_data %>%
  select(year_built, sqft_living_area, decade) %>%
  filter(year_built < 1840)

#x number fireplaces and size of house
fp_sqft <- FFX_res_data %>%
  select(fireplaces, year_built, sqft_living_area, decade)%>%
  filter(year_built < 1840)

#this function determines if a house has a fireplace or not.
has_fireplace <- function(n) {
  ifelse(n == 0, FALSE, TRUE)
}

#calculate how many houses were built per decade
FFX_houses_built_per_decade <- FFX_res_data %>%
  group_by(decade) %>%
  summarize(houses_built = n())


#this adds a column which indicates if a fireplace is present (TRUE) or not (FALSE) using function has_fireplace(), then
#groups the data by decade and whether it has a fireplace, then
#counts the data, then
#joins it with information about houses built per decade, then
#calculates the percentage built with and without fireplaces
FFX_res_data_sum <- FFX_res_data %>%
  mutate(has_fp = has_fireplace(fireplaces)) %>%
  group_by(decade, has_fp) %>%
  summarize(n = n()) %>%
  left_join(FFX_houses_built_per_decade, by = "decade") %>%
  mutate(percent = n/houses_built)

