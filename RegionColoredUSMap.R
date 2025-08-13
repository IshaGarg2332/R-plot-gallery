# --------------------------------------------------
# NOTE: This script requires private datasets that are not included due to confidentiality. 
# Replace the paths with your own files structured similarly to the expected data frames.
# --------------------------------------------------

#Import libraries
library(usmap)
library(ggplot2)
library(dplyr)
library(readxl)
library(scales) 


#Aggregates U.S. states into four regions (Northeast, Midwest, South, West), each associated with a custom_value 
#Outlines and fills each region’s boundary using sf geometries and joins with the value data for visual encoding


#Create a dataset with 4 variables
data <- data.frame(
  grouping11 = c("group1", "group2", "group3", "group4"),
  custom_value = c(0.73, 0.77, 0.78, 0.67)
)

#Import R US map data
map_sf <- us_map(regions = "states")

#Northeast
group1 <- tolower(c("Connecticut", "Maine", "Massachusetts", "New Hampshire", 
                    "Rhode Island", "Vermont", "New Jersey", "New York", "Pennsylvania"))

#Midwest
group2 <- tolower(c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin", "Iowa", "Kansas", 
                    "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota"))

#South
group3 <- tolower(c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", 
                    "South Carolina", "Virginia", "District of Columbia", "West Virginia", 
                    "Alabama", "Kentucky", "Mississippi", "Tennessee", "Arkansas", 
                    "Louisiana", "Oklahoma", "Texas"))

#West
group4 <- tolower(c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", 
                    "Utah", "Wyoming", "Alaska", "California", "Hawaii", "Oregon", "Washington"))

#For each, select the states in the respective 4 regions
highlight1 <- map_sf %>% filter(tolower(full) %in% group1)
highlight2 <- map_sf %>% filter(tolower(full) %in% group2)
highlight3 <- map_sf %>% filter(tolower(full) %in% group3)
highlight4 <- map_sf %>% filter(tolower(full) %in% group4)

#Outline each of the four regions
outline1 <- st_union(highlight1)
outline2 <- st_union(highlight2)
outline3 <- st_union(highlight3)
outline4 <- st_union(highlight4)

#Combined data
regionsdata <- st_sf(
  grouping11 = c("group1", "group2", "group3", "group4"),
  geometry = st_sfc(outline1[[1]], outline2[[1]], outline3[[1]], outline4[[1]])
)
regionsdata1 <- left_join(regionsdata, data, by = "grouping11")
st_crs(regionsdata1) <- st_crs(map_sf)

# Plot the US map with the 4 regions outlined
ggplot() +
  geom_sf(data = map_sf, fill = NA, color = "black", size = 0.25) +
  geom_sf(data = regionsdata1, aes(fill = custom_value), color = "black", linewidth = 1, alpha = 0.5) +
  scale_fill_gradient(low = "white", high = "blue", name = "Region Value") +
  theme_void() +
  theme(legend.position = "right")
