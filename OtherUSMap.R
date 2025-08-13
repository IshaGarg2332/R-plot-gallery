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
library(sf)


#Visualizes state-level percentage data using a map of the US, with shading from white to blue based on the Percentage variable
#Divides the country into four regions (Northeast, Midwest, South, West) and outlines each region with a bold black border using sf geometry unions


#Import data
state_dm <- read_excel("data.xlsx")

#Combine data with R US map data
us_map_data <- us_map(regions = "states")
map_sf <- left_join(us_map_data, state_dm, by = c("full" = "state"))

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

# Plot the US map with the 4 regions outlined
ggplot() +
  geom_sf(data = map_sf, aes(fill = Percentage), color = "white", size = 0.25) +
  geom_sf(data = outline1, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = outline2, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = outline3, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = outline4, fill = NA, color = "black", linewidth = 1) +
  scale_fill_gradient(low = "white", high = "blue", name = "Percentage") +
  theme_void() +
  theme(legend.position = "right")
