# --------------------------------------------------
# NOTE: This script requires private datasets that are not included due to confidentiality. 
# Replace the paths with your own files structured similarly to the expected data frames.
# --------------------------------------------------


#A map of Oregon showing all patient locations as points.
#Patient locations overlaid on the map, with point colors indicating travel distances to hospitals


#Import libraries
library(readxl)
library(ggplot2)
library(maps)
library(tidyverse)

#Import data
addresses <- read_excel("data")

#Turn lat and lng variables into numeric
addresses$lat <- as.numeric(addresses$lat)
addresses$lng <- as.numeric(addresses$lng)

#Create outline of Oregon (or any state)
MainStates <- map_data("state")
Cali <- subset(MainStates, region %in% c("oregon"))

ca_df <- MainStates %>%
  filter(region == "oregon")

counties <- map_data("county")
ca_county <- counties %>%
  filter(region == "oregon")

#Plot outline of Oregon (or any state)
ca_base <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) +
  coord_quickmap() +
  geom_polygon(color = "black", fill = "white")
ca_base + theme_void()

#Plot points for each location
ca_base +
  geom_point(data = addresses, 
             aes(x = lng, y = lat),
             size = 3, 
             inherit.aes = FALSE)

#Import patient data with lat and long values
data2 <- read_excel("data")

#Make patient lat and long values numeric
data2$res_lattitude <- as.numeric(data2$res_lattitude)
data2$res_longitude <- as.numeric(data2$res_longitude)

#Add points for each patient
#Color of the point is based on travel_distances
ca_base +
  geom_point(data = data2, 
             aes(x = res_longitude, y = res_lattitude, color = travel_distance), 
             size = 3, 
             inherit.aes = FALSE) 
