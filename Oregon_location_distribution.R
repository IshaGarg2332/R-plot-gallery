# --------------------------------------------------
# NOTE: This script requires private datasets that are not included due to confidentiality. 
# Replace the paths with your own files structured similarly to the expected data frames.
# --------------------------------------------------


#The geographic distribution of locations across Oregon, showing where each one is located
#The relative percentage values for each location, represented by color-coded points


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

#Import data with percentages
data2 <- read_excel("data")

#Merge data that has lat and lng with data that has percentages
totaldataframe <- merge(data2, addresses, by = "hospid")

#Plot points of each location
#Color of the point is based on percentage values
ca_base +
  geom_point(data = totaldataframe, 
             aes(x = lng, y = lat, color = cut(percent, 
                                               breaks = c(-1, 1, 2, 4, 8, 12, 16, 20), 
                                               labels = c("0-1%", "1-2%", "2-4%", "4-8%", "8-12%", "12-16%", "16-20%")
             )), 
             size = 3, 
             inherit.aes = FALSE) +
  scale_color_manual(
    name = "Percentage of Hospitals",
    values = c("0-1%" = "#9D4EDD", 
               "1-2%" = "#577590", 
               "2-4%" = "#43AA8B", 
               "4-8%" = "#90BE6D", 
               "8-12%" = "#F9C74F", 
               "12-16%" = "#F3722C", 
               "16-20%" = "#E63946")) +
  theme(
    legend.background = element_rect(fill = "white"), 
    legend.key = element_rect(fill = "white")
  )
