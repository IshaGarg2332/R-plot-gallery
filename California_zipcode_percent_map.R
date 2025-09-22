# --------------------------------------------------
# NOTE: This script requires private datasets that are not included due to confidentiality. 
# Replace the paths with your own files structured similarly to the expected data frames.
# --------------------------------------------------


#This map visualizes percentages by ZIP code in California
#It uses shapefiles for CA ZIP codes and merges them with the dataset percentages


#Import libraries
library(tigris)
library(ggplot2)
library(readxl)

#Import data
dataframe <- read_excel("data")

#Get california data
zipcodes <- zctas(year = 2000, state = "ca", cb = TRUE)
together <- merge(zipcodes,percentage,by="ZCTA",all.y=T)
us_states <- states(progress_bar = FALSE)
ca <- us_states |> filter(STUSPS == "ca")

#Create California plot by percentages
ggplot() +
  geom_sf(data = dataframe, fill = "white") +
  geom_sf(data = together, aes(fill = percent), linewidth = 0.1) +
  scale_fill_gradientn(colors = c("#A7C7E7", "#3690E6", "black"),
                       values = c(0, 0.1, 3.5),
                       name="title") +
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank()) +
  theme_minimal()


