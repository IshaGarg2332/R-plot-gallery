# --------------------------------------------------
# NOTE: This script requires private datasets that are not included due to confidentiality. 
# Replace the paths with your own files structured similarly to the expected data frames.
# --------------------------------------------------


#This set of heatmaps shows total wildfire days in California by ZIP code
#Each map represents a different year from 2016 to 2019


#Import libraries
library(tigris)
library(ggplot2)
library(readxl)
library(dplyr)
library(grid) 
library(gridExtra)
library(ggpubr)
library(sf)
library(RColorBrewer)
library(scales)

#Import and process data
df <- read_dta("data")

us_states <- states(progress_bar = FALSE)
ca <- us_states |> filter(STUSPS == "CA")
zipcodes <- zctas(year = 2000, state = "CA", cb = TRUE)


##2016
df2016 <- read_dta("data")
together2016 <- merge(zipcodes,df2016,by="ZCTA",all.y=T)
label(together$pc_df) <- "Total Wildfire days"

heatmap1 <- ggplot() +
  geom_sf(data = ca, fill = "white") +
  ggtitle("2016") +
  geom_sf(data = together, aes(fill = pc_df), linewidth = 0.1) +
  scale_fill_gradientn(
    colors = c("green", "#FFD8A8", "#FFB067", "#FF843C", "#FF5033", "#FF4040", "#FF1A1A", "#FF6666", "#E02B2B", "#D32F2F", "#B22222", "#9B1C1C", "#800000"),  
    values = rescale(c(0, 1, 2, 3, 4, 6, 8, 10, 12, 15, 18, 25, 30)),
    limits = c(0, 30),
    name="Total Wildfire days"
  ) +
  theme_minimal()


##2017
df2017 <- read_dta("data")
together2017 <- merge(zipcodes,df2017,by="ZCTA",all.y=T)
label(together$pc_df) <- "Total Wildfire days"

heatmap2 <- ggplot() +
  geom_sf(data = ca, fill = "white") +
  ggtitle("2017") +
  geom_sf(data = together, aes(fill = pc_df), linewidth = 0.1) +
  scale_fill_gradientn(
    colors = c("green", "#FFD8A8", "#FFB067", "#FF843C", "#FF5033", "#FF4040", "#FF1A1A", "#FF6666", "#E02B2B", "#D32F2F", "#B22222", "#9B1C1C", "#800000"),  
    values = rescale(c(0, 1, 2, 3, 4, 6, 8, 10, 12, 15, 18, 25, 30)),
    limits = c(0, 30),
    name="Total Wildfire days"
  ) +
  theme_minimal()


#2018
df2018 <- read_dta("data")
together2018 <- merge(zipcodes,df2018,by="ZCTA",all.y=T)
label(together$pc_df) <- "Total Wildfire days"

heatmap3 <- ggplot() +
  geom_sf(data = ca, fill = "white") +
  ggtitle("2018") +
  geom_sf(data = together, aes(fill = pc_df), linewidth = 0.1) +
  scale_fill_gradientn(
    colors = c("green", "#FFD8A8", "#FFB067", "#FF843C", "#FF5033", "#FF4040", "#FF1A1A", "#FF6666", "#E02B2B", "#D32F2F", "#B22222", "#9B1C1C", "#800000"),  
    values = rescale(c(0, 1, 2, 3, 4, 6, 8, 10, 12, 15, 18, 25, 30)),
    limits = c(0, 30),
    name="Total Wildfire days"
  ) +
  theme_minimal()


#2019
df2019 <- read_dta("data")
together2019 <- merge(zipcodes,df2019,by="ZCTA",all.y=T)
label(together$pc_df) <- "Total Wildfire days"

heatmap4 <- ggplot() +
  geom_sf(data = ca, fill = "white") +
  ggtitle("2019") +
  geom_sf(data = together, aes(fill = pc_df), linewidth = 0.1) +
  scale_fill_gradientn(
    colors = c("green", "#FFD8A8", "#FFB067", "#FF843C", "#FF5033", "#FF4040", "#FF1A1A", "#FF6666", "#E02B2B", "#D32F2F", "#B22222", "#9B1C1C", "#800000"),  
    values = rescale(c(0, 1, 2, 3, 4, 6, 8, 10, 12, 15, 18, 25, 30)),
    limits = c(0, 30),
    name="Total Wildfire days"
  ) +
  theme_minimal()


# Combine the heatmaps
grid.arrange(heatmap1, heatmap2, heatmap3, heatmap4, ncol = 2, nrow = 2)
