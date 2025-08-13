# --------------------------------------------------
# NOTE: This script requires private datasets that are not included due to confidentiality. 
# Replace the paths with your own files structured similarly to the expected data frames.
# --------------------------------------------------

#Import libraries
library("readxl")
library("tidyr")
library("dplyr")
library("tidyverse")
library("ggplot2")


#Plots a scatterplot of two variables, showing the relationship between the two variables.
#Adds a linear regression line (geom_smooth(method = "lm")) in black without a confidence interval


#Load in data
dataset <- read_excel("data.xlsx")

#Create plot with a line with linear fit
ggplot(dataset, aes(x, y)) + 
  geom_point() +
  xlab("") + 
  ylab("") +
  coord_cartesian(ylim = c(0, 0.6),
                  xlim = c(0, 0.2)) +
  theme(axis.text=element_text(size=10),
        plot.title = element_text(size=18),
        axis.title=element_text(size=14),
        legend.text = element_text(size = 13)) +
  geom_smooth(method = "lm",
              se = FALSE, 
              aes(color = "Linear Fit")) +
  scale_color_manual(values = c("Linear Fit" = "black")) +
  labs(colour = " ") +
  ggtitle("")


