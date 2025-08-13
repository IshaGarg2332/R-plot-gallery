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
library("readxl")
library("boot")
library("table1")
library("Hmisc")
library("sjmisc")
library("forcats")
library("ggrepel")
library("ggh4x")


#Plots percentages for each group (Value1 and Value2) across the ranges, connecting points with lines and showing data points with colored markers.
#Includes percentage labels above points, sets axis titles and breaks, customizes legend and text appearance, and limits y-axis between 0 and 100 for clarity.


#Create data manually (or can import)
Line1 <- tibble::tibble(values = c("Range1", "Range2", "Range3", "Range4", "Range5", "Range6", "Range7", "Range8"),
                       percentages = c(37.52, 84.19, 22.73, 55.40, 10.98, 68.27, 43.85, 79.61),
                       type = c("Value1", "Value1", "Value1", "Value1", "Value1", "Value1", "Value1", "Value1"))

Line1 <- Line1 %>% arrange(factor(values, levels = c("Range1", "Range2", "Range3", 
                                                     "Range4", "Range5", "Range6", 
                                                     "Range7", "Range8")))
                                    
Line2 <- tibble::tibble(values = c("Range1", "Range2", "Range3", "Range4", "Range5", "Range6", "Range7", "Range8"),
                        percentages = c(14.67, 91.28, 36.45, 58.99, 27.13, 72.54, 49.08, 63.31),
                        type = c("Value2", "Value2", "Value2", "Value2", "Value2", "Value2", "Value2", "Value2"))

#Arrange the order of the numbers for the graph
Line2 <- Line2 %>% arrange(factor(values, levels = c("Range1", "Range2", "Range3", 
                                                     "Range4", "Range5", "Range6", 
                                                     "Range7", "Range8")))

#Bind datasets into one
total <- rbind(Line1, Line2)

#Make a line graph showing percentages for each group across different value ranges
total1 <- total %>%
  mutate(values = fct_relevel(values, 
                              "Range1", "Range2", "Range3", 
                              "Range4", "Range5", "Range6", 
                              "Range7", "Range8")) %>%
ggplot(aes(x=values, y=percentages, group=type, label = percentages)) +
  geom_line(aes(color=type), size=0.7) +
  geom_point(aes(color=type), size=2.5) +
  ggtitle("Title") +
  xlab("X-axis") + 
  ylab("Y-axis") +
  labs(color='Legend Title') +
  geom_text(family = "Times New Roman", vjust=-1, size=4) + 
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(axis.text=element_text(size=9))


total1