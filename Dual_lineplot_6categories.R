# --------------------------------------------------
# NOTE: This script requires private datasets that are not included due to confidentiality. 
# Replace the paths with your own files structured similarly to the expected data frames.
# --------------------------------------------------

#Import libraries
library(ggplot2)
library(dplyr)
library(graphics)
library("ggpubr")
theme_set(theme_pubr())


#Both the top and bottom plots show six lines, each corresponding to one of the six value categories (Value1 to Value6), clearly distinguished by different colors
#The final figure shows two separate time series plots, one on top of the other, for easy comparison of trends across years


#Create data for graph 1 where each dataframe is one line
  a <- tibble::tibble(
    study = c(1.03, 1.78, 2.62, 3.44, 4.19, 5.07),
    Values = c("Value1", "Value1", "Value1", "Value1", "Value1", "Value1"),
    year = c(2015, 2016, 2017, 2018, 2019, 2020),
    variables = c(1, 2, 3, 4, 5, 6)
  )
  
  b <- tibble::tibble(
    study = c(1.12, 1.94, 2.81, 3.63, 4.52, 5.38),
    Values = c("Value2", "Value2", "Value2", "Value2", "Value2", "Value2"),
    year = c(2015, 2016, 2017, 2018, 2019, 2020),
    variables = c(7, 8, 9, 10, 11, 12)
  )
  
  c <- tibble::tibble(
    study = c(1.25, 2.08, 2.98, 3.91, 4.76, 5.72),
    Values = c("Value3", "Value3", "Value3", "Value3", "Value3", "Value3"),
    year = c(2015, 2016, 2017, 2018, 2019, 2020),
    variables = c(13, 14, 15, 16, 17, 18)
  )
  
  d <- tibble::tibble(
    study = c(1.31, 2.29, 3.18, 4.12, 5.08, 5.99),
    Values = c("Value4", "Value4", "Value4", "Value4", "Value4", "Value4"),
    year = c(2015, 2016, 2017, 2018, 2019, 2020),
    variables = c(19, 20, 21, 22, 23, 24)
  )
  
  e <- tibble::tibble(
    study = c(1.43, 2.37, 3.45, 4.36, 5.39, 6.00),
    Values = c("Value5", "Value5", "Value5", "Value5", "Value5", "Value5"),
    year = c(2015, 2016, 2017, 2018, 2019, 2020),
    variables = c(25, 26, 27, 28, 29, 30)
  )
  
  f <- tibble::tibble(
    study = c(1.54, 2.57, 3.62, 4.67, 5.71, 5.98),
    Values = c("Value6", "Value6", "Value6", "Value6", "Value6", "Value6"),
    year = c(2015, 2016, 2017, 2018, 2019, 2020),
    variables = c(31, 32, 33, 34, 35, 36)
  )
  

#Create data for graph 2 where each dataframe is one line
  first <- tibble::tibble(
    study = c(1.03, 3.45, 5.78, 7.12, 9.43, 11.99),
    Values = c("Value1", "Value1", "Value1", "Value1", "Value1", "Value1"),
    year = c(2015, 2016, 2017, 2018, 2019, 2020),
    variables = c(1, 2, 3, 4, 5, 6)
  )
  
  second <- tibble::tibble(
    study = c(1.15, 3.52, 6.01, 7.38, 9.61, 11.75),
    Values = c("Value2", "Value2", "Value2", "Value2", "Value2", "Value2"),
    year = c(2015, 2016, 2017, 2018, 2019, 2020),
    variables = c(7, 8, 9, 10, 11, 12)
  )
  
  third <- tibble::tibble(
    study = c(1.24, 3.61, 6.20, 7.59, 9.72, 11.88),
    Values = c("Value3", "Value3", "Value3", "Value3", "Value3", "Value3"),
    year = c(2015, 2016, 2017, 2018, 2019, 2020),
    variables = c(13, 14, 15, 16, 17, 18)
  )
  
  fourth <- tibble::tibble(
    study = c(1.36, 3.77, 6.45, 7.81, 9.93, 12.00),
    Values = c("Value4", "Value4", "Value4", "Value4", "Value4", "Value4"),
    year = c(2015, 2016, 2017, 2018, 2019, 2020),
    variables = c(19, 20, 21, 22, 23, 24)
  )
  
  fifth <- tibble::tibble(
    study = c(1.42, 3.89, 6.55, 8.04, 10.12, 12.00),
    Values = c("Value5", "Value5", "Value5", "Value5", "Value5", "Value5"),
    year = c(2015, 2016, 2017, 2018, 2019, 2020),
    variables = c(25, 26, 27, 28, 29, 30)
  )
  
  sixth <- tibble::tibble(
    study = c(1.53, 4.01, 6.74, 8.29, 10.34, 11.97),
    Values = c("Value6", "Value6", "Value6", "Value6", "Value6", "Value6"),
    year = c(2015, 2016, 2017, 2018, 2019, 2020),
    variables = c(31, 32, 33, 34, 35, 36)
  )
  

#Combine the datasets into one dataframe for each graph  
df <- bind_rows(a, b, c, d, e, f)
df1 <- bind_rows(first, second, third, fourth, fifth, sixth)

#Merge the dataframes of the two graphs
df2 <- data.frame(df, df1)

#Change the order of the variables for the graph 
df$Values <- factor(df$Values, levels=c('Value1', 'Value2', 'Value3', 'Value4', 'Value5', 'Value6'))
df2$Values.1 <- factor(df2$Values.1, levels=c('Value1', 'Value2', 'Value3', 'Value4', 'Value5', 'Value6'))

#Change the colors of the lines
cols <- c("#ae3c60","#df473c", "#f3c33c", "#255e79", "#267778", "#632340")

#Create the two plots
gold <- ggplot(df2, aes(x = year, y = study, color = Values, group = Values)) +
  geom_line(lwd = 1.5) +
  geom_point(size = 2.5) +
  scale_color_manual(values = cols) +
  labs(title="Title", x = "X-axis", y="Y-axis")+
  theme_minimal() + 
  theme(title = element_text(size = 15, face = "bold"),
        legend.title = element_text(size=12, face="bold"),
        legend.text = element_text(size =12),
        axis.title.x = element_text(size = 13, face="bold"),
        axis.title.y = element_text(size = 13, face="bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))+
  coord_cartesian(ylim = c(0, 6)) +
  scale_y_continuous(expand = c(0, 0))

silver <- ggplot(df2, aes(x = year.1, y = study.1, color = Values, group = Values)) +
  geom_line(lwd = 1.5) +
  geom_point(size = 2.5) +
  scale_color_manual(values = cols) +
  labs(title="Title", x = "X-axis", y="Y-axis")+
  theme_minimal() + 
  theme(title = element_text(size = 15, face = "bold"),
        legend.title = element_text(size=12, face="bold"),
        legend.text = element_text(size =12),
        axis.title.x = element_text(size = 13, face="bold"),
        axis.title.y = element_text(size = 13, face="bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))+
  coord_cartesian(ylim = c(0, 12.5)) +
  scale_y_continuous(expand = c(0, 0))

#Arrange the two plots into one plot, one on top of another
figure <- ggarrange(silver, gold,
                    ncol = 1, nrow = 2,
                    common.legend = TRUE, legend = "bottom")

#View the plot
figure
