# --------------------------------------------------
# NOTE: This script requires private datasets that are not included due to confidentiality. 
# Replace the paths with your own files structured similarly to the expected data frames.
# --------------------------------------------------

#Import libraries
library(readxl)
library(dplyr)


#Visualizes values across groups with 95% confidence intervals using error bars
#Bars are color-coded by group, with sample sizes labeled on the bottom of each bar


#Import data
data <- read_excel("data.xls")

#Create the confidence intervals for the error bars
data2 <- data1 %>%
  group_by(age_group) %>% 
  summarise(
    count = n(),
    e2d2 = mean(e2_d2, na.rm = TRUE),
    sd = sd(e2_d2),
    n = n(),
    se = sd / sqrt(n),
    ci = qt(0.975, df = n - 1) * se 
  )


#Plot the data
#Creates a barplot with error bars
ggplot(data2, aes(age_group, 
                  e2d2,fill = age_group)) +                       #Basic bar plot
  geom_bar(stat="identity",           
           color="black",                                         #Bar color
           position=position_dodge()) +
  geom_errorbar(aes(ymin=e2d2 - ci, ymax=e2d2 + ci), width=.2,    #Error bars
                position=position_dodge(.9)) +                    #Location of error bars
  labs(                                                           #Add titles for y-axis, x-axis, and legend
    x = "",
    y = "",
    fill = ""
  ) +
  scale_fill_manual(                                              #Bar colors
    values = c(
      "18 - 34" = "#3E1F47",
      "35 - 37" = "#2A3B66",
      "38 - 40" = "#1F4E3D",
      "41 - 42" = "#224C54",
      "43 - 99" = "#3A3A3C"
    )
    ) +
  geom_text(aes(label = count, y = 3), vjust = -0.1, color = "white", size = 5) +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

