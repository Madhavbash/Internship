###fruit weight plots
library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(zoo)
library(agricolae)
library(openxlsx)
library(ggplot2)
library(ggbeeswarm)
library(scales)
library(reshape2)
library(ggpubr)

## importing data

file2 <- "Data/orangedata2.xlsx"
field_data <- read_excel(file2)

field_data <- data.frame(field_data)
##Ordering treatments
field_data$Application <- factor(field_data$Application, 
                          levels = c("Control", "2SiO2", "5SiO2", 
                                      "10SiO2", "Mancozeb"))
##Visualizing the data

##Beeswarm plots
##week 4
#weight difference
ggplot(field_data, aes(x = Application, y = week4diff, 
                       color = as.factor(Inoculation), 
                       shape = as.factor(Puncture))) +
  scale_shape_manual(values = c(1, 16)) + 
  geom_quasirandom(alpha = 0.7,
                   size = 1.5)
#percent
ggplot(field_data, aes(x = Application, y = PercentLoss4, 
                       color = as.factor(Inoculation), 
                       shape = as.factor(Puncture))) +
  scale_shape_manual(values = c(1, 16)) +  # Set personalized shapes. 
  geom_quasirandom(alpha = 0.7,
                   size = 1.5)

##week 7
#weight
ggplot(field_data, aes(x = Application, y = week7diff, color = as.factor(Inoculation), 
                       shape = as.factor(Puncture)))+
scale_shape_manual(values = c(1, 16)) +
  geom_quasirandom(alpha = 0.7,
                   size = 1.5) 
#percent  
ggplot(field_data, aes(x = Application, y = PercentLoss7, color = as.factor(Inoculation), shape = as.factor(Puncture)))+
  scale_shape_manual(values = c(1, 16)) +
  geom_quasirandom(alpha = 0.7,
                   size = 1.5) 

##week 9
#weight
ggplot(field_data, aes(x = Application, y = week7diff, color = as.factor(Inoculation), 
                       shape = as.factor(Puncture)))+
  scale_shape_manual(values = c(1, 16)) +
  geom_quasirandom(alpha = 0.7,
                   size = 1.5) 
#percent
field_data$Inoculation <- as.factor(field_data$Inoculation)
field_data$Puncture <- as.factor(field_data$Puncture)
beeplot <- ggplot(field_data, aes(x = Application, y = PercentLoss9, 
                       color = Inoculation, 
                       shape = Puncture)) +
  scale_shape_manual(values = c(16, 1)) + 
  geom_quasirandom(alpha = 0.7,
                   size = 1.5)
beeplot
beeplot + scale_color_wsj(palette = "colors6")+
  scale_fill_wsj(palette = "colors6")+ ggtitle("Visual fruit scores")

beeplot+theme_wsj()+ scale_color_wsj(palette = "colors6")+
  scale_fill_wsj(palette = "colors6")+ ggtitle("Visual fruit scores")

##boxplot
ggplot(field_data, 
       aes_string(x = Application, y = PercentLoss9, color = Punc_ino)) +
  geom_boxplot() + 
  labs(color = "Puncture and inoculation",
       y = "Weight Loss %",
       x = "Treatment") +  theme_calc()+ scale_colour_calc()+
  scale_fill_wsj(palette = "colors6")+ ggtitle("Visual fruit scores")
  
