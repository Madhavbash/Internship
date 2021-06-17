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
ggplot(field_data, aes(x = Application, y = PercentLoss9, 
                       color = as.factor(Inoculation), 
                       shape = as.factor(Puncture))) +
  scale_shape_manual(values = c(16, 1)) + 
  geom_quasirandom(alpha = 0.7,
                   size = 1.5)


