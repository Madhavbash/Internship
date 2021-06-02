### fruit score analysis
library(readxl)
library(ggplot2)
library(ggbeeswarm)
library(MASS)
file3 <- "Data/Symptoms_fruits.xlsx"
scores <- read_excel(file3)
str(scores)
scores <- data.frame(scores)
scores$symptom4week <- as.factor(scores$symptom4week)
scores$symptom7week <- as.factor(scores$symptom7week)
scores$symptom9week <- as.factor(scores$symptom9week)

##plots
 ggplot(scores, aes(x = treatment, fill = symptom9week))+
   geom_bar(position = "stack")+
   facet_grid(inoculation ~ puncture, margins = FALSE)
#NOTE: inoculation iss vertically along the grid and puncture is horizontally.

##Ordinal logit model
m1 <- polr(symptom7week ~ treatment + puncture + inoculation, data = scores)
summary(m1)
