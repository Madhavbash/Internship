##Organizing the data
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
file2 <- "Data/orangedata2.xlsx"
field_data <- read_excel(file2)

##Visualizing the data

##Beeswarm plots
##week 4
# ggplot(field_data, aes(x = Application, y = week4diff, 
#                        color = as.factor(Inoculation), 
#                        shape = as.factor(Puncture))) +
#   geom_quasirandom(alpha = 0.7,
#                    size = 1.5)

ggplot(field_data, aes(x = Application, y = week4diff, 
                       color = as.factor(Inoculation), 
                       shape = as.factor(Puncture))) +
  scale_shape_manual(values = c(1, 16)) +  # Set personalized shapes. 
  geom_quasirandom(alpha = 0.7,
                   size = 1.5)

ggplot(field_data, aes(x = Application, y = week4diff, 
                       color = as.factor(Inoculation), 
                       fill = as.factor(Puncture))) +
  geom_boxplot() +
  theme_bw()

d <- field_data
d$inpu <- NA
d$inpu[d$Puncture == 1 & d$Inoculation == 1] <- "Inoculated and punctured"
d$inpu[d$Puncture == 0 & d$Inoculation == 1] <- "Inoculated but not punctured"
d$inpu[d$Puncture == 1 & d$Inoculation == 0] <- "Not inoculated but punctured"
d$inpu[d$Puncture == 0 & d$Inoculation == 0] <- "Neither inoculated or punctured"
d$inpu <- factor(d$inpu, levels = c("Inoculated and punctured",
                                "Inoculated but not punctured",
                                "Not inoculated but punctured",
                                "Neither inoculated or punctured"))
ggplot(d, aes(x = Application, y = week4diff, color = inpu)) +
  geom_boxplot() + 
  labs(color = "Puncture and inoculation",
       y = "Weight loss in week 4 (g)",
       x = "Treatment") +
  theme_classic()

# ggplot(field_data, aes(x = Application, y = PercentLoss4, 
#                        color = as.factor(Inoculation), 
#                        shape = as.factor(Puncture)))+
#   geom_quasirandom(alpha = 0.7,
#                    size = 1.5)

ggplot(field_data, aes(x = Application, y = PercentLoss4, 
                       color = as.factor(Inoculation), 
                       shape = as.factor(Puncture))) +
  scale_shape_manual(values = c(1, 16)) +  # Set personalized shapes. 
  geom_quasirandom(alpha = 0.7,
                   size = 1.5)

##week 7
ggplot(field_data, aes(x = Application, y = week7diff, color = as.factor(Inoculation), shape = as.factor(Puncture)))+
  geom_quasirandom(alpha = 0.7,
                   size = 1.5)
ggplot(field_data, aes(x = Application, y = PercentLoss7, color = as.factor(Inoculation), shape = as.factor(Puncture)))+
  geom_quasirandom(alpha = 0.7,
                   size = 1.5) 
  

##Multifactorial Anova
#week 4
orange_aov1A <- aov(week4diff ~ Application * Puncture * Inoculation,
                   data = field_data)
shapiro1A <- shapiro.test(orange_aov1A$residuals)

## p value < 0.05

orange_aov1B <- aov(PercentLoss4 ~ Application * Puncture * Inoculation,
                    data = field_data)
shapiro1B <- shapiro.test(orange_aov1B$residuals)

## p value less than 0.05

##week 7
orange_aov2A <- aov(week7diff ~ Application * Puncture * Inoculation,
                   data = field_data)
shapiro2A <- shapiro.test(orange_aov2A$residuals)

## p value less than 0.05

orange_aov2B <- aov(PercentLoss7 ~ Application * Puncture * Inoculation,
                    data = field_data)
shapiro2B <- shapiro.test(orange_aov2B$residuals)

## p value less than 0.05

###Multiple regression analysis
model1 <- lm(field_data$week4diff ~ field_data$Application +
             field_data$Puncture + field_data$Inoculation)
summary(model1)

model1B <- lm(field_data$PercentLoss4 ~ field_data$Application +
                field_data$Puncture + field_data$Inoculation)
summary(model1B)
model2A <- lm(field_data$week7diff ~ field_data$Application +
               field_data$Puncture + field_data$Inoculation)
summary(model2A)

model2A <- lm(field_data$week7diff ~ as.numeric(as.factor(field_data$Application)) +
                field_data$Puncture + field_data$Inoculation)
summary(model2A)
model_interaction1 <- lm(field_data$PercentLoss7 ~ field_data$Application +
                field_data$Inoculation + field_data$Puncture + I(field_data$Puncture * field_data$Inoculation))
summary(model2A)

model_int <- lm(field_data$PercentLoss7 ~ field_data$Application +
                           field_data$Inoculation + field_data$Puncture)
summary(model_int)

AIC(model_interaction1, model_int)
anova(model_interaction1, model_int)
##From the t. values of the above models, we can conclude that inoculation 
## has the most significant impact on disease occurence.
