### Multiple regression analysis
library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(zoo)
library(agricolae)
library(openxlsx)
##data
file2 <- "Data/orangedata2.xlsx"
field_data <- read_excel(file2)
str(field_data)
field_data <- data.frame(field_data)
##Ordering treatments
field_data$Application <- factor(field_data$Application, 
                          levels = c("Control", "2SiO2", "5SiO2", 
                                            "10SiO2", "Mancozeb"))
## week 4
model1A <- lm(field_data$week4diff ~ field_data$Application +
                field_data$Puncture + field_data$Inoculation)
summary(model1A)

model1B <- lm(field_data$PercentLoss4 ~ field_data$Application +
                field_data$Puncture + field_data$Inoculation)
summary(model1B)

model_interaction1 <- lm(field_data$PercentLoss4 ~ field_data$Application 
                         + field_data$Inoculation + field_data$Puncture 
                         + I(field_data$Puncture * field_data$Inoculation))
summary(model_interaction1)
##From the t. values of the above models, we can conclude that inoculation 
## has the most significant impact on disease occurence.

AIC(model1A, model1B, model_interaction1)
#from the AIC values, model1B and model_interaction1 are better models.

## week 7
model2A <- lm(field_data$week7diff ~ field_data$Application +
                field_data$Puncture + field_data$Inoculation)
summary(model2A)


model2B <- lm(field_data$PercentLoss7 ~ field_data$Application +
                field_data$Inoculation + field_data$Puncture)
summary(model2B)

#from the t-values, inoculation followed by puncture has the highest
#impact on fruit weight.
model_interaction2 <- lm(field_data$PercentLoss7 ~ field_data$Application +
                           field_data$Inoculation + field_data$Puncture + I(field_data$Puncture * field_data$Inoculation))
summary(model_interaction2)
#when we consider the interaction btwn inoculation and puncture,
#the t value for inoculation remains the highest followed by the
#t value for the interaction. 
#the combined impact of puncture and inoculation is higher than
#puncture alone

AIC(model2A, model2B, model_interaction2)
#model_interaction2 is the best model

##week 9
model3A <- lm(field_data$week9diff ~ field_data$Application +
                field_data$Puncture + field_data$Inoculation)
summary(model3A)

model3B <- lm(field_data$PercentLoss9 ~ field_data$Application +
                field_data$Inoculation + field_data$Puncture)
summary(model3B)

#from the t-values, inoculation followed by pumcture has the highest
#impact on fruit weight.

model_interaction3 <- lm(field_data$PercentLoss9 ~ field_data$Application 
                         + field_data$Inoculation + field_data$Puncture 
                         + I(field_data$Puncture * field_data$Inoculation))
summary(model_interaction3)

#the t value for the interaction between puncture and inoculation 
#is the highest. Compared to previous models, it drops for both the 
#individual variables- puncture and inoculation

AIC(model3A, model3B, model_interaction3)

#model interaction 3 is the best model.

