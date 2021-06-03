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
 
##Ordinal logit model
m1 <- polr(symptom7week ~ treatment + puncture + inoculation, data = scores)
summary(m1)
##getting the p values
m1_coef <- data.frame(coef(summary(m1)))
m1_coef$pValue <- pnorm(abs(m1_coef$t.value), lower.tail = FALSE) * 2
m1_coef

## p-Value is < 0.05 for control, puncture, and inoculation. 
##So, these variables are significantly associated with visual symptoms.

##Predicting

new_data <- data.frame("treatment" = "10SiO2", "puncture" = "P", 
                       "inoculation" = "NI")
round(predict(m1,new_data,type = "p"), 3)
new_data1 <- data.frame("treatment" = "10SiO2", "puncture" = "NP", 
                       "inoculation" = "I")
round(predict(m1,new_data1,type = "p"), 3)
new_data2 <- data.frame("treatment" = "control", "puncture" = "P", 
                        "inoculation" = "NI")
round(predict(m1,new_data2,type = "p"), 3)
new_data3 <- data.frame("treatment" = "10SiO2", "puncture" = "P", 
                        "inoculation" = "I")
round(predict(m1,new_data3,type = "p"), 3)

