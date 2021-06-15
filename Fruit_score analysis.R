### fruit score analysis
library(readxl)
library(ggplot2)
library(ggbeeswarm)
library(MASS)
library(agricolae)
library(ggpubr)
library(ggthemes)
file3 <- "Data/Symptoms_fruits.xlsx"
scores <- read_excel(file3)
str(scores)
scores <- data.frame(scores)
scores$symptom4week <- as.factor(scores$symptom4week)
scores$symptom7week <- as.factor(scores$symptom7week)
scores$symptom9week <- as.factor(scores$symptom9week)

### Order treatments
scores$treatment <- factor(scores$treatment, 
                           levels = c("control", "2SiO2", "5SiO2", "10SiO2", "mancozeb"))


##plots

bar_graph <- ggplot(scores, aes(x = treatment, fill = symptom9week))+
   geom_bar(position = "stack")+
   facet_grid(inoculation ~ puncture, margins = FALSE)

bar_graph+theme_bw()+ ggtitle("Visual fruit scores") 
bar_graph+theme_wsj()+ scale_color_wsj(palette = "colors6")+
scale_fill_wsj(palette = "colors6")+ ggtitle("Visual fruit scores")
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

#new_data <- data.frame("treatment" = "10SiO2", "puncture" = "P", 
#                       "inoculation" = "NI")
#round(predict(m1,new_data,type = "p"), 3)
#new_data1 <- data.frame("treatment" = "10SiO2", "puncture" = "NP", 
#                       "inoculation" = "I")
#round(predict(m1,new_data1,type = "p"), 3)
#new_data2 <- data.frame("treatment" = "control", "puncture" = "P", 
#                        "inoculation" = "NI")
#round(predict(m1,new_data2,type = "p"), 3)
#new_data3 <- data.frame("treatment" = "10SiO2", "puncture" = "P", 
#                        "inoculation" = "I")
#round(predict(m1,new_data3,type = "p"), 3)

lapply(dir("R", full.names = TRUE, pattern = ".R"), source)

scores$symptom9week <- # as numeric
  as.numeric(as.character(scores$symptom9week))
### symptoms or not
scores$symptoms.9.presence <- scores$symptom9week > 0

###1. Non punctured - non inoculated --------------------
scores.NPNI <- scores[scores$puncture == "NP" & scores$inoculation == "NI", ]

#### Fisher test. 
df0 <- scores.NPNI
df0$symptoms.9.presence <- as.factor(df0$symptoms.9.presence)
t1 <- table(df0$treatment, df0$symptoms.9.presence)
t1
ch <- fisher.test(t1)
ch # significant diferences between treatments p-value = 0.001434

ph <- agcatfisherposthoc(t1, control = "holm", digits = 2)
ph 
### there is some significant raw differences between 
### control and 5SI, 10SI and mancozeb
### but lost when correcting for multiple comparison.





###2. Punctured - Inoculated --------------------
df0 <- scores[scores$puncture == "P" & scores$inoculation == "I", ]

#### Fisher test. 
df0$symptoms.9.presence <- as.factor(df0$symptoms.9.presence)
t1 <- table(df0$treatment, df0$symptoms.9.presence)
t1
ch <- fisher.test(t1)
ch # significant differences between treatments p-value = 1.012e-06

ph <- agcatfisherposthoc(t1, control = "holm", digits = 2)
ph 
### There are differences between control and all treatments.
### There are not differences between treatments.




###3. Punctured - not Inoculated --------------------
df0 <- scores[scores$puncture == "P" & scores$inoculation == "NI", ]

#### Fisher test. 
df0$symptoms.9.presence <- as.factor(df0$symptoms.9.presence)
t1 <- table(df0$treatment, df0$symptoms.9.presence)
t1
ch <- fisher.test(t1)
ch # significant differences between treatments p-value = 0.009118

ph <- agcatfisherposthoc(t1, control = "holm", digits = 2)
ph 
### No significant differences once adjusted



###4. not Punctured - Inoculated --------------------
df0 <- scores[scores$puncture == "NP" & scores$inoculation == "I", ]

#### Fisher test. 
df0$symptoms.9.presence <- as.factor(df0$symptoms.9.presence)
t1 <- table(df0$treatment, df0$symptoms.9.presence)
t1
ch <- fisher.test(t1)
ch # significant differences between treatments p-value = 4.304e-05

ph <- agcatfisherposthoc(t1, control = "holm", digits = 2)
ph 
### No significant differences once adjusted. 
### Some raw differences between control and any treatment. 
