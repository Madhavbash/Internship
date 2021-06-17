###Fruit weight ANOVAs and Kruskal Wallis test
ibrary(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(zoo)
library(agricolae)
library(openxlsx)

#data
file2 <- "Data/orangedata2.xlsx"
field_data <- read_excel(file2)
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

##Testing for Differences betwn control and treatments in inoculated and 
##punctured fruits

PIdata <- subset(field_data, Punc_ino == "PI", 
                 select = c(Application, Tree, fruitN, week9diff, PercentLoss9 ))

PIaov <- aov(PercentLoss9  ~ Application, data = PIdata )
PIshap <- shapiro.test(PIaov$residuals)
summary(PIshap)

##p value is < 0.05. 
##Kruskal Wallis test

PIkrus <- kruskal.test(PercentLoss9  ~ Application, data = PIdata)

##p-value < 0.05 implies that there no significant differences in
##fruit weight loss between applications at week 9.
