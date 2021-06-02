### fruit score analysis
library(readxl)
library(MASS)
file3 <- "Data/Symptoms_fruits.xlsx"
scores <- read_excel(file3)
str(scores)
scores <- data.frame(scores)

##Ordinal logit model
