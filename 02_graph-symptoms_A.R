library(readxl)
library(ggplot2)
library(ggbeeswarm)
library(MASS)
library(agricolae)
library(ggpubr)
library(ggthemes)
file3 <- "Data/Symptoms_fruits.xlsx"
scores <- read_excel(file3)
# str(scores)
scores <- data.frame(scores)
# scores$symptom4week <- as.factor(scores$symptom4week)
# scores$symptom7week <- as.factor(scores$symptom7week)
scores$symptom9week <- as.factor(scores$symptom9week)

### Order treatments
scores$treatment <- factor(scores$treatment, 
                           levels = c("control", "2SiO2", "5SiO2", "10SiO2", "mancozeb"))

##plots
D <- data.frame(x = rep(levels(scores$treatment), 4),
  y = 35, 
  puncture = c(rep("NP", 10), rep("P", 10)),
  inoculation = c(rep("I", 5), rep("NI", 5), 
    rep("I", 5), rep("NI", 5)),
  lab = c(rep("a", 10), "b", rep("a", 9)), 
  symptom9week = NA)
      
Plot <- ggplot(scores, aes(x = treatment, fill = symptom9week)) +
   geom_bar(position = "stack") +
   facet_grid(inoculation ~ puncture, margins = FALSE) +
  geom_text(data = D, position = "identity", 
    aes(y = y, x = x, label = lab)) 

Plot +theme_wsj()+ scale_color_wsj(palette = "colors6")+
  scale_fill_wsj(palette = "colors6")+ ggtitle("Visual fruit scores")
Plot
Plot + theme_calc()+ scale_colour_calc()+
  scale_fill_wsj(palette = "colors6")+ ggtitle("Visual fruit scores")
