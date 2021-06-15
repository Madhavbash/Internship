##Organizing the data
library(readxl)
library(ggplot2)
library(ggbeeswarm)

### Import data
file2 <- "Data/orangedata2.xlsx"
field_data <- read_excel(file2)

variables <- c("week4diff", "week7diff", "week9diff")
name.var <- c(
  "Weight loss in 4 weeks (g)",
  "Weight loss in 7 weeks (g)", 
  "Weight loss in 9 weeks (g)")

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
for (i in 1:3) {
  p <- ggplot(d, 
    aes_string(y = variables[i], x = "Application", color = "inpu")) +
    geom_boxplot() + 
    labs(color = "Puncture and inoculation",
       y = name.var[i],
       x = "Treatment") +
    theme_classic()
  print(p)
}


