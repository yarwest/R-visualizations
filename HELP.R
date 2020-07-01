setwd("~/Desktop/")
TOTAL_Cleaned <- read.csv("MY DATA/TOTAL_Cleaned.csv", sep=";")
attach(TOTAL_Cleaned)
names(TOTAL_Cleaned)

# libraries
install.packages("ggplot2")
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)


### Situation: National Economy Poland ###
ggerrorplot(subset(TOTAL_Cleaned, Country=="PL"), x="Year", y="Situation_Nat_Economy", desc_stat = "mean_sd",  
            error.plot = "errorbar", add="mean", ylab = "Situation: National Economy (Mean)", xlab = "Year") +
  ggtitle("Citizens' Perception of the National Economy in Poland") +
  theme_linedraw() +
  theme(
    text = element_text(size=16, color="Black"),
    axis.text.x = element_text(angle = 90),
    plot.title=element_text(hjust = 0.5, face="bold"),
    legend.position=c(0.1,0.85)
  ) 

t.test(Situation_Nat_Economy ~ Year, 
       subset = Country == "PL" & Year == 2008.2 | Year == 2015.1, 
       alternative = c("two.sided"), mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)



### Percent stacked bar chart - Poland ###

ggplot(subset(TOTAL_Cleaned, Country=="PL"), aes(x = factor(Year), 
       y = prop.table(stat(count)), 
       fill = factor(Situation_Nat_Economy), 
       label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "stack") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Year', y = 'Response Percentage', fill = 'Response') +
  theme_minimal() +
  theme(text = element_text(size=16, color="Black"),
    axis.text.x = element_text(angle = 90),
    plot.title=element_text(hjust = 0.5, face="bold")) 


