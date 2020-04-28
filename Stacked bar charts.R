setwd("/Users/yarnoboelens/Code/school/R-crap/")
TOTAL_Cleaned <- read.csv("TOTAL_Cleaned.csv", sep=";")
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

answers = subset(TOTAL_Cleaned[order(TOTAL_Cleaned$Situation_Nat_Economy),], Country=="PL" & !is.na(Situation_Nat_Economy))
percentile = quantile(answers$Situation_Nat_Economy, probs = seq(0, 1, by=1/length(answers$Situation_Nat_Economy)))
percentageLabels = paste(seq(0, 1, by=0.05) * 100, "%", sep="")
answerLabels = c("Very good", "Rather good", "Rather bad", "Very bad")

ggplot(answers, aes(fill=Situation_Nat_Economy, y=tail(percentile,-1), x=Year)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_y_continuous(breaks=seq(0, 1, by= 0.05), labels = percentageLabels) +
  scale_x_continuous(breaks=unique(Year), labels=unique(Year)) +
  scale_fill_gradient(name="Response", breaks=1:4, labels=answerLabels)+
  labs(x = 'Year', y='Response Percentage') +
  theme_minimal() +
  theme(text = element_text(size=16, color="Black"),
        axis.text.x = element_text(angle = 90),
        plot.title=element_text(hjust = 0.5, face="bold"))
