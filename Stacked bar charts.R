setwd("/Users/yarnoboelens/Code/school/R-crap/")
TOTAL_Cleaned <- read.csv("TOTAL_Cleaned3.csv", sep=";")
attach(TOTAL_Cleaned)
names(TOTAL_Cleaned)

# libraries
remove.packages(c("ggplot2", "ggpubr"))
install.packages('ggplot2', dependencies = TRUE)
install.packages('ggpubr', dependencies = TRUE)
library(ggplot2)
library(ggpubr)

### Situation: National Economy Poland ###
polandData <- subset(TOTAL_Cleaned, Country=="PL")
ggerrorplot(polandData, x="Year", y="Situation_Nat_Economy", desc_stat = "mean_sd",  
            error.plot = "errorbar", add="mean", ylab = "Situation: National Economy (Mean)", xlab = "Year") +
  ggtitle("Citizens' Perception of the National Economy in Poland") +
  theme_linedraw() +
  theme(
    text = element_text(size=16, color="Black"),
    axis.text.x = element_text(angle = 90),
    plot.title=element_text(hjust = 0.5, face="bold"),
    legend.position=c(0.1,0.85)
  ) 

tTestData <- subset(TOTAL_Cleaned, Country == "PL" & Year == 2008.2 | Year == 2015.1)
t.test(tTestData$Year, tTestData$Situation_Nat_Economy,
       alternative = c("two.sided"), mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)

### Percent stacked bar chart - Poland ###

answers = subset(TOTAL_Cleaned[order(TOTAL_Cleaned$Situation_Nat_Economy),], Country=="PL" & !is.na(Situation_Nat_Economy) & Situation_Nat_Economy != 5 & Situation_Nat_Economy != 0)
answers$Situation_Nat_Economy_String = unlist(lapply(answers$Situation_Nat_Economy, toString))
percentile = quantile(answers$Situation_Nat_Economy, probs = seq(0, 1, by=1/length(answers$Situation_Nat_Economy)))
percentageLabels = paste(seq(0, 1, by=0.05) * 100, "%", sep="")
answerLabels = c("Very good", "Rather good", "Rather bad", "Very bad")
years = unique(TOTAL_Cleaned$Year)

ggplot(answers, aes(fill=Situation_Nat_Economy_String, group=Year, y=tail(percentile,-1), x=Year)) + 
  geom_col(position="fill", width=.4) +
  scale_x_discrete(breaks=years, labels=years) +
  scale_y_continuous(breaks=seq(0, 1, by= 0.05), labels = percentageLabels) +
  scale_fill_manual(values=c('1'="#999999", '2'="#E69F00", '3'="#123123","4"="#000000"), name="Response", breaks=c('1','2','3','4'), labels=answerLabels) +
  labs(x = 'Year', y='Response Percentage') +
  theme_minimal() +
  theme(text = element_text(size=16, color="Black"),
        axis.text.x = element_text(angle = 90),
        plot.title=element_text(hjust = 0.5, face="bold")) +
  coord_cartesian(ylim = c(0,1))

## issues_economic && issues_unemployment

polandData <- subset(TOTAL_Cleaned, Country=="PL")
years = unique(TOTAL_Cleaned$Year)

issuesEconomicPercentages <- lapply(years, function(x) {
  roundData = subset(polandData, Year==x)
  issuesEconomicTable = table(roundData$Issues_Economic)
  return ((issuesEconomicTable[2]/(issuesEconomicTable[1] + issuesEconomicTable[2])) * 100)
})

issuesUnemploymentPercentages <- lapply(years, function(x) {
  roundData = subset(polandData, Year==x)
  issuesUnemploymentTable = table(roundData$Issues_Unemployment)
  return ((issuesUnemploymentTable[2]/(issuesUnemploymentTable[1] + issuesUnemploymentTable[2])) * 100)
})

nEconomicDatapoints <- length(issuesEconomicPercentages)
nUnemploymentDatapoints <- length(issuesUnemploymentPercentages)

df <- data.frame(
  x=rep(years, 2),
  y=c(unlist(issuesEconomicPercentages), unlist(issuesUnemploymentPercentages)),
  issue=c(rep("economic issues", nEconomicDatapoints), rep("unemployment issues", nUnemploymentDatapoints))
)

colSeq <- c(rep("red", nEconomicDatapoints), rep("yellow", nUnemploymentDatapoints))

ggplot(df, aes(x=x, y=y, group=issue, color=colSeq)) +
   geom_line(size=1) +
   geom_point(size=2) +
   ggtitle("Percentage of people") +
   scale_y_continuous(breaks=seq(0, 100, by= 5), labels = percentageLabels) +
   labs(x="Voting round", y="Percentage yes answers") +
   theme_linedraw() +
   theme(
       text = element_text(size=16, color="Black"),
       plot.title=element_text(hjust = 0.5, face="bold"),
       axis.text.x=element_text(angle=90),
       legend.position=c(0.2,0.9)
   ) +
   coord_cartesian(ylim = c(0,100)) +
   scale_colour_manual(name="Issues",
                      breaks=colSeq,
                      values=colSeq,
                      labels=df$issue)
