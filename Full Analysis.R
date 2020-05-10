setwd("/Users/yarnoboelens/Code/school/R-crap/")
FULL_PL_HU <- read.csv("FULL_PL_HU.csv", sep=";")
attach(FULL_PL_HU)
names(FULL_PL_HU)

remove.packages(c("ggpubr", "rcompanion"))
install.packages('ggpubr', dependencies = TRUE)
install.packages('rcompanion', dependencies = TRUE)
library(ggpubr)
library(rcompanion)
library(plyr)

########################################################
###     Histogram: Check for Normal Distribution     ###
########################################################
mu <- ddply(df, "Country", summarise, grp.mean=mean(Satisfaction))
head(mu)

ggplot(FULL_PL_HU, aes(x=Satisfaction, color=Country)) +
  geom_density(adjust=1.5) +
  scale_color_manual(values=c("#f7bb3d", "#cd5038")) +
  ggtitle("Density Plot for Satisfaction with National Economy in Poland and Hungary") +
  theme_linedraw() +
  theme(text = element_text(size=16, color="Black"),
        plot.title=element_text(hjust = 0.5, face="bold"),
        legend.position=c(0.1,0.85)) 

########################################################
###     Poland Satisfaction - According to Vote     ###
########################################################
ggerrorplot(subset(FULL_PL_HU, Country=="PL"), x = "Year", y = "Satisfaction", 
            desc_stat = "median_mad", error.plot = "errorbar", add="median",
            color = "Vote", position = position_dodge(0.3), 
            ylab = "Satisfaction (Median)", xlab = "Year", size=0.7) + 
  scale_color_manual(values=c("#ebb9af", "#cd5038")) +
  expand_limits(y=c(0,10)) +
  scale_y_continuous(breaks=c(0:10), labels=c("Not satisfied at all", unlist(1:9), "Very satisfied")) +
  ggtitle("Satisfaction with National Economy in Poland") +
  theme_linedraw() +
  theme(text = element_text(size=16, color="Black"),
    plot.title=element_text(hjust = 0.5, face="bold"),
    legend.position=c(0.1,0.85)) +
  geom_vline(xintercept=4, linetype="dashed", color="darkgrey", size=0.5) +
  geom_vline(xintercept=7.5, linetype="dashed", color="darkgrey", size=0.5) 

########################################################
###   Poland Satisfaction - Comparison 2008 & 2014   ###
########################################################
ggerrorplot(subset(FULL_PL_HU, Country=="PL" & Year == "2008" | Country=="PL" & Year == "2014"), x="Year", y="Satisfaction", 
            desc_stat = "median_mad", error.plot = "errorbar", add="median", 
            ylab = "Satisfaction (Median)", xlab = "Year") +
  expand_limits(y=c(0,10)) +
  scale_y_continuous(breaks=c(0:10), labels=c("Not satisfied at all", unlist(1:9), "Very satisfied")) +
  ggtitle("Satisfaction with National Economy in Poland") +
  theme_linedraw() +
  theme(text = element_text(size=16, color="Black"),
    plot.title=element_text(hjust = 0.5, face="bold")) 

###########################################################
### Poland Satisfaction 2008 & 2014 - Significance Test ###
###########################################################
PL_Test<-subset(FULL_PL_HU, Country=="PL" & Year == "2008" | Country=="PL" & Year == "2014")
wilcox.test(PL_Test$Year, 
            PL_Test$Satisfaction, alternative = c("greater"), 
            paired = FALSE, exact = NULL, correct = TRUE, 
            conf.int = TRUE, conf.level = 0.95)

wilcoxonR(x = PL_Test$Satisfaction,
          g = PL_Test$Year)

###########################################################
###       Hungary Satisfaction - According to Vote     ###
###########################################################
ggerrorplot(subset(FULL_PL_HU, Country=="HU"), x = "Year", y = "Satisfaction", 
            desc_stat = "median_mad", error.plot = "errorbar", add="median",
            color = "Vote", position = position_dodge(0.3), 
            ylab = "Satisfaction (Median)", xlab = "Year", size=0.7) +  
  scale_color_manual(values=c("#dc9809", "#fad485")) +
  expand_limits(y=c(0,10)) +
  scale_y_continuous(breaks=c(0:10), labels=c("Not satisfied at all", unlist(1:9), "Very satisfied")) +
  ggtitle("Satisfaction with National Economy in Hungary") +
  theme_linedraw() +
  theme(text = element_text(size=16, color="Black"),
        plot.title=element_text(hjust = 0.5, face="bold"),
        legend.position=c(0.1,0.85)) +
  geom_vline(xintercept=4, linetype="dashed", color="darkgrey", size=0.5) +
  geom_vline(xintercept=5, linetype="dashed", color="darkgrey", size=0.5) 

###########################################################
###   Hungary Satisfaction - Comparison 2008 & 2010     ###
###########################################################
ggerrorplot(subset(FULL_PL_HU, Country=="HU" & Year == "2008" | Country=="HU" & Year == "2010"), x="Year", y="Satisfaction", 
            desc_stat = "median_mad", error.plot = "errorbar", add="median", 
            ylab = "Satisfaction (Median)", xlab = "Year") +
  expand_limits(y=c(0,10)) +
  scale_y_continuous(breaks=c(0:10), labels=c("Not satisfied at all", unlist(1:9), "Very satisfied")) +
  ggtitle("Satisfaction with National Economy in Hungary") +
  theme_linedraw() +
  theme(text = element_text(size=16, color="Black"),
        plot.title=element_text(hjust = 0.5, face="bold")) 

###########################################################
### Hungary Satisfaction 2008 & 2010 - Significance Test ##
###########################################################
HU_Test<-subset(FULL_PL_HU, Country=="HU" & Year == "2008" | Country=="HU" & Year == "2010")

wilcox.test(HU_Test$Year, HU_Test$Satisfaction, alternative = c("greater"), 
            paired = FALSE, exact = NULL, correct = TRUE, 
            conf.int = TRUE, conf.level = 0.95)

wilcoxonR(x = HU_Test$Satisfaction,
          g = HU_Test$Year)

