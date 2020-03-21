
######################
# Plotting functions
# YOU CAN IGNORE THIS PART
######################

install.packages("ggplot2")
library(ggplot2)

colors <- c("#cd5038","#f7bb3d")
countries <- c("Poland", "Hungary")

# Labels, the labels for the individual bars (vector)
# Grouplabels, the labels for each group of bars (vector)
# Data, the data for all bars (vector)
# y, min and max value for y axis (vector)
barPlot <- function(labels, groupLabels, yLabels, data, y) {
  nQuestions <- length(groupLabels)
  
  # Data points per country per question
  nDataPCPQ <- (length(data)/length(countries))/nQuestions
  
  df <- data.frame(
    x=labels,
    y=data,
    Countries=
      rep(
        c(
          rep(countries[1], nDataPCPQ),
          rep(countries[2], nDataPCPQ)
        ), nQuestions)
  )
  
  df$Countries <- factor(df$Countries, levels=unique(df$Countries))

  groupStarts <- seq(1, 1 + (7 * ((length(data)/4)-1)), by=7)
  xSeq <- unlist(lapply(groupStarts, FUN=function(x){c(unlist(seq(from=x, to=x+1, by=1)), unlist(seq(from=x+3, to=x+4, by=1)))}))
  iGroups <- 1:length(groupStarts)
  xGroups <- unlist(lapply(iGroups, FUN=function(x){mean(xSeq[(1 + (4*(x-1))):(4 + (4*(x-1)))])}))
  
  ggplot(data=transform(df, x=xSeq), aes(x=x, y=y, fill=Countries)) +
    geom_histogram(stat = "identity") + labs(x="", y="") +
    scale_x_discrete(breaks = NULL) +
    geom_text(aes(x=xSeq, y=df$y, label=df$y), vjust=-1) +
    geom_text(aes(x=xSeq, y=1, label=labels), vjust=2) +
    scale_x_continuous(breaks=xGroups, labels=groupLabels) +
    scale_y_continuous(breaks=c(y[1]:y[2]), labels=yLabels) +
    coord_cartesian(ylim = y) +
    theme_light() +
    theme(
      text = element_text(size=17, color="#4C4D4D"),
      plot.title=element_text(hjust = 0.5),
      legend.position=c(0.9,0.8)
    ) +
    scale_fill_manual(values=colors)
}

## question 1
labels <- rep(c(2008, 2016), 2)
questions <- c("Of every 100 working age how many unemployed and looking for work?")
possibleAnswers <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50 or more")

pl <- c(3.67, 3.07)
hu <- c(6.31, 3.93)
data <- c(pl, hu)

barPlot(labels, questions, possibleAnswers, data, y=c(1,11))

## question 2 & 3
labels <- rep(c(2008, 2016), 4)
questions <- c("How likely unemployed and looking for work next 12 months?", "How likely not enough money for household \n necessities next 12 months?")
possibleAnswers <- c("Not likely at all", "Not very likely", "Likely", "Very likely")

data <- c(2.25, 2.33, 1.93, 1.58, 1.38, 1.07, 1.36, 0.994)

barPlot(labels, questions, possibleAnswers, data, y=c(1,4))