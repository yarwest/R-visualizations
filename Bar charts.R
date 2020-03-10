######################
# Plotting functions
# YOU CAN IGNORE THIS PART
######################

install.packages("ggplot2")
library(ggplot2)

barPlot <- function(labels, groupLabels, data) {
  colors <- c("#f7bb3d","#cd5038")
  countries <- c("Hungary", "Poland")
  
  df <- data.frame(
    x=labels,
    y=data
  )

  groupStarts <- seq(1, 1 + (7 * ((length(data)/4)-1)), by=7)
  xSeq <- unlist(lapply(groupStarts, FUN=function(x){c(unlist(seq(from=x, to=x+1, by=1)), unlist(seq(from=x+3, to=x+4, by=1)))}))
  iGroups <- 1:length(groupStarts)
  xGroups <- unlist(lapply(iGroups, FUN=function(x){mean(xSeq[(1 + (4*(x-1))):(4 + (4*(x-1)))])}))
  colorSeq <- rep(c(colors[1],colors[1],colors[2],colors[2]), length(data)/4)
  
  ggplot(data=transform(df, x=xSeq), aes(x=x, y=y, fill=colorSeq)) +
    geom_histogram(stat = "identity") + labs(x="", y="") +
    scale_x_discrete(breaks = NULL) +
    geom_text(aes(x=xSeq, y=df$y, label=df$y), vjust=-1) +
    geom_text(aes(x=xSeq, y=0, label=labels), vjust=2) +
    scale_x_continuous(breaks=xGroups, labels=groupLabels) +
    theme(text = element_text(size=15)) +
    scale_fill_discrete(name="Countries",
                        breaks=colors,
                        labels=countries)
}

## question 1

labels <- rep(c(2008, 2016), 2)
questions <- c("Of every 100 working age how many unemployed and looking for work?")

data <- c(6.31, 3.93, 3.67, 3.07)

barPlot(labels, questions, data)

## question 2 & 3

labels <- rep(c(2008, 2016), 4)
questions <- c("How likely unemployed and looking for work next 12 months?", "How likely not enough money for household necessities next 12 months?")

data <- c(1.93, 1.58, 2.25, 2.33, 1.36, 0.994, 1.38, 1.07)

barPlot(labels, questions, data)