######################
# Plotting functions
# YOU CAN IGNORE THIS PART
######################

install.packages("ggplot2")
library(ggplot2)

quarters <- c('Q1','Q2','Q3','Q4')
countries <- c("Hungary", "Poland")
colors <- c("#f7bb3d","#cd5038")

# Function for generating yearly or quarterly labels
# Pass years as a vector
# Pass the count of data points as a number
generateLabels <- function(yearsParam, nDataPoints) {
  years <- as.character(yearsParam)
  # Check if there are 4 times as many data points as there are years (which means the data is quarterly)
  # If it is quarterly generate quarterly labels, otherwise just use the years
  if(nDataPoints == (4 * length(years))) {
    pastingQuarters <- function(x, listItems) {
      paste(listItems, x, sep=" ")
    }
    return(unlist(lapply(X=years, FUN=pastingQuarters, listItems=quarters)))
  }
  if(nDataPoints == ((length(years)+1) /2)) {
    return(years[(1:nDataPoints) * 2 - 1])
  }
  return(years)
}

# Function for plotting progresion as a line diagram
# Pass data as a list of data collections
# Pass years as a vector
# Pass a title
# Optional label for x axis
# Optional label for y axis
# Pass a vector of vertical line points
# Optional restrictions for the y axis
timeLinePlot <- function(data, yearsParam, titleParam, xLabel="", yLabel="", yAxisLabels="", verticalLines, y=c(NA,NA)) {
  nDataSets <- length(data)
  nDataPoints <- length(data[[1]])
  labels <- generateLabels(yearsParam, nDataPoints)
  
  df <- data.frame(
    x=rep(labels, nDataSets),
    y=unlist(data),
    country=c(rep(countries[1],nDataPoints),rep(countries[2],nDataPoints))
  )
  
  colSeq <- c(rep(colors[1],nDataPoints), rep(colors[2],nDataPoints))
  
  annotationY <- y[2] - (y[2]*0.1)
  drawVerticalLines <- function(line) {
    return (c(
      annotate("text", x=line[[1]], label=line[[2]], vjust=-1, y=annotationY, angle=90, color="#4C4D4D", size=5),
      geom_vline(xintercept = line[[1]], lty=3, lwd=1, color="#4C4D4D")
    ))
  }
  
  ggplot(df, aes(x=x, y=y, group=country, color=colSeq)) +
    geom_line(size=1) +
    geom_point(size=2) +
    geom_vline(xintercept = 4, col="Black", lty=3, lwd=2) +
    annotate("text", x=4, label="Recession", vjust=-1, y=annotationY, angle=90, color="#4C4D4D", size=5) +
    lapply(X=verticalLines, FUN=drawVerticalLines) +
    ggtitle(titleParam) +
    labs(x=xLabel, y=yLabel) +
    theme_light() +
    theme(text = element_text(size=17, color="#4C4D4D"), plot.title=element_text(hjust = 0.5), legend.position=c(0.1,0.8)) +
    scale_y_continuous(breaks=c(y[1]:y[2]), labels=yAxisLabels) +
    coord_cartesian(ylim = y) +
    scale_colour_manual(name="Countries",
                        breaks=colors,
                        values=c("#f7bb3d"=colors[1], "#cd5038"=colors[2]),
                        labels=countries)
}

######################
# Mean Comparison Satisfaction National Economy Poland and Hungary_Only PiS and Fidesz
######################

meanSatisfactionEconomyHUFidesz <- c(3.96, 2.77, 2.14, 1.44, 3.48, 3.87, 4.91, 5.99)
meanSatisfactionEconomyPLPiS <- c(2.76, 3.47, 4.3, 4.03, 3.85, 3.44, 3.56, 5.5)
possibleAnswers <- c("Extremely\ndissatisfied", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Extremely\nsatisfied")

timeLinePlot(
  list(meanSatisfactionEconomyHUFidesz, meanSatisfactionEconomyPLPiS),
  2002:2016,
  titleParam="Satisfaction National Economy PiS and Fidesz",
  xLabel = "Years",
  yLabel = "Satisfaction (Mean Value)",
  yAxisLabels = possibleAnswers,
  list(list(x=5,label="Fidesz won election"), list(x=7.5,label="PiS won election")),
  y=c(0,10))

######################
# Mean Comparison Satisfaction National Economy Poland and Hungary_Total N
######################

meanSatisfactionEconomyHU <- c(4.09, 3.12, 2.89, 1.82, 3.16, 3.35, 3.79, 4.82)
meanSatisfactionEconomyPL <- c(2.76, 3.19, 4.08, 4.45, 4.45, 4.2, 4.08, 4.86)
possibleAnswers <- c("Extremely\ndissatisfied", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Extremely\nsatisfied")

timeLinePlot(
  list(meanSatisfactionEconomyHU, meanSatisfactionEconomyPL),
  2002:2016,
  titleParam="Satisfaction National Economy Poland and Hungary",
  xLabel = "Years",
  yLabel = "Satisfaction (Mean Value)",
  yAxisLabels = possibleAnswers,
  list(list(x=5,label="Fidesz won election"), list(x=7.5,label="PiS won election")),
  y=c(0,10))


######################
# YOU CAN IGNORE THIS PART
# OLD STUFF, Probably doesn't work anymore
######################


######################
# Comparison Feeling About Household Income Nowadays Poland and Hungary
######################

feelingIncomeHU <- c(43.29, 31.14, 43.29, 56.31, 47.84, 54.53, 39.85, 25.99)
feelingIncomePL <- c(37.38, 29.74, 37.14, 36.17, 31.97, 41.54, 26.3, 26.85)

timeLinePlot(
  list(feelingIncomeHU, feelingIncomePL),
  2002:2016,
  titleParam="Comparison Feeling About Household Income Nowadays Poland and Hungary",
  xLabel = "Years",
  yLabel = "Feeling About Household Income (in %)",
  yAxisLabels = c("0", "25", "50", "75", "100"),
  list(list(x=5,label="Fidesz won election"), list(x=7.5,label="PiS won election")),
  y=c(0,100))

######################
# Comparison Satisfaction National Economy Poland and Hungary
######################

satisfactionEconomyHU <- c(43.07, 20.39, 11.66, 5.1, 31.73, 43.44, 59.66, 82.17)
satisfactionEconomyPL <- c(13.41, 35.1, 49.93, 42.96, 39.9, 33.1, 33.61, 72.31)

timeLinePlot(
  list(satisfactionEconomyHU, satisfactionEconomyPL),
  2002:2016,
  titleParam="Comparison Satisfaction National Economy Poland and Hungary",
  xLabel = "Years",
  yLabel = "Satisfaction with National Economy (in %)",
  yAxisLabels = c("0", "25", "50", "75", "100"),
  list(list(x=5,label="Fidesz won election"), list(x=7.5,label="PiS won election")),
  y=c(0,100))