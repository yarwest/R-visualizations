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
  
  avgY <- mean(df$y)
  drawVerticalLines <- function(line) {
    geom_text(aes(x=line[1], label=paste("\n",line[2]), y=avgY), angle=90, color="black")
    geom_vline(xintercept = line[1], lty=3, lwd=1)
  }

  lapply(X=verticalLines, FUN=drawVerticalLines)
  
  ggplot(df, aes(x=x, y=y, group=country, color=colSeq)) +
    geom_line(size=1) +
    geom_point(size=2) +
    geom_vline(xintercept = 4, col="Black", lty=3, lwd=2) +
    geom_text(aes(x=4, label="\nRecession", y=avgY), angle=90, color="black") +
    lapply(X=verticalLines, FUN=drawVerticalLines) +
    ggtitle(titleParam) +
    labs(x=xLabel, y=yLabel) +
    theme(text = element_text(size=25)) +
    theme_light() +
    scale_y_continuous(breaks=c(yAxisLabels$y), labels=yAxisLabels$label) +
    coord_cartesian(ylim = y) +
    scale_colour_manual(name="Countries",
                        breaks=colors,
                        values=c("#f7bb3d"=colors[1], "#cd5038"=colors[2]),
                        labels=countries)
}

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
  yAxisLabels = c(
    list(y=0,label="0"),
    list(y=25,label="25"),
    list(y=50,label="50"),
    list(y=75,label="75"),
    list(y=100,label="100")
  ),
  c(list(x=5,label="Fidesz won election"), list(x=7.5,label="PiS won election")),
  y=c(0,100))

######################
# Mean Comparison Satisfaction National Economy Poland and Hungary_Only PiS and Fidesz
######################

meanSatisfactionEconomyHUFidesz <- c(3.96, 2.77, 2.14, 1.44, 3.48, 3.87, 4.91, 5.99)
meanSatisfactionEconomyPLPiS <- c(2.76, 3.47, 4.3, 4.03, 3.85, 3.44, 3.56, 5.5)
possibleAnswers <- c("Extremely dissatisfied", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Extremely satisfied")

timeLinePlot(
  list(meanSatisfactionEconomyHUFidesz, meanSatisfactionEconomyPLPiS),
  2002:2016,
  titleParam="Satisfaction National Economy PiS and Fidesz",
  xLabel = "Years",
  yLabel = "Satisfaction (Mean Value)",
  yAxisLabels = possibleAnswers,
  c(list(x=5,label="Fidesz won election"), list(x=7.5,label="PiS won election")),
  y=c(0,10))

######################
# Mean Comparison Satisfaction National Economy Poland and Hungary_Total N
######################

meanSatisfactionEconomyHU <- c(4.09, 3.12, 2.89, 1.82, 3.16, 3.35, 3.79, 4.82)
meanSatisfactionEconomyPL <- c(2.76, 3.19, 4.08, 4.45, 4.45, 4.2, 4.08, 4.86)
possibleAnswers <- c("Extremely dissatisfied", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Extremely satisfied")

timeLinePlot(
  list(meanSatisfactionEconomyHU, meanSatisfactionEconomyPL),
  2002:2016,
  titleParam="Satisfaction National Economy Poland and Hungary",
  xLabel = "Years",
  yLabel = "Satisfaction (Mean Value)",
  yAxisLabels = possibleAnswers,
  c(list(x=5,label="Fidesz won election"), list(x=7.5,label="PiS won election")),
  y=c(0,10))

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
  yAxisLabels = c(
    list(y=0,label="0"),
    list(y=25,label="25"),
    list(y=50,label="50"),
    list(y=75,label="75"),
    list(y=100,label="100")
  ),
  c(list(x=5,label="Fidesz won election"), list(x=7.5,label="PiS won election")),
  y=c(0,100))