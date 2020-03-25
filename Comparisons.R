######################
# Plotting functions
# YOU CAN IGNORE THIS PART
######################

install.packages("ggplot2")
library(ggplot2)

quarters <- c('Q1','Q2','Q3','Q4')
countries <- c("Poland", "Hungary")
colors <- c("#cd5038","#f7bb3d")

# Function for generating yearly, bi-yearly or quarterly labels
# quarterly, boolean indicating if the data is quarterly
# Pass years as a vector
# Pass the count of data points as a number
generateLabels <- function(quarterly, yearsParam, nDataPoints) {
  years <- as.character(yearsParam)
  # Check if there are 4 times as many data points as there are years (which means the data is quarterly)
  # If it is quarterly generate quarterly labels, otherwise just use the years
  if(quarterly) {
    return(apply(expand.grid(quarters=quarters, years=years), 1, paste, collapse=" "))
  }
  if(nDataPoints == ((length(years)+1) /2)) {
    return(years[(1:nDataPoints) * 2 - 1])
  }
  return(years)
}

# Function for plotting progression as a line diagram
# Pass data as a list of data collections
# Pass years as a vector
# Pass a title
# Optional label for x axis
# Optional label for y axis
# Optional yAxisLabels, labels for the y axis (string vector)
# Optional verticalLines, x value and label where to draw a vertical line (list of lists)
# Optional xRecession, x value where to draw the recession line (number)
# Optional restrictions for the y axis
timeLinePlot <- function(data, yearsParam, titleParam, xLabel="", yLabel="", yAxisLabels=c(), verticalLines=c(), xRecession=4, y=c(NA,NA)) {
  nDataSets <- length(data)
  nDataPoints <- length(data[[1]])
  quarterly <- nDataPoints == (4 * length(yearsParam))
  labels <- generateLabels(quarterly, yearsParam, nDataPoints)
  
  df <- data.frame(
    x=rep(c(labels), nDataSets),
    y=unlist(data),
    country=c(rep(countries[1],nDataPoints),rep(countries[2],nDataPoints))
  )
  
  df$x <- factor(df$x, levels=unique(df$x))
  df$country <- factor(df$country, levels=unique(df$country))
  
  colSeq <- c(rep(colors[1],nDataPoints), rep(colors[2],nDataPoints))
  
  zeroLine <- function(df) {
    if(min(df$y) < 0) {
      return(geom_hline(yintercept = 0, col="Black"))
    }
  }
  
  recessionLine <- function(xRecession) {
    if(xRecession > 0) {
      return(c(
        geom_vline(xintercept=xRecession, col="Black", lty=2, lwd=1),
        annotate("text", x=xRecession, label="Recession", vjust=-1, y=annotationY, angle=90, color="Black", size=5)
      ))
    }
  }
  
  if(!is.na(y[2])) {
    offset = (y[2] - y[1]) * 0.1
    annotationY <- y[2] - offset
  } else {
    maxYVal = max(df$y)
    offset = (maxYVal - min(df$y)) * 0.1
    annotationY <- maxYVal - offset
  }
  drawVerticalLines <- function(line) {
    return (c(
      annotate("text", x=line[[1]], label=line[[2]], vjust=-1, y=annotationY, angle=90, color="Black", size=5),
      geom_vline(xintercept = line[[1]], lty=3, lwd=1, color="Black")
    ))
  }
  
  alterYAxis <- function(yAxisLabels) {
    if (length(yAxisLabels) > 0) {
      scale_y_continuous(breaks=c(y[1]:y[2]), labels=yAxisLabels)
    }
  }
  
  ggplot(df, aes(x=x, y=y, group=country, color=colSeq)) +
    geom_line(size=1) +
    geom_point(size=2) +
    zeroLine(df) +
    recessionLine(xRecession) +
    lapply(X=verticalLines, FUN=drawVerticalLines) +
    ggtitle(titleParam) +
    labs(x=xLabel, y=yLabel) +
    theme_linedraw() +
    theme(
      text = element_text(size=16, color="Black"),
      plot.title=element_text(hjust = 0.5, face="bold"),
      axis.text.x=element_text(angle= if(quarterly) 90 else 0),
      legend.position=c(0.9,0.1)
    ) +
    alterYAxis(yAxisLabels) +
    coord_cartesian(ylim = y) +
    scale_colour_manual(name="Countries",
                        breaks=colors,
                        values=colors,
                        labels=countries)
}

######################
# Comparison real GDP change per quarter in percentage Poland and Hungary
######################

realGDPChangePerQuarterPL <- c(1.5, 2.3, 1.7, -1.4, 4.5, 1.2, 1.7, 1.3, 2.0, 0.4, -0.4, 0.3, 1.7, 0.0, 0.6, 2.4, -0.6, 1.5, 1.4, 1.0, 1.3, 1.3, 1.3, 0.8, 0.1, -0.2, 0.4, -0.4, 0.2, 1.1, 0.8, 0.1, 1.1, 1.1, 0.8, 0.6, 1.2, 0.7, 1.2, 1.2)
realGDPChangePerQuarterHU <- c(0.9,1.3,0.6,0.9,-1.3,-0.1,0.6,0.4,0.8,0.5,-0.2,-3.2,-4.2,-0.2,0.0,0.0,-0.2,0.7,0.6,0.1,0.9,0.0,0.3,0.9,-2.0,-0.3,0.4,-0.2,0.6,0.9,1.3,1.1,0.8,1.3,0.9,0.6,1.7,0.2,0.7,1.0)

timeLinePlot(
  data=list(realGDPChangePerQuarterPL, realGDPChangePerQuarterHU),
  yearsParam=2006:2015,
  titleParam="Real GDP Growth Rate in Poland and Hungary (Quarterly Data)",
  xLabel="Year",
  yLabel="GDP Growth Rate (in %)",
  verticalLines=list(list(x=18,label="Election Fidesz")),
  xRecession=9)

######################
# Comparison Employment rate Poland and Hungary
######################

employmentRatePL <- c(63.4, 63.4, 63.5, 63.2, 63.0, 63.2, 63.1, 63.3, 63.7, 63.7, 63.9, 64.1, 64.6, 64.5, 64.9, 64.9, 65.1, 65.3, 65.5, 65.4, 65.4, 65.7, 65.8, 66.0, 66.2, 66.5, 66.5, 66.8, 66.6, 67.0, 67.1, 67.3, 67.7, 67.8, 67.9, 68.0, 68.1, 67.9, 68.1, 68.2)
employmentRateHU <- c(61.9, 62.0, 61.9, 62.2, 61.9, 61.7, 61.5, 61.5, 61.1, 61.0, 61.4, 61.3, 61.1, 61.2, 61.1, 61.4, 61.7, 61.9, 62.0, 62.0, 62.0, 62.3, 62.5, 62.7, 63.0, 63.6, 64.1, 64.2, 63.8, 64.6, 65.0, 65.4, 66.5, 66.8, 67.2, 67.4, 68.1, 68.5, 68.9, 69.1)

timeLinePlot(
  data=list(employmentRatePL, employmentRateHU),
  yearsParam=2006:2015,
  titleParam="Employment Rate in Poland and Hungary (Quarterly Data)",
  xLabel="Years",
  yLabel="Employment Rate (in %)",
  verticalLines=list(list(x=18,label="Election Fidesz")),
  xRecession=9,
  y=c(61,69))

######################
# Mean Comparison Satisfaction National Economy Poland and Hungary_Only PiS and Fidesz
######################

meanSatisfactionEconomyPLPiS <- c(2.81, 3.47, 4.33, 4.04, 3.84, 3.44, 3.62, 5.46)
meanSatisfactionEconomyHUFidesz <- c(3.94, 2.76, 2.17, 1.4, 3.48, 3.88, 4.88, 5.95)
possibleAnswers <- c("Extremely\ndissatisfied", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Extremely\nsatisfied")

timeLinePlot(
  data=list(meanSatisfactionEconomyPLPiS, meanSatisfactionEconomyHUFidesz),
  yearsParam=2002:2016,
  titleParam="Satisfaction National Economy PiS and Fidesz",
  xLabel = "Years",
  yLabel = "Satisfaction (Mean Value)",
  yAxisLabels = possibleAnswers,
  verticalLines=list(list(x=5,label="Election Fidesz"), list(x=7.5,label="Election PiS")),
  y=c(0,10))

######################
# Mean Comparison Satisfaction National Economy Poland and Hungary_Total N
######################

meanSatisfactionEconomyPL <- c(2.77, 3.19, 4.09, 4.45, 4.43, 4.19, 4.08, 4.84)
meanSatisfactionEconomyHU <- c(4.09, 3.19, 2.86, 1.7, 3.19, 3.37, 3.8, 4.81)
possibleAnswers <- c("Extremely\ndissatisfied", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Extremely\nsatisfied")

timeLinePlot(
  data=list(meanSatisfactionEconomyPL, meanSatisfactionEconomyHU),
  yearsParam=2002:2016,
  titleParam="Satisfaction National Economy Poland and Hungary",
  xLabel = "Years",
  yLabel = "Satisfaction (Mean Value)",
  yAxisLabels = possibleAnswers,
  verticalLines=list(list(x=5,label="Election Fidesz"), list(x=7.5,label="Election PiS")),
  y=c(0,10))

######################
# Comparison Press Freedom Poland and Hungary
######################

PressFreedomPL <- c(22, 19, 18, 47, 54, 58, 59)
PressFreedomHU <- c(56, 64, 65, 67, 71, 73, 87)

timeLinePlot(
  data=list(PressFreedomPL, PressFreedomHU),
  yearsParam=2013:2019,
  titleParam="Freedom of Press in Poland and Hungary",
  xLabel = "Years",
  yLabel = "Ranking (out of 180)",
  verticalLines=list(list(x=3,label="Election PiS")),
  xRecession=-1,
  y=c(0,90))
