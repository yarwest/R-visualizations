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
# Pass a vector of vertical line points
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
  
  colSeq <- c(rep(colors[1],nDataPoints), rep(colors[2],nDataPoints))
  
  zeroLine <- function(df) {
    if(min(df$y) < 0) {
      return(geom_hline(yintercept = 0, col="Black"))
    }
  }
  
  recessionLine <- function(xRecession) {
    if(xRecession > 0) {
      return(c(
        geom_vline(xintercept=xRecession, col="Black", lty=3, lwd=2),
        annotate("text", x=xRecession, label="Recession", vjust=-1, y=annotationY, angle=90, color="#4C4D4D", size=5)
      ))
    }
  }
  
  if(!is.na(y[2])) {
    annotationY <- y[2] - (y[2]*0.1)
  } else {
    maxYVal = max(df$y)
    annotationY <- maxYVal - (maxYVal*0.25)
  }
  drawVerticalLines <- function(line) {
    return (c(
      annotate("text", x=line[[1]], label=line[[2]], vjust=-1, y=annotationY, angle=90, color="#4C4D4D", size=5),
      geom_vline(xintercept = line[[1]], lty=3, lwd=1, color="#4C4D4D")
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
    theme_light() +
    theme(
      text = element_text(size=17, color="#4C4D4D"),
      plot.title=element_text(hjust = 0.5),
      axis.text.x=element_text(angle= if(quarterly) 90 else 0),
      legend.position=c(0.9,0.1)
    ) +
    alterYAxis(yAxisLabels) +
    coord_cartesian(ylim = y) +
    scale_colour_manual(name="Countries",
                        breaks=colors,
                        values=c("#f7bb3d"=colors[1], "#cd5038"=colors[2]),
                        labels=countries)
}

######################
# Comparison real GDP change per quarter in percentage Hungary and Poland
######################

realGDPChangePerQuarterHU <- c(0.9,1.3,0.6,0.9,-1.3,-0.1,0.6,0.4,0.8,0.5,-0.2,-3.2,-4.2,-0.2,0.0,0.0,-0.2,0.7,0.6,0.1,0.9,0.0,0.3,0.9,-2.0,-0.3,0.4,-0.2,0.6,0.9,1.3,1.1,0.8,1.3,0.9,0.6,1.7,0.2,0.7,1.0)
realGDPChangePerQuarterPL <- c(1.5, 2.3, 1.7, -1.4, 4.5, 1.2, 1.7, 1.3, 2.0, 0.4, -0.4, 0.3, 1.7, 0.0, 0.6, 2.4, -0.6, 1.5, 1.4, 1.0, 1.3, 1.3, 1.3, 0.8, 0.1, -0.2, 0.4, -0.4, 0.2, 1.1, 0.8, 0.1, 1.1, 1.1, 0.8, 0.6, 1.2, 0.7, 1.2, 1.2)
timeLinePlot(
  data=list(realGDPChangePerQuarterHU, realGDPChangePerQuarterPL),
  yearsParam=2006:2015,
  titleParam="Real GDP growth rate per quarter Hungary and Poland",
  xLabel="Quarters",
  yLabel="GDP growth rate (in %)",
  verticalLines=list(list(x=17,label="Fidesz won election")),
  xRecession=9)

######################
# Comparison unemployment rate per quarter Hungary and Poland
######################

unemploymentRateHU <- c(7.7, 7.2, 7.5, 7.5, 7.5, 7.0, 7.3, 7.8, 8.0, 7.6, 7.7, 8.0, 9.8, 9.6, 10.3, 10.4, 11.8, 11.2, 10.8, 10.8, 11.8, 10.9, 10.7, 10.7, 11.9, 11.0, 10.5, 10.6, 11.6, 10.2, 9.8, 9.1, 8.3, 8.1, 7.4, 7.1, 7.8, 6.9, 6.4, 6.2)
unemploymentRatePL <- c(16.1, 14.1, 13.1, 12.2, 11.3, 9.6, 9.0, 8.5, 8.1, 7.1, 6.6, 6.7, 8.3, 7.9, 8.1, 8.5, 10.6, 9.6, 9.2, 9.3, 10.1, 9.5, 9.3, 9.8, 10.5, 9.9, 9.9, 10.1, 11.3, 10.5, 9.8, 9.8, 10.6, 9.1, 8.2, 8.1, 8.6, 7.4, 7.1, 6.9)

timeLinePlot(
  data=list(unemploymentRateHU, unemploymentRatePL),
  yearsParam=2006:2015,
  titleParam="Comparison unemployment rate per quarter Hungary and Poland",
  xLabel="Years",
  yLabel="Unemployment rate (in %)",
  verticalLines=list(list(x=17,label="Fidesz won election")),
  xRecession=9)

######################
# Mean Comparison Satisfaction National Economy Hungary and Poland_Only PiS and Fidesz
######################

meanSatisfactionEconomyHUFidesz <- c(3.94, 2.76, 2.17, 1.4, 3.48, 3.88, 4.88, 5.95)
meanSatisfactionEconomyPLPiS <- c(2.81, 3.47, 4.33, 4.04, 3.84, 3.44, 3.62, 5.46)
possibleAnswers <- c("Extremely\ndissatisfied", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Extremely\nsatisfied")

timeLinePlot(
  data=list(meanSatisfactionEconomyHUFidesz, meanSatisfactionEconomyPLPiS),
  yearsParam=2002:2016,
  titleParam="Satisfaction National Economy PiS and Fidesz",
  xLabel = "Years",
  yLabel = "Satisfaction (Mean Value)",
  yAxisLabels = possibleAnswers,
  verticalLines=list(list(x=5,label="Fidesz won election"), list(x=7.5,label="PiS won election")),
  y=c(0,10))

######################
# Mean Comparison Satisfaction National Economy Hungary and Poland_Total N
######################

meanSatisfactionEconomyHU <- c(4.09, 3.19, 2.86, 1.7, 3.19, 3.37, 3.8, 4.81)
meanSatisfactionEconomyPL <- c(2.77, 3.19, 4.09, 4.45, 4.43, 4.19, 4.08, 4.84)
possibleAnswers <- c("Extremely\ndissatisfied", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Extremely\nsatisfied")

timeLinePlot(
  data=list(meanSatisfactionEconomyHU, meanSatisfactionEconomyPL),
  yearsParam=2002:2016,
  titleParam="Satisfaction National Economy Hungary and Poland",
  xLabel = "Years",
  yLabel = "Satisfaction (Mean Value)",
  yAxisLabels = possibleAnswers,
  verticalLines=list(list(x=5,label="Fidesz won election"), list(x=7.5,label="PiS won election")),
  y=c(0,10))

######################
# Comparison Press Freedom Poland and Hungary
######################

PressFreedomHU <- c(56, 64, 65, 67, 71, 73, 87)
PressFreedomPL <- c(22, 19, 18, 47, 54, 58, 59)

timeLinePlot(
  data=list(PressFreedomHU, PressFreedomPL),
  yearsParam=2013:2019,
  titleParam="Comparison Press Freedom Poland and Hungary",
  xLabel = "Years",
  yLabel = "Ranking (out of 180)",
  verticalLines=list(list(x=2,label="PiS won election")),
  xRecession=-1,
  y=c(0,90))