######################
# Plotting functions
# YOU CAN IGNORE THIS PART
######################

install.packages("ggplot2")
library(ggplot2)

quarters <- c('Q1','Q2','Q3','Q4')
countries <- c("Poland", "Hungary")
colors <- c("#cd5038","#f7bb3d")

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
timeLinePlot <- function(data, yearsParam, country, colorParam, titleParam, xLabel="", yLabel="", yAxisLabels=c(), verticalLines=c(), xRecession=4, y=c(NA,NA)) {
  nDataSets <- length(data)
  nDataPoints <- length(data[[1]])
  quarterly <- nDataPoints == (4 * length(yearsParam))
  labels <- generateLabels(quarterly, yearsParam, nDataPoints)
  
  df <- data.frame(
    x=rep(c(labels), nDataSets),
    y=unlist(data),
    country=rep(country,nDataPoints)
  )

  df$x <- factor(df$x, levels=unique(df$x))
  
  if(!is.na(y[2])) {
    annotationY <- y[2] - (y[2]*0.1)
  } else {
    annotationY = mean(c(max(df$y), min(df$y)))
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
  
  ggplot(df, aes(x=x, y=y, group=country, color=colorParam)) +
    geom_line(size=1, color=colorParam) +
    geom_point(size=2, color=colorParam) +
    geom_hline(yintercept = 0, col="Black") +
    geom_vline(xintercept=xRecession, col="Black", lty=3, lwd=2) +
    annotate("text", x=xRecession, label="Recession", vjust=-1, y=annotationY, angle=90, color="#4C4D4D", size=5) +
    lapply(X=verticalLines, FUN=drawVerticalLines) +
    ggtitle(titleParam) +
    labs(x=xLabel, y=yLabel) +
    theme_light() +
    theme(
      text = element_text(size=17, color="#4C4D4D"),
      plot.title=element_text(hjust = 0.5),
      axis.text.x=element_text(angle= if(quarterly) 90 else 0),
      legend.position="none"
    ) +
    alterYAxis(yAxisLabels) +
    coord_cartesian(ylim = y)
}


######################
# Poland
######################

######################
# Real GDP change per quarter in percentage Poland
######################

realGDPChangePerQuarterPL <- c(1.5, 2.3, 1.7, -1.4, 4.5, 1.2, 1.7, 1.3, 2.0, 0.4, -0.4, 0.3, 1.7, 0.0, 0.6, 2.4, -0.6, 1.5, 1.4, 1.0, 1.3, 1.3, 1.3, 0.8, 0.1, -0.2, 0.4, -0.4, 0.2, 1.1, 0.8, 0.1, 1.1, 1.1, 0.8, 0.6, 1.2, 0.7, 1.2, 1.2)
timeLinePlot(
  data=list(realGDPChangePerQuarterPL),
  yearsParam=2006:2015,
  country=countries[1],
  color=colors[1],
  titleParam="Real GDP growth rate per quarter in Poland",
  xLabel="Quarters",
  yLabel="GDP growth rate (in %)",
  xRecession=9)

######################
# Real GDP change per year in percentage Poland
######################

realGDPChangePerYearPL <- c(7.0, 4.2, 2.8, 3.6, 5.0, 1.6, 1.4, 3.3, 3.8)
timeLinePlot(
  data=list(realGDPChangePerYearPL),
  yearsParam=2007:2015,
  country=countries[1],
  color=colors[1],
  titleParam="Real GDP growth rate per year in Poland",
  xLabel="Years",
  yLabel="GDP growth rate (in %)",
  xRecession=2)

######################
# Hungary
######################

######################
# Real GDP change per quarter in percentage Hungary
######################

realGDPChangePerQuarterHU <- c(0.9,1.3,0.6,0.9,-1.3,-0.1,0.6,0.4,0.8,0.5,-0.2,-3.2,-4.2,-0.2,0.0,0.0,-0.2,0.7,0.6,0.1,0.9,0.0,0.3,0.9,-2.0,-0.3,0.4,-0.2,0.6,0.9,1.3,1.1,0.8,1.3,0.9,0.6,1.7,0.2,0.7,1.0)
timeLinePlot(
  data=list(realGDPChangePerQuarterHU),
  yearsParam=2006:2015,
  country=countries[2],
  color=colors[2],
  titleParam="Real GDP growth rate per quarter in Hungary",
  xLabel="Quarters",
  yLabel="GDP growth rate (in %)",
  verticalLines=list(list(x=18,label="Fidesz won election")),
  xRecession=9)

######################
# Real GDP change per year in percentage Hungary
######################

realGDPChangePerYearHU <- c(0.2, 1.1, -6.7, 0.7, 1.8, -1.5, 2.0, 4.2, 3.8)
timeLinePlot(
  data=list(realGDPChangePerYearHU),
  yearsParam=2007:2015,
  country=countries[2],
  color=colors[2],
  titleParam="Real GDP growth rate per year in Hungary",
  xLabel="Years",
  yLabel="GDP growth rate (in %)",
  verticalLines=list(list(x=5,label="Fidesz won election")),
  xRecession=2)