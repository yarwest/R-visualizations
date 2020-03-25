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
# country, the country which the graph concerns
# colorParam, the color to use for the line, points and the legend
# Pass years as a vector
# Pass a title
# Optional label for x axis
# Optional label for y axis
# Optional yAxisLabels, labels for the y axis (string vector)
# Optional verticalLines, x value and label where to draw a vertical line (list of lists)
# Optional xRecession, x value where to draw the recession line (number)
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
  
  ggplot(df, aes(x=x, y=y, group=country, color=colorParam)) +
    geom_line(size=1, color=colorParam) +
    geom_point(size=2, color=colorParam) +
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
      legend.position="none"
    ) +
    alterYAxis(yAxisLabels) +
    coord_cartesian(ylim = y)
}

######################
# Poland
######################


######################
# Gross Disposable Income of Households in real terms per capita Poland
######################

GDIHouseholdsPL <- c(18258,	18931,	18889,	19087,	18959,	19107,	20002,	21026,	21960,	23035,	23505,	23591,	23842,	24195,	24897,	25837,	27377,	28208)

timeLinePlot(
  data=list(GDIHouseholdsPL),
  yearsParam=2000:2017,
  country=countries[1],
  colorParam=colors[1],
  titleParam="Gross Disposable Income of Households in real terms per capita Poland",
  xLabel="Years",
  yLabel="Gross Disposable Income (in PLN)",
  verticalLines=list(list(x=16,label="Election PiS")),
  xRecession=9)

######################
# Hungary
######################

######################
# Gross Disposable Income of Households in real terms per capita Hungary
######################

GDIHouseholdsHU <- c(1289063, 1359117, 1428376, 1527216, 1623920, 1702682, 1738129, 1686716, 1655776, 1594416, 1566814, 1628180, 1579022, 1614380, 1669150, 1736426, 1824989, 1908398)
timeLinePlot(
  data=list(GDIHouseholdsHU),
  yearsParam=2000:2017,
  country=countries[2],
  colorParam=colors[2],
  titleParam="Gross Disposable Income of Households in real terms per capita Hungary",
  xLabel="Years",
  yLabel="Gross disposable income (in HUF)",
  verticalLines=list(list(x=11,label="Election Fidesz")),
  xRecession=9)