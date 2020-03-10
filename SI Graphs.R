######################
# Plotting functions
# YOU CAN IGNORE THIS PART
######################

quarters <- c('Q1','Q2','Q3','Q4')

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
# Pass data as a vector of data points
# Pass years as a vector
# Pass a title
# Pass a vector of vertical lines 
timeLinePlot <- function(data, yearsParam, titleParam, verticalLines) {
  # Organize data
  nDataPoints <- length(data)
  dataList <- list(x = 1:nDataPoints, y = data)
  
  # Plot line with points
  plot(dataList, type="o", xaxt="n", ann=FALSE, pch=20)
  
  # Generate and set labels for x axis
  axis(1, at=1:nDataPoints, labels=generateLabels(yearsParam, nDataPoints), las=2)
  
  # Add 0 line
  abline(h=c(0), col="Red", lty=3, lwd=1)
  
  drawVerticalLines <- function(x) {
    abline(v = x, col="Black", lty=3, lwd=1)
  }
  lapply(X=verticalLines, FUN=drawVerticalLines)
  
  # Add a title
  title(titleParam)
}

# Function for plotting comparision line diagram with a variable number of lines
# Pass data as a matrix of data sets (columns) where the columns are named
# Pass years as a vector
# Pass a title
# Pass a vector of vertical lines
comparisonLinePlot <- function(data, yearsParam, titleParam, verticalLines) {
  # Organize data
  nDataSets <- ncol(data)
  nDataPoints <- length(data[,1])
  
  # Plot matrix
  matplot(data, type=c("o"), pch=20, col=c('#f7bb3d','#cd5038'), xaxt="n", ann=FALSE, lty=1, lwd=2)
  # Create legend based on matrix column names
  legend("topleft", legend=colnames(dataMat), col=c('#f7bb3d','#cd5038'), pch=20)
  
  # Generate and set labels for x axis
  axis(1, at=1:nDataPoints, labels=generateLabels(yearsParam, nDataPoints), las=2)
  
  # Add 0 line
  abline(h=c(0), col="Black", lty=3, lwd=1)
  
  drawVerticalLines <- function(x) {
    abline(v = x, col="Black", lty=3, lwd=1)
  }
  lapply(X=verticalLines, FUN=drawVerticalLines)
  
  # add a title and subtitle
  title(titleParam)
}



######################
# Comparison Satisfaction National Economy Poland and Hungary
######################

SatisfactionEconomyPL <- c(13.41, 35.1, 49.93, 42.96, 39.9, 33.1, 33.61, 72.31)
SatisfactionEconomyHU <- c(43.07, 20.39, 11.66, 5.1, 31.73, 43.44, 59.66, 82.17)

dataMat <- matrix(c(SatisfactionEconomyPL, SatisfactionEconomyHU), ncol=2, byrow = FALSE)
colnames(dataMat) = c("PL", "HU")

comparisonLinePlot(dataMat, 2002:2016, "Comparison Satisfaction National Economy Poland and Hungary", c(5, 7.5))
abline(v = 4, col="Black", lty=3, lwd=2)
title(xlab="Years", ylab="Satisfaction with National Economy (in %)")

######################
# Mean Comparison Satisfaction National Economy Poland and Hungary_Only PiS and Fidesz
######################

SatisfactionEconomyPL <- c(2.76, 3.47, 4.3, 4.03, 3.85, 3.44, 3.56, 5.5)
SatisfactionEconomyHU <- c(3.96, 2.77, 2.14, 1.44, 3.48, 3.87, 4.91, 5.99)

dataMat <- matrix(c(SatisfactionEconomyPL, SatisfactionEconomyHU), ncol=2, byrow = FALSE)
colnames(dataMat) = c("PL", "HU")

comparisonLinePlot(dataMat, 2002:2016, "Satisfaction National Economy PiS and Fidesz", c(5, 7.5))
abline(v = 4, col="Black", lty=3, lwd=2)
title(xlab="Years", ylab="Satisfaction (Mean Value)")

######################
# Mean Comparison Satisfaction National Economy Poland and Hungary_Total N
######################

SatisfactionEconomyPL <- c(2.76, 3.19, 4.08, 4.45, 4.45, 4.2, 4.08, 4.86)
SatisfactionEconomyHU <- c(4.09, 3.12, 2.89, 1.82, 3.16, 3.35, 3.79, 4.82)

dataMat <- matrix(c(SatisfactionEconomyPL, SatisfactionEconomyHU), ncol=2, byrow = FALSE)
colnames(dataMat) = c("PL", "HU")

comparisonLinePlot(dataMat, 2002:2016, "Satisfaction National Economy Poland and Hungary", c(5, 7.5))
abline(v = 4, col="Black", lty=3, lwd=2)
title(xlab="Years", ylab="Satisfaction (Mean Value)")

######################
# Comparison Feeling About Household Income Nowadays Poland and Hungary
######################

SatisfactionIncomePL <- c(37.38, 29.74, 37.14, 36.17, 31.97, 41.54, 26.3, 26.85)
SatisfactionIncomeHU <- c(43.29, 31.14, 43.29, 56.31, 47.84, 54.53, 39.85, 25.99)

dataMat <- matrix(c(SatisfactionIncomePL, SatisfactionIncomeHU), ncol=2, byrow = FALSE)
colnames(dataMat) = c("PL", "HU")

comparisonLinePlot(dataMat, 2002:2016, "Comparison Feeling About Household Income Nowadays Poland and Hungary", c(5, 7.5))





