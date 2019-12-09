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
  return(generatedLabels <- years)
}

# Function for plotting progresion as a line diagram
# Pass data as a vector of data points
# Pass years as a vector
# Pass a title
timeLinePlot <- function(data, yearsParam, titleParam) {
  # Organize data
  nDataPoints <- length(data)
  dataList <- list(x = 1:nDataPoints, y = data)
  
  # Plot line with points
  plot(dataList, type="o", xaxt="n", ann=FALSE, pch=20)
  
  # Generate and set labels for x axis
  axis(1, at=1:nDataPoints, labels=generateLabels(yearsParam, nDataPoints), las=2)
  
  # Add 0 line
  abline(h=c(0), col="Red", lty=3, lwd=1)
  
  # Add a title
  title(titleParam)
}

# Function for plotting comparision line diagram with a variable number of lines
# Pass data as a matrix of data sets (columns) where the columns are named
# Pass years as a vector
# Pass a title
comparisonLinePlot <- function(data, yearsParam, titleParam) {
  # Organize data
  nDataSets <- ncol(data)
  nDataPoints <- length(data[,1])
  
  # Plot matrix
  matplot(data, type=c("o"), pch=20, col=1:nDataSets, xaxt="n", ann=FALSE)
  # Create legend based on matrix column names
  legend("topleft", legend=colnames(dataMat), col=1:nDataSets, pch=20)
  
  # Generate and set labels for x axis
  axis(1, at=1:nDataPoints, labels=generateLabels(yearsParam, nDataPoints), las=2)
  
  # add a title and subtitle
  title(titleParam)
}

######################
# Poland
######################

######################
# Real GDP change per year in percentage Poland
######################

realGDPChangePerYearPL <- c(7.0, 4.2, 2.8, 3.6, 5.0, 1.6, 1.4, 3.3, 3.8)
timeLinePlot(realGDPChangePerYearPL, 2007:2015, "Real GDP growth rate per year in Poland")


######################
# Real GDP change per quarter in percentage Poland
######################

realGDPChangePerQuarterPL <- c(1.5, 2.3, 1.7, -1.4, 4.5, 1.2, 1.7, 1.3, 2.0, 0.4, -0.4, 0.3, 1.7, 0.0, 0.6, 2.4, -0.6, 1.5, 1.4, 1.0, 1.3, 1.3, 1.3, 0.8, 0.1, -0.2, 0.4, -0.4, 0.2, 1.1, 0.8, 0.1, 1.1, 1.1, 0.8, 0.6, 1.2, 0.7, 1.2, 1.2)
timeLinePlot(realGDPChangePerQuarterPL, 2006:2015, "Real GDP growth rate per quarter in Poland")


######################
# Hungary
######################

######################
# Real GDP change per quarter in percentage Hungary
######################

realGDPChangePerQuarterHU <- c(0.9,1.3,0.6,0.9,-1.3,-0.1,0.6,0.4,0.8,0.5,-0.2,-3.2,-4.2,-0.2,0.0,0.0,-0.2,0.7,0.6,0.1,0.9,0.0,0.3,0.9,-2.0,-0.3,0.4,-0.2,0.6,0.9,1.3,1.1,0.8,1.3,0.9,0.6,1.7,0.2,0.7,1.0)
timeLinePlot(realGDPChangePerQuarterHU, 2006:2015, "Real GDP growth rate per quarter in Hungary")


######################
# Real GDP change per year in percentage Hungary
######################

realGDPChangePerYearHU <- c(0.2, 1.1, -6.7, 0.7, 1.8, -1.5, 2.0, 4.2, 3.8)
timeLinePlot(realGDPChangePerYearHU, 2007:2015, "Real GDP growth rate per year in Hungary")



######################
# Comparison unemployment rate Poland and Hungary
######################

unemploymentRatePL <- c(16.1, 14.1, 13.1, 12.2, 11.3, 9.6, 9.0, 8.5, 8.1, 7.1, 6.6, 6.7, 8.3, 7.9, 8.1, 8.5, 10.6, 9.6, 9.2, 9.3, 10.1, 9.5, 9.3, 9.8, 10.5, 9.9, 9.9, 10.1, 11.3, 10.5, 9.8, 9.8, 10.6, 9.1, 8.2, 8.1, 8.6, 7.4, 7.1, 6.9)
unemploymentRateHU <- c(7.7, 7.2, 7.5, 7.5, 7.5, 7.0, 7.3, 7.8, 8.0, 7.6, 7.7, 8.0, 9.8, 9.6, 10.3, 10.4, 11.8, 11.2, 10.8, 10.8, 11.8, 10.9, 10.7, 10.7, 11.9, 11.0, 10.5, 10.6, 11.6, 10.2, 9.8, 9.1, 8.3, 8.1, 7.4, 7.1, 7.8, 6.9, 6.4, 6.2)

dataMat <- matrix(c(unemploymentRatePL, unemploymentRateHU), ncol=2, byrow = FALSE)
colnames(dataMat) = c("PL", "HU")

comparisonLinePlot(dataMat, 2006:2015, "Comparison unemployment rate Poland and Hungary")
