#################################################################################################
# Author: QY YU
# Date: 24/06/2020
# Project: R Programming - Week 2 - Programming Assignment 1 - Air Pollution
# Purpose: Function to return a vector of correlations for the monitors 
#          that meet the threshold requirement
# which takes a directory of data files and a threshold for complete cases and 
# calculates the correlation between sulfate and nitrate for monitor locations 
# where the number of completely observed cases (on all variables) is greater than the threshold 
#################################################################################################


corr <- function(directory, threshold = 0){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the 
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  CC = complete(directory, id = 1:332)
  ans = vector()
  ans = as.numeric(ans)
  Q1 = subset(CC, nobs > threshold)
  vec = as.character(Q1$id)

  g = getwd()
  # final = data.frame()
  for (monitor in vec) {
    monitor = paste("00", monitor, sep = "")                
    idfinal = substr(monitor, nchar(monitor)-2, nchar(monitor))
    
    file = paste(g, "/", directory, "/", idfinal, ".", "csv", sep = "")
    csv = read.csv(file)
    cc_csv = csv[complete.cases(csv),]
    corr = cor(cc_csv$sulfate, cc_csv$nitrate)
    ans = c(ans, corr)
  }
  
  ans
}

# End of function.



# Testing the output 
source("corr.R")
source("complete.R")

cr <- corr("specdata", 150)
head(cr)
summary(cr)

cr <- corr("specdata", 400)
head(cr)
summary(cr)

cr <- corr("specdata", 5000)
summary(cr)
length(cr)

cr <- corr("specdata")
summary(cr)
length(cr)


