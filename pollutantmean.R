###############################################################################
# Author: QY YU
# Date: 24/06/2020
# Project: R Programming - Week 2 - Programming Assignment 1 - Air Pollution
# Purpose: Function to calculate the mean of a pollutant (sulfate or nitrate)
#          across a specified list of monitors
###############################################################################


pollutantmean <- function(directory, polutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of  the pollutant for which we will calcultate the
  ## mean; either "sulfate" or "nitrate"
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  g = getwd()
  id = as.character(id)
  final = data.frame()
  for (monitor in id){
    monitor = paste("00", monitor, sep = "")                
    idfinal = substr(monitor, nchar(monitor)-2, nchar(monitor))
    
    file = paste(g, "/", directory, "/", idfinal, ".", "csv", sep = "")
    csv = read.csv(file)
    
    final = rbind(csv, final)
    
  }
  
  # final(head)
  # polutant = as.character(polutant)
  sub = subset(final, select = polutant)
  sub2 = as.matrix(sub)
  mean(sub2, na.rm = TRUE)        
}

# End of function.



# Testing the output 
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)


