#################################################################################
# Author: QY YU
# Date: 24/06/2020
# Project: R Programming - Week 2 - Programming Assignment 1 - Air Pollution
# Purpose: Function to return a data frame where the first column is the name
# is the name of the file and the second column is the number of complete cases.
#################################################################################


complete <- function(directory, id = 1:332) {
  ## 'director' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the from:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
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
  
  CCvec = complete.cases(final)
  CC = final[CCvec, ]
  # res = tapply(CC$sulfate, CC$ID, length)
  res = table(CC$ID)
  resres = as.data.frame(res)
  colnames(resres) = c("id", "nobs")
  resres
}

# End of function.



# Testing the output 
complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)


