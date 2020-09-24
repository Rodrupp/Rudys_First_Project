pollutantmean <- function(directory, pollutant, id = 1:332) {
        if(pollutant == "sulphate") {
                pollutant <- "sulfate"
        }  ## function accepts old spelling of sulphate
        
        if((pollutant != "sulfate") & (pollutant != "nitrate")) {
                return("Pollutant must be Sulfate or Nitrate")
        }  ## returns error in case of wrong pollutant 
        
        filename_list <- list.files(path = directory) 
           ## character vector with names of all files in directory
        c_means <- vector(mode = "numeric", length = 0)
           ## empty vector of length 0, that is filled in the for loop
        
        for (i in id) {
                if (!file.exists(file.path(directory, filename_list[i]))) {
                        return(c("File does not exist.", directory, i))
                } ## returns error in case file path is wrong
                
                x <- read.csv(file.path(directory, filename_list[i]))
                  ## data frame from one file
                
                y <- x[ , pollutant][!is.na(x[ , pollutant])]
                  ## subsets one column and removes NAs
                
                c_means <- c(c_means, y)
                  ## concatenates columns into the same vector
        }
        mean(c_means)  
}