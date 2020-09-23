complete <- function(directory, id = 1:332) {
        filename_list <- list.files(path = directory) 
        ## character vector of all files in directory
        
        comp_df <- data.frame()  ## empty data frame
        
        for (i in id) {
                x <- read.csv(file.path(directory, filename_list[i]))
                y <- sum(as.numeric((complete.cases(x))))
                comp_df <- rbind(comp_df, c(i, y))
                ## loop reads each file whose id is given, creates a logical
                ## vector with the complete cases, then counts the number of
                ## complete cases. It then binds the id number and the number
                ## of complete cases to the data frame as a new row.
        }
        names(comp_df)[1] <- "id"
        names(comp_df)[2] <- "nobs"  ## renaming the columns
        comp_df
}