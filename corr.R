## corr2 was my first working function and produces correct results, but it is
## slower because it calls the 'complete()' function, which reads the data,
## and the reads the data again in the 'for()' loop.

corr2 <- function(directory, threshold = 0) {
        filename_list <- list.files(path = directory) 
        
        n_comp_cases <- complete(directory)
        ## data frame of monitor id and number of complete cases

        above <- n_comp_cases$nobs > threshold
        ## logical vector for cases above the threshold

        new_id <- n_comp_cases[above, "id"]
        ## subset of data frame with number of complete cases above threshold
        
        correlations <- vector(mode = "numeric", length = 0L)
        ## empty vector
        
        for (i in 1:length(new_id)) {
                if (length(new_id) <= 0) return(correlations)
                ## leaves the loop in case the threshold is too high
                
                x <- read.csv(file.path(directory, filename_list[new_id[i]]))
                ## reads the file and saves as data frame
                sulfate <- x[complete.cases(x), "sulfate"] ## sulfate column
                nitrate <- x[complete.cases(x), "nitrate"] ## nitrate column
                correlations <- c(correlations, cor(sulfate, nitrate))
                ## calculates correlation and concatenates it into the vector
        }
        correlations
}

## 'corr()' was my second attempt after finding a comment on the forums. It 
## doesn't read the data twice, and tests for the number of complete cases
## inside the 'for' loop. However, this produces NAs in the 'correlations' 
## vector in files that contain no data. Therefore the final output removes NAs
## from the vector

corr <- function(directory, threshold = 0) {
        filename_list <- list.files(path = directory) 
        
        correlations <- vector(mode = "numeric", length = 0L)
        ## empty vector
        
        for (i in 1:length(filename_list)) {
                x <- read.csv(file.path(directory, filename_list[i]))
                ## reads the file and saves as data frame
                
                if (sum(complete.cases(x)) < threshold) next
                ## skips loop if number of complete cases is below threshold
                
                sulfate <- x[complete.cases(x), "sulfate"] ## sulfate column
                nitrate <- x[complete.cases(x), "nitrate"] ## nitrate column
                correlations <- c(correlations, cor(sulfate, nitrate))
                ## calculates correlation and concatenates it into the vector
        }
        correlations[!is.na(correlations)]
}