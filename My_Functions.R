add2 <- function(x, y) {
	x + y
}

above10 <- function(x) {
	use <- x > 10
	x[use]
}

above <- function(x, n = 10) { ## default value is 10
	use <- x > n
	x[use]
}

## the next function will take a matrix or data frame and calculate 
## the mean of each column.

columnmean <- function(y, removeNA = TRUE) {
	nc <- ncol(y) 
	means <- numeric(nc) 
	## creates an empty vector with length equal to 
	## the number of columns in the matrix
	
	for(i in 1:nc) {
		means[i] <- mean(y[ ,i], na.rm = removeNA)
		## replaces elements of the vector with the mean of each column
		## na.rm will remove NA values before taking the mean, 
		## if removeNA is set to TRUE.
	}
	means  ## prints out the vector
}

print("Code read")