best <- function(state, outcome) {
        ## Loading the data from 2 possible folders, error message on failure.
        if(file.exists("outcome-of-care-measures.csv")) {
                data <- read.csv("outcome-of-care-measures.csv", 
                                 colClasses = "character")
        } else if(file.exists("ProgrammingAssignment3/outcome-of-care-measures.csv")) {
                data <- read.csv("ProgrammingAssignment3/outcome-of-care-measures.csv", 
                                 colClasses = "character")
        } else stop("cannot find file")
        
        ## Testing if a valid value is given for `state`
        if(!(state %in% data[ ,7])) stop("invalid state")
        
        ## logical vector to select the chosen state from the dataframe
        state_logical <- data[ ,7] == state
        
        ## character vectors to allow for varied capitalisation
        ha <- c("Heart Attack", "heart attack", "Heart attack", "heart Attack")
        hf <- c("Heart Failure", "heart failure", "Heart failure", "heart Failure")
        pn <- c("Pneumonia", "pneumonia")
        
        ## selecting the column form the data frame, appropriate to the chosen
        ## outcome. Returns an error if outcome choice is invalid.
        if(outcome %in% ha) oc <- data[state_logical, c(2, 11)]
        else if(outcome %in% hf) oc <- data[state_logical, c(2, 17)]
        else if(outcome %in% pn) oc <- data[state_logical, c(2, 23)]
        else stop("invalid outcome")
        
        ## removing NAs
        NA_logical <- oc[ , 2] == "Not Available"
        oc <- oc[!NA_logical, ]
        
        ## This finds the row number of the hospitals with the minimum value
        oc[ , 2] <- as.numeric(oc[ , 2])
        min_rows <- grep(min(oc[ , 2]), oc[ , 2])
        
        ## hospital names with miminum value
        hospitals <- oc[min_rows, 1]
        
        ## orders the hospitals alphabetically and returns the first element.
        hospitals[order(hospitals)][1]
}