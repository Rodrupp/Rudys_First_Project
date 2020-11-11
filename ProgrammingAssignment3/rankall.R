rankall <- function(outcome, num = "best") {
        ## Loading the data from 2 possible folders, error message on failure.
        if(file.exists("outcome-of-care-measures.csv")) {
                data <- read.csv("outcome-of-care-measures.csv", 
                                 colClasses = "character")
        } else if(file.exists("ProgrammingAssignment3/outcome-of-care-measures.csv")) {
                data <- read.csv("ProgrammingAssignment3/outcome-of-care-measures.csv", 
                                 colClasses = "character")
        } else stop("cannot find file")
        
        ## character vectors to allow for varied capitalisation
        ha <- c("Heart Attack", "heart attack", "Heart attack", "heart Attack")
        hf <- c("Heart Failure", "heart failure", "Heart failure", "heart Failure")
        pn <- c("Pneumonia", "pneumonia")
        
        if(outcome %in% ha) { outcome <- "heart attack"
        } else if(outcome %in% hf) { outcome <- "heart failure"
        } else if(outcome %in% pn) { outcome <- "pneumonia"
        } else stop("invalid outcome")
        
        ## Selecting relevant columns and renaming them
        oc <- data[ , c(2,7,11,17,23)]
        colnames(oc) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
        
        ## splitting the data using state column
        by_state <- split(oc, oc$state)
        
        ## creating empty vectors to be filled in loop
        hospital <- character()
        state <- character()
        
        ## to allow for num = "best"
        if(num == "best") num <- 1
        
        for (i in 1:length(by_state)) {
                
                ## selecting one state at a time, removing NAs and
                ## setting the relevant column to numeric for ordering.
                ## then ordering the data frame by outcome and hospital name.
                x <- by_state[[i]]
                x <- x[!(x[[outcome]] == "Not Available"), ]
                x[ , outcome] <- as.numeric(x[ , outcome])
                x <- x[order(x[ ,outcome], x$hospital), ]
                
                ## to allow for num = "worst"
                if(num == "worst") y <- nrow(x) else y <- num
                
                # filling in values for vectors that were set before
                hospital[i] <- x[y, "hospital"]
                state[i] <- x[1, "state"]
        }
        
        ## building the data frame from the vectors
        data.frame(hospital,state)
        
}