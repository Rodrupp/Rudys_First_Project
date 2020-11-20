cards <- c("Ace", as.character(2:10), "Jack", "Queen", "King")
spades <- paste(cards, rep("of Spades", 13))
hearts <- paste(cards, rep("of Hearts", 13))
clubs <- paste(cards, rep("of Clubs", 13))
diamonds <- paste(cards, rep("of Diamonds", 13))
deck <- c(spades, hearts, clubs, diamonds, "Joker", "Joker")
decknj <- c(spades, hearts, clubs, diamonds)

setup <- function(n = 2, names = NULL) {
        if(!(length(names) == n | is.null(names))) {
                stop("number of names must equal number of players.")
        }
        if(is.null(names)) {
                names <- character()
                for(i in 1:n) {
                        x <- paste("player", i, sep = "")
                        names[i] <- x
                }
        }
        hand <<- list()
        for (i in 1:n) {
                hand[i] <<- as.character(NA)
        }
        names(hand) <<- names
        
        drawpile <<- deck
}


shuffle <- function() {
        drawpile <<- sample(drawpile)
}

pickup <- function(player, n = 1) {
        for (i in 1:n) {
                x <- c(hand[[player]], drawpile[length(drawpile)])
                hand[[player]] <<- x[!is.na(x)]
                length(drawpile) <<- length(drawpile) - 1
        }
}


deal <- function(n = 7, p = length(hand), NoJoker = FALSE, TwoDecks = FALSE, 
                 shuffle = TRUE) {
        if(NoJoker == TRUE) {
                deck <- deck[1:52]
        }
        if(TwoDecks == TRUE) {
                deck <- c(deck, deck)
        }
        if (shuffle == TRUE) {
                drawpile <<- sample(deck)
        }
        drawpile <<- drawpile
        
        for (j in 1:n) {
               for (i in 1:p) {
                        pickup(names(hand)[i], 1)
                }
        }
}

reveal <- function(n) {
        drawpile[length(drawpile):(length(drawpile)-(n-1))]
}

## discard
## discard pile
## table list
