cards <- c("Ace", as.character(2:10), "Jack", "Queen", "King")
spades <- paste(cards, rep("of Spades", 13))
hearts <- paste(cards, rep("of Hearts", 13))
clubs <- paste(cards, rep("of Clubs", 13))
diamonds <- paste(cards, rep("of Diamonds", 13))
deck <- c(spades, hearts, clubs, diamonds, "Joker", "Joker")
decknj <- c(spades, hearts, clubs, diamonds)

reset <- function() {
        player1 <<- character(length = 0)
        player2 <<- character(length = 0)
        player3 <<- character(length = 0)
        player4 <<- character(length = 0)
        player5 <<- character(length = 0)
        player6 <<- character(length = 0)
        drawpile <<- deck
}

shuffle <- function() {
        drawpile <<- sample(drawpile)
}

p1pickup <- function(n = 1) {
        for (i in 1:n) {
        player1 <<- c(player1, drawpile[length(drawpile)])
        length(drawpile) <<- length(drawpile) - 1
        }
}

p2pickup <- function(n = 1) {
        for (i in 1:n) {
                player2 <<- c(player2, drawpile[length(drawpile)])
                length(drawpile) <<- length(drawpile) - 1
        }
}

p3pickup <- function(n = 1) {
        for (i in 1:n) {
                player3 <<- c(player3, drawpile[length(drawpile)])
                length(drawpile) <<- length(drawpile) - 1
        }
}

p4pickup <- function(n = 1) {
        for (i in 1:n) {
                player4 <<- c(player4, drawpile[length(drawpile)])
                length(drawpile) <<- length(drawpile) - 1
        }
}

p5pickup <- function(n = 1) {
        for (i in 1:n) {
                player5 <<- c(player5, drawpile[length(drawpile)])
                length(drawpile) <<- length(drawpile) - 1
        }
}

p6pickup <- function(n = 1) {
        for (i in 1:n) {
                player1 <<- c(player6, drawpile[length(drawpile)])
                length(drawpile) <<- length(drawpile) - 1
        }
}

deal <- function(n = 7, p = 2, NoJoker = FALSE, TwoDecks = FALSE, shuffle = TRUE) {
        if(p > 6) return("Maximum Number of players is 6")
        if(NoJoker == TRUE) {
                deck <- deck[1:52]
        }
        if(TwoDecks == TRUE) {
                deck <- c(deck, deck)
        }
        if (shuffle == TRUE) {
                drawpile <- sample(deck)
        }
        for (i in 1:n) {
                p1pickup()
                if(p > 1) p2pickup()
                if(p > 2) p3pickup()
                if(p > 3) p4pickup()
                if(p > 4) p5pickup()
                if(p > 5) p6pickup()
        }
}

reveal <- function(n) {
        pile[length(pile):(length(pile)-(x-1))]
}

