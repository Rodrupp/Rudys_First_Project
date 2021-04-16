makecubearray <- function(length = 2, dimen = 3, dimnames = NULL, func = prod, ...) {
x <- rep(length, dimen)
array.l <- array(NA, dim = x, dimnames = dimnames)
for (a in 1:length) {
        if(dimen == 1){
                array.l[a] <- func(c(a), ...)   
        } else {
for (b in 1:length) {
        if(dimen == 2){
                array.l[a,b] <- func(c(a,b), ...)   
        } else {
for (c in 1:length) {
        if(dimen == 3){
                array.l[a,b,c] <- func(c(a,c,b), ...)
        } else {
for (d in 1:length) {
        if(dimen == 4) {
                array.l[a,b,c,d] <- func(c(a,c,b,d), ...)
        } else {
for (e in 1:length) {
        if(dimen == 5) {
                array.l[a,b,c,d,e] <- func(c(a,c,b,d,e), ...)
        } else {
for (f in 1:length) {
        if(dimen == 6) {
                array.l[a,b,c,d,e,f] <- func(c(a,c,b,d,e,f), ...)
        } else {
        return("dimen must be between 1 and 6")        
                
        }}}}}}}}}}}}
array.l
}