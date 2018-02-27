#Variance of diagnosticity ratio
var_diag_ratio <- function(vec1, vec2, x, t){
    a <- sum(vec1!= x)
    b <- (sum(vec1 == x))*(t+1)
    c <- sum(vec2 != x)
    d <- (sum(vec2 == x))*(t+2)
    e <- (a/b)+(c/d)
}