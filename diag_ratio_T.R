#Tredoux diagnosticity ratio
diag_ratio_T <- function(vec1, vec2, x, t){
    a <- (sum(vec1 == x)+0.5)/(t+1.5)
    b <-(sum(vec2 == x)+0.5)/(t+2.5)
    c <- a/b
}