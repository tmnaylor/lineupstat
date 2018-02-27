#Wells diagnosticity ratio
diag_ratio_W <- function(vec1, vec2, x, t){
    a <- sum(vec1 == x)/(t+1)
    b <-sum(vec2 == x)/(t+2)
    c <- a/b
}