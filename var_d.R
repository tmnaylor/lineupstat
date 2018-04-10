# Variance of diagnosticity ratio
# Let's either define what x and t are explicitly or
# rename them so that their names are easier to grasp
# e.g. suspect_pos_tp and suspect_pos_ta
var_diag_ratio <- function(vec1, vec2, x, t){
    a <- sum(vec1  != x)
    b <- (sum(vec1 == x))*(t+1)
    c <- sum(vec2  != x)
    d <- (sum(vec2 == x))*(t+2)
    e <- (a/b)+(c/d)
}