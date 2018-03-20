# Weight for diagnosticity ratio (inverse of variance)
# What are x and t? 
# Presumably vec1 and vec2 are ?
# Is this an old function?  Looks like it -- yes
#OLD FX. IGNORE
ratio_weight <- function(vec1, vec2, x, t){
    a <- sum(vec1==x)*sum(vec2==x)
    b <- sum(t+1)*sum(t+2)
    c <- sum(vec1==x)*sum(vec2 !=x)*sum(t+1)
    d <- sum(vec2==x)*sum(vec1 !=x)*sum(t+2)
    e <- (a*b)/(c+d)
}