# Tredoux diagnosticity ratio
# add comment
# What are x and t?  What are the vecs of?  Raw lineup choices?
diag_ratio_T <- function(vec1, vec2, x, t){
    a <- (sum(vec1 == x) + 0.5)/(t + 0.5)
    b <- (sum(vec2 == x) + 0.5)/(t + 0.5)
    c <- a/b
} 
