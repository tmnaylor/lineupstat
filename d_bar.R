#Function for computing pooled estimator from a set of k diagnosticity ratios
#df must be a dataframe combining vectors of requisite paramater estimates (created in prev steps)

d_bar <- function(df){
    numerator <- sum(df$wi*df$lnd)
    denominator <- sum(df$wi)
    d_bar <- exp(numerator/denominator)
    return(d_bar)
}