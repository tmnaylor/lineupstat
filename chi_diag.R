#Function for getting chi-squared value for homogeneity of diagnosticity ratios

chi_diag <- function(df, d_bar){
    q <- sum(((df$lnd-log(d_bar))^2)/(df$var))
    return(q)
}