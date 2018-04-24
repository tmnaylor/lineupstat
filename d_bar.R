#'Mean diagnosticity ratio for k lineup pairs
#'
#'Function for computing pooled estimator from a set of k diagnosticity ratios
#'
#'@param df A dataframe containing: ln(d), variance of ln(d), d weights
#'@examples
#'ratio <- ln_diag_ratio(linedf)  
#'var <- var_lnd(linedf)
#'wi <- d_weights(linedf)
#'df <- cbind(ratio, var, wi)
#'d_bar(df)

d_bar <- function(df){
    numerator   <- sum(df$wi*df$lnd)
    denominator <- sum(df$wi)
    d_bar       <- exp(numerator/denominator)
    return(d_bar)
}