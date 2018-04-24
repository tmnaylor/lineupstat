#'Chi-squared estimate of homogeneity of diagnosticity ratio
#'
#'Function for getting chi-squared value for homogeneity of diagnosticity ratios
#'@param df A dataframe containing: ln(d), variance of ln(d), d weights
#'@details: To compute df, use the diag_param helper function
#'          This function requires the d_bar function 
#'@references Tredoux, C. G. (1998). Statistical inference on measures of lineup 
#'            fairness. Law and Human Behavior, 22(2), 217-237.
#'@examples
#'ratio <- ln_diag_ratio(linedf)  
#'var <- var_lnd(linedf)
#'wi <- d_weights(linedf)
#'df <- cbind(ratio, var, wi)
#'chi_diag(df)

chi_diag <- function(df){
    q <- sum(((df$lnd-log(d_bar(df)))^2)/(df$var))
    return(q)
}