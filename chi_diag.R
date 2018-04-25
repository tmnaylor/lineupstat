#'Chi-squared estimate of homogeneity of diagnosticity ratio
#'
#'Function for getting chi-squared value for homogeneity of diagnosticity ratios
#'@param df A dataframe containing: ln(d), variance of ln(d), d weights
#'@details: To compute df, use the diag_param helper function
#'          This function calls the d_bar function 
#'@references Tredoux, C. G. (1998). Statistical inference on measures of lineup 
#'            fairness. Law and Human Behavior, 22(2), 217-237.
#'@examples
#'ratio <- ln_diag_ratio(linedf)  
#'var <- var_lnd(linedf)
#'wi <- d_weights(linedf)
#'df <- cbind(ratio, var, wi)
#'chi_diag(df)
#'
#'@references Malpass, R. S. (1981). Effective size and defendant bias in
#'            eyewitness identification lineups. Law and Human Behavior, 5(4), 299-309.
#'
#'            Malpass, R. S., Tredoux, C., & McQuiston-Surrett, D. (2007). Lineup
#'            construction and lineup fairness. In R. Lindsay, D. F. Ross, J. D. Read,
#'            & M. P. Toglia (Eds.), Handbook of Eyewitness Psychology, Vol. 2: Memory for
#'            people (pp. 155-178). Mahwah, NJ: Lawrence Erlbaum Associates.
#'
#'            Tredoux, C. G. (1998). Statistical inference on measures of lineup fairness.
#'            Law and Human Behavior, 22(2), 217-237.
#'
#'            Tredoux, C. (1999). Statistical considerations when determining measures of
#'            lineup size and lineup bias. Applied Cognitive Psychology, 13, S9-S26.
#'
#'            Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'            empirically assessing the fairness of a lineup. Law and Human Behavior,
#'            3(4), 285-293.

chi_diag <- function(df){
    q <- sum(((df$lnd-log(d_bar(df)))^2)/(df$var))
    return(q)
}